{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Manifest.Session
  ( Db
  , unDb
  , Session(..)
  , SessionConfig(..)
  , defaultConfig
  , PendingOp(..)
  , IdentityMap
  , withSession
  , execDb
  , statementLog
  , setBaseline
  , lookupBaseline
  , get
  , selectWhere
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (throwIO)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Type.Reflection (SomeTypeRep)
import Manifest.Core.Codec (SqlParam, ToField(..), decodeRow)
import Manifest.Core.Meta (pkColumn, cmName)
import Manifest.Core.Query (Cond(..), Op(..))
import Manifest.Core.Sql (renderSelect)
import Manifest.Entity
import Manifest.Error (DbError(..), DbException(..))
import Manifest.Postgres (Connection, Pool, execText, withConnection)

-- | (TypeRep, encoded-PK) → baseline encoded-column vector.
type IdentityMap = Map (SomeTypeRep, SqlParam) [SqlParam]

-- | A deferred write awaiting flush. (Adds are eager — see plan notes.)
data PendingOp where
  OpSave   :: Entity a => a -> PendingOp
  OpDelete :: Entity a => a -> PendingOp

data SessionConfig = SessionConfig
  { cfgAutoflush :: Bool   -- ^ flush pending writes before each query (default on)
  }

defaultConfig :: SessionConfig
defaultConfig = SessionConfig { cfgAutoflush = True }

data Session = Session
  { sessConn     :: Connection
  , sessIdentity :: IORef IdentityMap
  , sessPending  :: IORef [PendingOp]
  , sessLog      :: IORef [(ByteString, [SqlParam])]
  , sessConfig   :: SessionConfig
  }

-- | The session monad: sealed @ReaderT Session IO@.
newtype Db a = Db { unDb :: ReaderT Session IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Acquire a connection, fresh per-session maps, run, release.
withSession :: Pool -> Db a -> IO a
withSession pool (Db r) =
  withConnection pool $ \conn -> do
    idMap <- newIORef Map.empty
    pend  <- newIORef []
    logr  <- newIORef []
    runReaderT r (Session conn idMap pend logr defaultConfig)

-- | Execute a data statement, appending it to the session statement log.
execDb :: ByteString -> [SqlParam] -> Db [[SqlParam]]
execDb sql params = Db $ do
  sess <- ask
  liftIO $ modifyIORef' (sessLog sess) (++ [(sql, params)])
  liftIO $ execText (sessConn sess) sql params

-- | The statements executed so far this session, in order.
statementLog :: Db [(ByteString, [SqlParam])]
statementLog = Db $ ask >>= liftIO . readIORef . sessLog

-- Identity-map helpers (used here and by flush in Task 10) --------------------

setBaseline :: forall a. Entity a => a -> Db ()
setBaseline a = Db $ do
  sess <- ask
  liftIO $ modifyIORef' (sessIdentity sess) (Map.insert (identityKey a) (rowEncode a))

lookupBaseline :: (SomeTypeRep, SqlParam) -> Db (Maybe [SqlParam])
lookupBaseline k = Db $ do
  sess <- ask
  liftIO $ Map.lookup k <$> readIORef (sessIdentity sess)

-- Read path -------------------------------------------------------------------

decodeRowDb :: forall a. Entity a => [SqlParam] -> Db a
decodeRowDb row = case decodeRow (rowDecoder @a) row of
  Right a  -> pure a
  Left err -> Db (liftIO (throwIO (DbException (DecodeFailure err))))

-- | flush hook — the real implementation is added in Task 10. Until then,
-- autoflush is a no-op (no writes can be pending yet).
autoflushHook :: Db ()
autoflushHook = pure ()

-- | Load by primary key; records a baseline snapshot for the loaded entity.
get :: forall a. (Entity a, ToField (PrimKey a)) => Key a -> Db (Maybe a)
get (Key k) = do
  autoflushHook
  let tm  = tableMeta @a
      sql = renderSelect tm [Cond (cmName (pkColumn tm)) OpEq (toField k)]
  rows <- execDb sql [toField k]
  case rows of
    []        -> pure Nothing
    (row : _) -> do
      a <- decodeRowDb @a row
      setBaseline a
      pure (Just a)

-- | Load all rows matching the (ANDed) conditions; each becomes managed.
selectWhere :: forall a. Entity a => [Cond a] -> Db [a]
selectWhere conds = do
  autoflushHook
  let tm  = tableMeta @a
      sql = renderSelect tm conds
      ps  = [ v | Cond _ _ v <- conds ]
  rows <- execDb sql ps
  mapM (\row -> do a <- decodeRowDb @a row; setBaseline a; pure a) rows
