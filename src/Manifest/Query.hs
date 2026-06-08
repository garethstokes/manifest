{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Manifest.Query
  ( Query
  , TableElem
  , from
  , where_
  , renderQuery
  , runQuery
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeError (ErrorMessage (..), Unsatisfiable)
import Type.Reflection (SomeTypeRep, someTypeRep)
import Manifest.Core.Codec (RowDecoder, SqlParam, decodeRow)
import Manifest.Core.Meta (ColumnMeta (..), TableMeta (..))
import Manifest.Core.Query (Cond (..), Op (..))
import Manifest.Core.Sql (bcIntercalate)
import Manifest.Entity (Entity (..))
import Manifest.Error (DbError (..), DbException (..))
import Manifest.Session (Db, execDb)

data Query (ts :: [Type]) r = Query
  { qFrom    :: ByteString
  , qAliases :: [(SomeTypeRep, ByteString)]
  , qSelect  :: ByteString
  , qWhere   :: [QCond]
  , qOrder   :: [ByteString]
  , qGroup   :: [ByteString]
  , qLimit   :: Maybe Int
  , qOffset  :: Maybe Int
  , qDecode  :: RowDecoder r
  }

data QCond = QCond ByteString Op SqlParam

type family TableElem (a :: Type) (ts :: [Type]) :: Constraint where
  TableElem a (a ': _)  = ()
  TableElem a (_ ': ts) = TableElem a ts
  TableElem a '[] =
    Unsatisfiable ('Text "Query: table " ':<>: 'ShowType a
              ':<>: 'Text " is not in scope for this query.")

-- | Type improvement helper for 'where_' (and later condition combinators). A
-- bare label like @#userName@ elaborates to @Column a t@ for an unconstrained
-- @a@ (labels carry no entity), so @[Cond a]@ alone leaves @a@ ambiguous. For a
-- single-table query @'[t]@ there is only one sensible choice, so @DefTable@
-- pins @a ~ t@ and inference succeeds with no annotation. For a multi-table
-- query @a@ is left as supplied — pick it with @\@T@ (the 'TableElem' guard then
-- rejects any @T@ not in scope).
type family DefTable (a :: Type) (ts :: [Type]) :: Type where
  DefTable a '[t] = t
  DefTable a _    = a

qualifiedCols :: ByteString -> TableMeta a -> [ByteString]
qualifiedCols alias tm = [ alias <> "." <> cmName c | c <- tmColumns tm ]

from :: forall a. Entity a => Query '[a] a
from =
  let tm    = tableMeta @a
      alias = "t0"
      tref  = someTypeRep (Proxy @a)
  in Query
       { qFrom    = tmTable tm <> " AS " <> alias
       , qAliases = [(tref, alias)]
       , qSelect  = bcIntercalate ", " (qualifiedCols alias tm)
       , qWhere   = []
       , qOrder   = []
       , qGroup   = []
       , qLimit   = Nothing
       , qOffset  = Nothing
       , qDecode  = rowDecoder @a
       }

-- | Look up an in-scope table's alias. Total when @TableElem a ts@ holds (from/
-- innerJoin add every in-scope table to qAliases).
aliasOf :: forall a ts r. Entity a => Query ts r -> ByteString
aliasOf q =
  case lookup (someTypeRep (Proxy @a)) (qAliases q) of
    Just al -> al
    Nothing -> error "Manifest.Query: alias missing (internal invariant violated)"

-- | Add ANDed conditions on table @a@ (must be in scope). Each condition's column
-- is qualified with @a@'s alias.
where_ :: forall a ts r. (Entity a, TableElem a ts, a ~ DefTable a ts) => [Cond a] -> Query ts r -> Query ts r
where_ conds q =
  let al = aliasOf @a q
      qcs = [ QCond (al <> "." <> col) op p | Cond col op p <- conds ]
  in q { qWhere = qWhere q ++ qcs }

renderOp :: Op -> ByteString
renderOp OpEq = "="
renderOp OpNeq = "<>"
renderOp OpGt = ">"
renderOp OpLt = "<"

renderQuery :: Query ts r -> (ByteString, [SqlParam])
renderQuery q =
  let (whereTxt, params) = renderWhere (qWhere q)
      groupTxt = if null (qGroup q) then "" else " GROUP BY " <> bcIntercalate ", " (qGroup q)
      orderTxt = if null (qOrder q) then "" else " ORDER BY " <> bcIntercalate ", " (qOrder q)
      limTxt = maybe "" (\n -> " LIMIT "  <> BC.pack (show n)) (qLimit q)
      offTxt = maybe "" (\n -> " OFFSET " <> BC.pack (show n)) (qOffset q)
      sql = "SELECT " <> qSelect q <> " FROM " <> qFrom q
              <> whereTxt <> groupTxt <> orderTxt <> limTxt <> offTxt
  in (sql, params)

renderWhere :: [QCond] -> (ByteString, [SqlParam])
renderWhere [] = ("", [])
renderWhere cs =
  let clause (QCond col op _, i) = col <> " " <> renderOp op <> " $" <> BC.pack (show i)
      txt = bcIntercalate " AND " (map clause (zip cs [1 :: Int ..]))
  in (" WHERE " <> txt, [ p | QCond _ _ p <- cs ])

-- | Decode one row, throwing DbException on failure (same mechanism as
-- Session.decodeRowDb). Reused by runQuery (and later tasks).
decodeRowAs :: RowDecoder x -> [SqlParam] -> Db x
decodeRowAs dec row = either (liftIO . throwIO . DbException . DecodeFailure) pure (decodeRow dec row)

runQuery :: Query ts r -> Db [r]
runQuery q = do
  let (sql, params) = renderQuery q
  rows <- execDb sql params
  mapM (decodeRowAs (qDecode q)) rows
