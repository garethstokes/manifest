{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Manifest.Migrate
  ( ManagedTable(..)
  , managed
  , renderCreateTable
  , renderAddColumn
  , liveColumns
  , tableExists
  , TableDiff(..)
  , diffTable
  ) where

import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy)
import qualified Data.ByteString.Char8 as BC
import Manifest.Core.Codec (SqlParam)
import Manifest.Core.Meta (ColumnMeta(..), TableMeta(..), sqlTypeDDL, sqlTypeLive)
import Manifest.Entity (Entity, tableMeta)
import Manifest.Session (Db, execDb)

-- | A table the migration engine manages: its name + its columns (with SQL types).
data ManagedTable = ManagedTable
  { mtName    :: ByteString
  , mtColumns :: [ColumnMeta]
  } deriving (Eq, Show)

-- | Reflect an entity's managed schema. @managed (Proxy @User)@.
managed :: forall a. Entity a => Proxy a -> ManagedTable
managed _ = let tm = tableMeta @a in ManagedTable (tmTable tm) (tmColumns tm)

-- | One column's DDL fragment: @name TYPE [NOT NULL]@. A serial PK column is
-- @name BIGSERIAL PRIMARY KEY@; a non-serial PK gets @PRIMARY KEY@ too.
columnDDL :: ColumnMeta -> ByteString
columnDDL c =
  cmName c <> " " <> sqlTypeDDL (cmSqlType c)
    <> (if cmIsPK c then " PRIMARY KEY" else if cmNullable c then "" else " NOT NULL")

-- | @CREATE TABLE name (col1 …, col2 …, …)@ from the managed schema.
renderCreateTable :: ManagedTable -> ByteString
renderCreateTable (ManagedTable name cols) =
  "CREATE TABLE " <> name <> " (" <> BC.intercalate ", " (map columnDDL cols) <> ")"

-- | @ALTER TABLE name ADD COLUMN col …@ (additive). Added columns are never PK.
renderAddColumn :: ByteString -> ColumnMeta -> ByteString
renderAddColumn table c =
  "ALTER TABLE " <> table <> " ADD COLUMN " <> cmName c <> " " <> sqlTypeDDL (cmSqlType c)
    <> (if cmNullable c then "" else " NOT NULL")

-- | A live column as Postgres reports it: (name, data_type, is_nullable).
liveColumns :: ByteString -> Db [(ByteString, ByteString, Bool)]
liveColumns table = do
  rows <- execDb
    "SELECT column_name, data_type, (is_nullable = 'YES') \
    \FROM information_schema.columns \
    \WHERE table_schema = 'public' AND table_name = $1 \
    \ORDER BY ordinal_position"
    [Just table]
  pure (mapMaybe parse rows)
  where
    parse :: [SqlParam] -> Maybe (ByteString, ByteString, Bool)
    parse [Just n, Just t, Just b] = Just (n, t, b == "t")
    parse _ = Nothing

-- | True when the table has at least one column in the @public@ schema.
tableExists :: ByteString -> Db Bool
tableExists table = not . null <$> liveColumns table

-- | The diff between a managed table and the live DB.
data TableDiff
  = CreateTable ManagedTable                     -- table absent → CREATE
  | AlterTable ByteString [ColumnMeta] [String]  -- missing columns to ADD; destructive issues (review only)
  | UpToDate
  deriving (Eq, Show)

diffTable :: ManagedTable -> Db TableDiff
diffTable mt@(ManagedTable name cols) = do
  exists <- tableExists name
  if not exists
    then pure (CreateTable mt)
    else do
      live <- liveColumns name
      let liveNames = [ n | (n, _, _) <- live ]
          missing   = [ c | c <- cols, cmName c `notElem` liveNames ]
          -- destructive: a column present in BOTH but with a different SQL type.
          destructive =
            [ "column " <> BC.unpack (cmName c) <> " type mismatch: record "
                <> BC.unpack (sqlTypeLive (cmSqlType c)) <> " vs live " <> BC.unpack lt
            | c <- cols
            , (n, lt, _) <- live, n == cmName c
            , sqlTypeLive (cmSqlType c) /= lt
            ]
      pure $ if null missing && null destructive then UpToDate else AlterTable name missing destructive
