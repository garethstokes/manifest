{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Manifest.Migrate
  ( ManagedTable(..)
  , managed
  , renderCreateTable
  , renderAddColumn
  ) where

import Data.ByteString (ByteString)
import Data.Proxy (Proxy)
import qualified Data.ByteString.Char8 as BC
import Manifest.Core.Meta (ColumnMeta(..), TableMeta(..), sqlTypeDDL)
import Manifest.Entity (Entity, tableMeta)

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
