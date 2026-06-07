module Manifest.Core.Sql
  ( renderConds
  , renderSelect
  , renderInsert
  , renderUpdate
  , renderDelete
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Manifest.Core.Meta (ColumnMeta(..), TableMeta(..))
import Manifest.Core.Query (Cond(..), Op(..))

bcIntercalate :: ByteString -> [ByteString] -> ByteString
bcIntercalate sep = BC.intercalate sep

placeholder :: Int -> ByteString
placeholder n = BC.pack ('$' : show n)

renderOp :: Op -> ByteString
renderOp OpEq  = "="
renderOp OpNeq = "<>"
renderOp OpGt  = ">"
renderOp OpLt  = "<"

-- | Render a WHERE clause (ANDed) starting at placeholder index @start@.
-- Returns the clause text (empty if no conditions) and the next free index.
renderConds :: Int -> [Cond a] -> (ByteString, Int)
renderConds start [] = ("", start)
renderConds start conds =
  let go i (Cond col op _) = (col <> " " <> renderOp op <> " " <> placeholder i, i + 1)
      step (acc, i) c = let (txt, i') = go i c in (acc ++ [txt], i')
      (clauses, next) = foldl step ([], start) conds
  in (" WHERE " <> bcIntercalate " AND " clauses, next)

-- | @SELECT c1, c2, ... FROM t [WHERE ...]@
renderSelect :: TableMeta a -> [Cond a] -> ByteString
renderSelect tm conds =
  let cols = bcIntercalate ", " (map cmName (tmColumns tm))
      (whereTxt, _) = renderConds 1 conds
  in "SELECT " <> cols <> " FROM " <> tmTable tm <> whereTxt

-- | @INSERT INTO t (cols) VALUES ($1, ...) RETURNING all_cols@
renderInsert :: TableMeta a -> [ColumnMeta] -> ByteString
renderInsert tm insCols =
  let names  = map cmName insCols
      vals   = [ placeholder i | i <- [1 .. length insCols] ]
      ret    = bcIntercalate ", " (map cmName (tmColumns tm))
  in "INSERT INTO " <> tmTable tm
       <> " (" <> bcIntercalate ", " names <> ")"
       <> " VALUES (" <> bcIntercalate ", " vals <> ")"
       <> " RETURNING " <> ret

-- | @UPDATE t SET c1 = $1, ... WHERE pk = $n@
renderUpdate :: TableMeta a -> [ByteString] -> ByteString -> ByteString
renderUpdate tm setCols pkCol =
  let sets = [ c <> " = " <> placeholder i | (c, i) <- zip setCols [1 ..] ]
      pkPh = placeholder (length setCols + 1)
  in "UPDATE " <> tmTable tm
       <> " SET " <> bcIntercalate ", " sets
       <> " WHERE " <> pkCol <> " = " <> pkPh

-- | @DELETE FROM t WHERE pk = $1@
renderDelete :: TableMeta a -> ByteString -> ByteString
renderDelete tm pkCol =
  "DELETE FROM " <> tmTable tm <> " WHERE " <> pkCol <> " = " <> placeholder 1
