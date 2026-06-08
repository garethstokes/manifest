# Manifest SP4b — Composable typed query builder Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A composable, typed `Query` value you assemble with `from` / `where_` / `orderBy` / `limit` / `offset` / `innerJoin` / `groupBy` / aggregate combinators and run with `runQuery`, bringing real joins, ordering, pagination, and aggregates to the Core query layer (today there is only single-table `SELECT … WHERE`).

**Architecture:** One new module `Manifest.Query`. A `Query (ts :: [Type]) r` is indexed by the type-level list of tables in scope (`ts`) and the result row type (`r`). It carries pre-rendered SQL fragments (qualified FROM/JOINs, WHERE conds, ORDER, GROUP, LIMIT/OFFSET, the SELECT list) plus a `RowDecoder r`. A `TableElem a ts` constraint (same `Unsatisfiable` pattern as the existing relation `Member`) makes `where_`/`orderBy`/`groupBy` reject columns of tables not in the query, at compile time. Tables get value-level aliases `t0`,`t1`,…; columns qualify by `TypeRep` lookup. Join results are left-nested tuples whose decoder composes the entities' `RowDecoder`s. `runQuery` reuses the session's `execDb` (which logs + runs SQL) and `decodeRow`.

**Tech Stack:** GHC 9.10.1 via zinc; existing `Manifest.Core.Query` (`Column a t`, `Cond a`, operators), `Manifest.Core.Codec` (applicative `RowDecoder`), `Manifest.Session` (`execDb`, `Db`), `Manifest.Core.Meta`/`Entity` (`tableMeta`, `rowDecoder`). Custom `test/Harness.hs` (not hspec) and `Fixtures` (`User`, `Post`, etc., against an ephemeral Postgres).

**Scope (MVP):** `from`, `where_`, `orderBy`/`asc`/`desc`, `limit`/`offset`, `innerJoin`/`on`, `runQuery`; aggregates `count` / `aggregate` (`Sum`/`Avg`/`Min`/`Max`) and a grouped path `groupBy` → `countGroups` / `aggregateGroups`. **Deliberately deferred** (documented as Planned): `LEFT`/`RIGHT`/`OUTER` joins, `HAVING`, subqueries/CTEs, `DISTINCT`, multi-column `GROUP BY`, self-joins (one alias per entity type), and session-management of builder results (results are plain decoded values, not registered in the identity map; use `get`/`selectWhere` when you need managed rows). These are real limitations and must be stated, not implied away.

---

## Design reference (read once, all tasks build on this)

The single new module is `src/Manifest/Query.hs`. The core type and helpers, established in Task 1:

```haskell
-- | A query in progress. @ts@ is the type-level list of tables in scope (for
-- compile-time column-membership checks); @r@ is the decoded result row type.
data Query (ts :: [Type]) r = Query
  { qFrom    :: ByteString                 -- "users AS t0 INNER JOIN posts AS t1 ON …"
  , qAliases :: [(SomeTypeRep, ByteString)] -- table type -> alias, in join order
  , qSelect  :: ByteString                 -- the SELECT list, qualified, in decoder order
  , qWhere   :: [QCond]                    -- qualified conditions (ANDed)
  , qOrder   :: [ByteString]               -- "t0.user_name ASC"
  , qGroup   :: [ByteString]               -- "t0.post_author"  (set by groupBy)
  , qLimit   :: Maybe Int
  , qOffset  :: Maybe Int
  , qDecode  :: RowDecoder r
  }

-- | A WHERE/ON condition whose column text is already alias-qualified.
data QCond = QCond ByteString Op SqlParam

-- | Compile-time proof that table @a@ is one of the query's tables @ts@.
type family TableElem (a :: Type) (ts :: [Type]) :: Constraint where
  TableElem a (a ': _)  = ()
  TableElem a (_ ': ts) = TableElem a ts
  TableElem a '[] =
    Unsatisfiable ('Text "Query: table " ':<>: 'ShowType a
              ':<>: 'Text " is not in scope for this query.")
```

Key invariants every task must preserve:

- **Aliases** are assigned by join order: `from` uses `t0`; the Nth `innerJoin` uses `t<N>` where `N = length qAliases`. `aliasOf @a q` looks up `someTypeRep (Proxy @a)` in `qAliases` (it is always present when `TableElem a ts` holds, because `from`/`innerJoin` add every in-scope table to `qAliases`).
- **Column qualification:** an unqualified `Column a t` (its `colName`) becomes `aliasOf @a q <> "." <> colName`. The deriver already produced `colName` as the snake_case column.
- **SELECT list / decoder order agree:** `from @a` selects all of `a`'s columns (qualified) in `tmColumns` order and decodes with `rowDecoder @a`. `innerJoin @b` appends `b`'s qualified columns and composes the decoder `(,) <$> qDecode <*> rowDecoder @b`, so the row layout matches the tuple.
- **Params** come only from `qWhere` (in order); placeholders are numbered `$1..$n` at assembly time.
- `Entity a` has `Typeable a` as a superclass, so every combinator that needs a `TypeRep` already has it from its `Entity` constraint.

Pure assembler + runner (Task 1):

```haskell
-- | Assemble the final SQL and ordered parameter list. Pure, so SQL shape is
-- unit-testable without a database.
renderQuery :: Query ts r -> (ByteString, [SqlParam])

-- | Run the query and decode each row into @r@. Reuses the session's execDb
-- (logs + runs) and decodeRow. Results are NOT registered in the identity map.
runQuery :: Query ts r -> Db [r]
```

---

### Task 1: `Query` type, `from`, `renderQuery`, `runQuery` (single table)

**Files:**
- Create: `src/Manifest/Query.hs`
- Modify: `zinc.toml` is not needed (no new deps; `Type.Reflection`/`GHC.TypeError` are in `base`). The library `source-dirs` already covers `src`.
- Create: `test/QueryBuilderSpec.hs`
- Modify: `test/Spec.hs`

- [ ] **Step 1: Write the failing test**

Create `test/QueryBuilderSpec.hs`. First two tests: the pure SQL shape of `from @User`, and a DB round-trip selecting all rows.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module QueryBuilderSpec (tests) where

import qualified Data.ByteString.Char8 as BC
import Data.List (sort)
import Fixtures (Post, PostT (..), User, UserT (..), withTestDb)
import Manifest
import Manifest.Query
import Harness

tests :: [Test]
tests = group "QueryBuilder"
  [ test "from @User renders SELECT of all columns from the aliased table" $
      assertEqual "sql"
        "SELECT t0.user_id, t0.user_name, t0.user_email FROM users AS t0"
        (fst (renderQuery (from @User)))
  , test "runQuery (from @User) returns all rows" $
      withTestDb $ \pool -> do
        names <- withSession pool $ do
          _ <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (User { userId = 0, userName = "Bob", userEmail = Nothing } :: User)
          us <- runQuery (from @User)
          pure (sort (map userName us))
        assertEqual "names" ["Ada", "Bob"] names
  ]
```

Wire into `test/Spec.hs`: add `import qualified QueryBuilderSpec` and append `QueryBuilderSpec.tests` to the `++` chain in `main`.

- [ ] **Step 2: Run to verify it fails**

Run: `nix develop -c zinc test 2>&1 | tail -20`
Expected: compile failure — `Manifest.Query` / `from` / `renderQuery` / `runQuery` not found.

- [ ] **Step 3: Implement `Manifest.Query` (core type + `from` + `renderQuery` + `runQuery`)**

Create `src/Manifest/Query.hs`:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
  , renderQuery
  , runQuery
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeError (ErrorMessage (..), Unsatisfiable)
import Type.Reflection (SomeTypeRep, someTypeRep)
import Manifest.Core.Codec (RowDecoder, SqlParam, decodeRow)
import Manifest.Core.Meta (ColumnMeta (..), TableMeta (..))
import Manifest.Core.Query (Op (..))
import Manifest.Core.Sql (bcIntercalate)
import Manifest.Entity (Entity (..))
import Manifest.Error (DbException (..), DecodeError)   -- DecodeError re-exported? if not, drop from import; see note
import Manifest.Session (Db, execDb)

-- See plan "Design reference" for the field meanings.
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

-- | Qualify an entity's columns with an alias: ["t0.user_id", "t0.user_name", …].
qualifiedCols :: ByteString -> TableMeta a -> [ByteString]
qualifiedCols alias tm = [ alias <> "." <> cmName c | c <- tmColumns tm ]

-- | @SELECT a.* FROM a AS t0@. The base of every query.
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

renderOp :: Op -> ByteString
renderOp OpEq = "="; renderOp OpNeq = "<>"; renderOp OpGt = ">"; renderOp OpLt = "<"

-- | Assemble final SQL + ordered params. Pure; used by runQuery and tests.
renderQuery :: Query ts r -> (ByteString, [SqlParam])
renderQuery q =
  let (whereTxt, params) = renderWhere (qWhere q)
      groupTxt = if null (qGroup q) then ""
                 else " GROUP BY " <> bcIntercalate ", " (qGroup q)
      orderTxt = if null (qOrder q) then ""
                 else " ORDER BY " <> bcIntercalate ", " (qOrder q)
      limTxt = maybe "" (\n -> " LIMIT "  <> BC.pack (show n)) (qLimit q)
      offTxt = maybe "" (\n -> " OFFSET " <> BC.pack (show n)) (qOffset q)
      sql = "SELECT " <> qSelect q <> " FROM " <> qFrom q
              <> whereTxt <> groupTxt <> orderTxt <> limTxt <> offTxt
  in (sql, params)

-- | Render the ANDed WHERE, numbering placeholders $1..$n.
renderWhere :: [QCond] -> (ByteString, [SqlParam])
renderWhere [] = ("", [])
renderWhere cs =
  let clause (QCond col op _, i) = col <> " " <> renderOp op <> " $" <> BC.pack (show i)
      txt = bcIntercalate " AND " (map clause (zip cs [1 :: Int ..]))
  in (" WHERE " <> txt, [ p | QCond _ _ p <- cs ])

-- | Run + decode. Reuses execDb (logs + runs). Results are not session-managed.
runQuery :: Query ts r -> Db [r]
runQuery q = do
  let (sql, params) = renderQuery q
  rows <- execDb sql params
  mapM decodeOne rows
  where
    decodeOne row = case decodeRow (qDecode q) row of
      Right x  -> pure x
      Left err -> failDecode err

failDecode :: DecodeError -> Db a
failDecode err = Db' (throwDecode err)   -- see note below
```

> **Implementer notes (resolve while compiling):**
> - `runQuery`'s decode-failure path must throw a `DbException (DecodeFailure err)`, exactly like `Manifest.Session.decodeRowDb` does. The simplest correct implementation is to **not** reimplement it: import nothing extra and instead build the decoder result handling to mirror `decodeRowDb`. Concretely, replace the `decodeOne`/`failDecode`/`Db'` sketch with: `mapM (\row -> either (liftIO . throwIO . DbException . DecodeFailure) pure (decodeRow (qDecode q) row)) rows`, importing `Control.Monad.IO.Class (liftIO)`, `Control.Exception (throwIO)`, and `Manifest.Error (DbException (..), DbError (..))` (the `DecodeFailure` constructor lives in `DbError`). Check `src/Manifest/Error.hs` for the exact constructor name (`DecodeFailure` / `DecodeError` value) and `Manifest.Session.decodeRowDb` (lines ~109-112) for the precise pattern, and copy it. `liftIO`/`throwIO` are usable inside `Db` because `Db` derives `MonadIO`.
> - Drop any import that does not resolve (e.g. if `DecodeError` is not what `decodeRow` returns by that name). The goal is: decode each row with `qDecode`, throw on `Left` the same way `decodeRowDb` throws. Compile iteratively.
> - `Op (..)` is exported from `Manifest.Core.Query`. `bcIntercalate` is exported from `Manifest.Core.Sql`.

- [ ] **Step 4: Run to verify it passes**

Run: `nix develop -c zinc test 2>&1 | tail -8` then `nix develop -c .zinc/build/spec 2>&1 | tail -2`
Expected: both new tests pass; total = previous baseline (85) + 2 = 87.

- [ ] **Step 5: `-Wall` check on the new module**

Run: `nix develop -c zinc build 2>&1 | grep -iE "warning|Query.hs" | tail -20`
Expected: no warnings referencing `Manifest/Query.hs`. Remove any unused import the compiler flags.

- [ ] **Step 6: Commit**

```bash
git add src/Manifest/Query.hs test/QueryBuilderSpec.hs test/Spec.hs
git commit -m "feat(query): composable Query builder — from/renderQuery/runQuery"
```

---

### Task 2: `where_`

**Files:**
- Modify: `src/Manifest/Query.hs`
- Modify: `test/QueryBuilderSpec.hs`

- [ ] **Step 1: Write the failing test**

Append to `QueryBuilderSpec`'s list:

```haskell
  , test "where_ qualifies the condition by the table alias" $
      assertEqual "sql"
        "SELECT t0.user_id, t0.user_name, t0.user_email FROM users AS t0 WHERE t0.user_name = $1"
        (fst (renderQuery (where_ [#userName ==. ("Bob" :: String)] (from @User))))
  , test "where_ filters rows at runtime" $
      withTestDb $ \pool -> do
        names <- withSession pool $ do
          _ <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (User { userId = 0, userName = "Bob", userEmail = Nothing } :: User)
          us <- runQuery (where_ [#userName ==. ("Bob" :: String)] (from @User))
          pure (map userName us)
        assertEqual "names" ["Bob"] names
  ]
```

(`#userName ==. ("Bob" :: String)` builds a `Cond User`; `String` has `ToField`/`FromField`.)

- [ ] **Step 2: Run to verify it fails**

Run: `nix develop -c zinc test 2>&1 | tail -8`
Expected: `where_` not in scope.

- [ ] **Step 3: Implement `where_`**

Add to the export list of `Manifest.Query`: `, where_`. Import `Cond (..)` from `Manifest.Core.Query`. Add:

```haskell
-- | Look up an in-scope table's alias. Total when @TableElem a ts@ holds.
aliasOf :: forall a ts r. Entity a => Query ts r -> ByteString
aliasOf q =
  case lookup (someTypeRep (Proxy @a)) (qAliases q) of
    Just al -> al
    Nothing -> error "Manifest.Query: alias missing (internal invariant violated)"

-- | Add ANDed conditions on table @a@ (which must be in scope). Each condition's
-- column is qualified with @a@'s alias.
where_ :: forall a ts r. (Entity a, TableElem a ts) => [Cond a] -> Query ts r -> Query ts r
where_ conds q =
  let al = aliasOf @a q
      q' = [ QCond (al <> "." <> col) op p | Cond col op p <- conds ]
  in q { qWhere = qWhere q ++ q' }
```

`Cond` is `Cond ByteString Op SqlParam` (see `Manifest.Core.Query`), so the pattern `Cond col op p` binds the unqualified column, the operator, and the encoded value.

- [ ] **Step 4: Run to verify it passes**

Run: `nix develop -c zinc test 2>&1 | tail -4` then `… .zinc/build/spec | tail -2`
Expected: green; total 89.

- [ ] **Step 5: Negative-safety smoke (manual, not committed)**

Confirm the `TableElem` guard works: temporarily add `where_ [#postTitle ==. ("x"::String)] (from @User)` somewhere and build; expect a compile error naming `Query: table Post is not in scope`. Remove it before committing. (This verifies the compile-time membership check; it is a manual check, no test artifact.)

- [ ] **Step 6: Commit**

```bash
git add src/Manifest/Query.hs test/QueryBuilderSpec.hs
git commit -m "feat(query): where_ with compile-time table-membership check"
```

---

### Task 3: `orderBy` / `asc` / `desc`, `limit`, `offset`

**Files:**
- Modify: `src/Manifest/Query.hs`
- Modify: `test/QueryBuilderSpec.hs`

- [ ] **Step 1: Write the failing test**

Append:

```haskell
  , test "orderBy + limit + offset render in order" $
      assertEqual "sql"
        "SELECT t0.user_id, t0.user_name, t0.user_email FROM users AS t0 ORDER BY t0.user_name DESC LIMIT 2 OFFSET 1"
        (fst (renderQuery (offset 1 (limit 2 (orderBy [desc #userName] (from @User))))))
  , test "orderBy + limit return rows in order, paginated" $
      withTestDb $ \pool -> do
        names <- withSession pool $ do
          mapM_ (\n -> add (User { userId = 0, userName = n, userEmail = Nothing } :: User))
                ["Ada", "Bob", "Cay", "Dee"]
          us <- runQuery (limit 2 (orderBy [asc #userName] (from @User)))
          pure (map userName us)
        assertEqual "first two by name" ["Ada", "Bob"] names
  ]
```

- [ ] **Step 2: Run to verify it fails**

Expected: `orderBy` / `asc` / `desc` / `limit` / `offset` not in scope.

- [ ] **Step 3: Implement ordering + pagination**

Add to exports: `, orderBy, asc, desc, limit, offset, OrderTerm`. Import `Column (..)` from `Manifest.Core.Query`. Add:

```haskell
-- | An ORDER BY term: a column of table @a@ and a direction.
data OrderTerm a = OrderTerm ByteString ByteString   -- (colName, "ASC"/"DESC")

asc, desc :: Column a t -> OrderTerm a
asc  (Column c) = OrderTerm c "ASC"
desc (Column c) = OrderTerm c "DESC"

-- | Order by columns of table @a@ (which must be in scope), appended in order.
orderBy :: forall a ts r. (Entity a, TableElem a ts) => [OrderTerm a] -> Query ts r -> Query ts r
orderBy terms q =
  let al = aliasOf @a q
      rendered = [ al <> "." <> c <> " " <> dir | OrderTerm c dir <- terms ]
  in q { qOrder = qOrder q ++ rendered }

limit :: Int -> Query ts r -> Query ts r
limit n q = q { qLimit = Just n }

offset :: Int -> Query ts r -> Query ts r
offset n q = q { qOffset = Just n }
```

`Column` is `newtype Column a t = Column { colName :: ByteString }` (see `Manifest.Core.Query`), so `Column c` binds the column name. `#userName :: Column User Text` supplies the `a = User` for `OrderTerm User`, which `orderBy`'s `TableElem User ts` checks.

- [ ] **Step 4: Run to verify it passes**

Expected: green; total 91.

- [ ] **Step 5: Commit**

```bash
git add src/Manifest/Query.hs test/QueryBuilderSpec.hs
git commit -m "feat(query): orderBy/asc/desc, limit, offset"
```

---

### Task 4: `innerJoin` and `on`

**Files:**
- Modify: `src/Manifest/Query.hs`
- Modify: `test/QueryBuilderSpec.hs`

- [ ] **Step 1: Write the failing test**

Append:

```haskell
  , test "innerJoin renders an aliased INNER JOIN and selects both tables" $
      assertEqual "sql"
        ( "SELECT t0.user_id, t0.user_name, t0.user_email, t1.post_id, t1.post_author, t1.post_title"
       <> " FROM users AS t0 INNER JOIN posts AS t1 ON t1.post_author = t0.user_id" )
        (fst (renderQuery (innerJoin @Post (on #postAuthor #userId) (from @User))))
  , test "innerJoin returns (User, Post) pairs at runtime" $
      withTestDb $ \pool -> do
        pairs <- withSession pool $ do
          u <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P1" } :: Post)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P2" } :: Post)
          rows <- runQuery (innerJoin @Post (on #postAuthor #userId) (from @User))
          pure [ (userName usr, postTitle pst) | (usr, pst) <- rows ]
        assertEqual "joined pairs" [("Ada","P1"), ("Ada","P2")] pairs
  ]
```

- [ ] **Step 2: Run to verify it fails**

Expected: `innerJoin` / `on` not in scope.

- [ ] **Step 3: Implement `on` and `innerJoin`**

Add to exports: `, innerJoin, on, On`. Add:

```haskell
-- | A join condition equating a column of one in-scope table to a column of
-- another. Stores both tables' TypeReps so it can be alias-qualified at join time.
data On = On SomeTypeRep ByteString SomeTypeRep ByteString

-- | @on colL colR@ builds @colL = colR@. Typically one side is the newly joined
-- table and the other an existing one.
on :: forall la lt rb rt. (Entity la, Entity rb)
   => Column la lt -> Column rb rt -> On
on (Column lc) (Column rc) =
  On (someTypeRep (Proxy @la)) lc (someTypeRep (Proxy @rb)) rc

-- | INNER JOIN table @b@ into the query, equating columns per the @On@. The
-- result row becomes @(r, b)@; @b@ is added to scope (@ts@) and to the alias map.
innerJoin
  :: forall b ts r. Entity b
  => On -> Query ts r -> Query (b ': ts) (r, b)
innerJoin (On lref lc rref rc) q =
  let tm     = tableMeta @b
      alias  = "t" <> BC.pack (show (length (qAliases q)))
      bref   = someTypeRep (Proxy @b)
      aliases' = qAliases q ++ [(bref, alias)]
      lookupAl ref = maybe (error "Manifest.Query: join references a table not in scope")
                           id (lookup ref aliases')
      onTxt  = lookupAl lref <> "." <> lc <> " = " <> lookupAl rref <> "." <> rc
  in Query
       { qFrom    = qFrom q <> " INNER JOIN " <> tmTable tm <> " AS " <> alias <> " ON " <> onTxt
       , qAliases = aliases'
       , qSelect  = qSelect q <> ", " <> bcIntercalate ", " (qualifiedCols alias tm)
       , qWhere   = qWhere q
       , qOrder   = qOrder q
       , qGroup   = qGroup q
       , qLimit   = qLimit q
       , qOffset  = qOffset q
       , qDecode  = (,) <$> qDecode q <*> rowDecoder @b
       }
```

Note the result index `(b ': ts)`: after the join, `where_`/`orderBy` accept columns of `b` as well as the original tables. The decoder `(,) <$> qDecode q <*> rowDecoder @b` matches the SELECT list (existing columns, then `b`'s), and `decodeRow` consumes them left to right.

- [ ] **Step 4: Run to verify it passes**

Expected: green; total 93. If the join SQL or pairs mismatch, inspect via `fst (renderQuery …)` and the statement log.

- [ ] **Step 5: Commit**

```bash
git add src/Manifest/Query.hs test/QueryBuilderSpec.hs
git commit -m "feat(query): innerJoin + on, result tuples via composed decoders"
```

---

### Task 5: Aggregates and `groupBy`

**Files:**
- Modify: `src/Manifest/Query.hs`
- Modify: `test/QueryBuilderSpec.hs`

Aggregates are terminal runners that reuse the query's FROM/WHERE. `count`/`aggregate` are ungrouped scalars; `groupBy` produces a `Grouped` carrying the group column, consumed by `countGroups`/`aggregateGroups` to return `[(key, …)]`.

- [ ] **Step 1: Write the failing test**

Append:

```haskell
  , test "count returns the number of matching rows" $
      withTestDb $ \pool -> do
        n <- withSession pool $ do
          mapM_ (\nm -> add (User { userId = 0, userName = nm, userEmail = Nothing } :: User))
                ["Ada", "Bob", "Cay"]
          count (where_ [#userName >. ("Ada" :: String)] (from @User))
        assertEqual "count > Ada" 2 n
  , test "aggregate Sum over a column" $
      withTestDb $ \pool -> do
        s <- withSession pool $ do
          u <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P1" } :: Post)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P2" } :: Post)
          aggregate Sum (#postAuthor) (from @Post)    -- sum of post_author over all posts
        assertEqual "sum of author ids" (Just (2 * 1)) s  -- both posts authored by user id 1
  , test "groupBy + countGroups counts children per key" $
      withTestDb $ \pool -> do
        grouped <- withSession pool $ do
          u <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P1" } :: Post)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P2" } :: Post)
          countGroups (groupBy #postAuthor (from @Post))
        assertEqual "posts per author" [(1, 2)] grouped
  ]
```

(With a single inserted user the serial PK is `1`, so `postAuthor = 1`. The fixtures use `BIGSERIAL` starting at 1 in a fresh ephemeral DB.)

- [ ] **Step 2: Run to verify it fails**

Expected: `count` / `aggregate` / `Sum` / `groupBy` / `countGroups` not in scope.

- [ ] **Step 3: Implement aggregates + grouping**

Add to exports: `, count, aggregate, Agg (..), groupBy, Grouped, countGroups, aggregateGroups`. Import `field` and `FromField` from `Manifest.Core.Codec`. Add:

```haskell
-- | SQL aggregate functions.
data Agg = Sum | Avg | Min | Max

aggFn :: Agg -> ByteString
aggFn Sum = "SUM"; aggFn Avg = "AVG"; aggFn Min = "MIN"; aggFn Max = "MAX"

-- | Render only FROM + WHERE of a query (aggregates ignore SELECT/ORDER/LIMIT).
fromWhere :: Query ts r -> (ByteString, [SqlParam])
fromWhere q =
  let (whereTxt, params) = renderWhere (qWhere q)
  in (" FROM " <> qFrom q <> whereTxt, params)

-- | @SELECT COUNT(*) FROM … [WHERE …]@ → the row count.
count :: Query ts r -> Db Int
count q = do
  let (fw, params) = fromWhere q
  rows <- execDb ("SELECT COUNT(*)" <> fw) params
  decodeScalar field rows

-- | @SELECT agg(alias.col) FROM … [WHERE …]@ → the scalar (NULL on empty set → Nothing).
aggregate :: forall a n ts r. (Entity a, TableElem a ts, FromField n)
          => Agg -> Column a n -> Query ts r -> Db (Maybe n)
aggregate ag (Column c) q = do
  let col = aliasOf @a q <> "." <> c
      (fw, params) = fromWhere q
  rows <- execDb ("SELECT " <> aggFn ag <> "(" <> col <> ")" <> fw) params
  decodeScalar field rows   -- field :: RowDecoder (Maybe n) via FromField (Maybe n)

-- | A query with a GROUP BY key column of table @a@ pinned (type @k@).
data Grouped k ts r = Grouped ByteString (Query ts r)   -- (qualified group column, query)

groupBy :: forall a k ts r. (Entity a, TableElem a ts)
        => Column a k -> Query ts r -> Grouped k ts r
groupBy q0@(Column c) qy = Grouped (aliasOf @a qy <> "." <> c) qy
  where _ = q0   -- silence unused if needed; remove if not

-- | @SELECT key, COUNT(*) FROM … [WHERE …] GROUP BY key@ → counts per key.
countGroups :: forall k ts r. FromField k => Grouped k ts r -> Db [(k, Int)]
countGroups (Grouped key q) = do
  let (fw, params) = fromWhere q
  rows <- execDb ("SELECT " <> key <> ", COUNT(*)" <> fw <> " GROUP BY " <> key) params
  mapM (decodeRowAs ((,) <$> field <*> field)) rows

-- | @SELECT key, agg(alias.col) FROM … GROUP BY key@ → aggregate per key.
aggregateGroups :: forall a n k ts r. (Entity a, TableElem a ts, FromField k, FromField n)
                => Agg -> Column a n -> Grouped k ts r -> Db [(k, Maybe n)]
aggregateGroups ag (Column c) (Grouped key q) = do
  let col = aliasOf @a q <> "." <> c
      (fw, params) = fromWhere q
  rows <- execDb ("SELECT " <> key <> ", " <> aggFn ag <> "(" <> col <> ")"
                    <> fw <> " GROUP BY " <> key) params
  mapM (decodeRowAs ((,) <$> field <*> field)) rows
```

Add two small decode helpers (mirroring `runQuery`'s decode-and-throw):

```haskell
-- | Decode the single column of the single returned row (COUNT/aggregate scalar).
decodeScalar :: RowDecoder x -> [[SqlParam]] -> Db x
decodeScalar dec rows = case rows of
  (row : _) -> decodeRowAs dec row
  []        -> -- COUNT(*) always returns a row; aggregate over empty returns one NULL row too.
               -- Defensive: treat no-row as a decode error.
               failDecodeMsg "aggregate returned no row"

-- | Decode one row with the given decoder, throwing DbException on failure
-- (same mechanism as Session.decodeRowDb).
decodeRowAs :: RowDecoder x -> [SqlParam] -> Db x
decodeRowAs dec row = either throwDb pure (decodeRow dec row)
```

> **Implementer note:** `throwDb` / `failDecodeMsg` must throw `DbException (DecodeFailure …)` exactly as `runQuery` does (Task 1). Implement them with `liftIO . throwIO . DbException . DecodeFailure` and reuse for `runQuery` too (refactor Task 1's inline version into `decodeRowAs` and call it from `runQuery` — cleaner). `field :: FromField x => RowDecoder x`; for `aggregate`, `n`'s `FromField (Maybe n)` instance (already defined for all `FromField a`) handles the NULL-on-empty case. Confirm `FromField Int` decodes Postgres `COUNT(*)` (returned as text `"2"`) — it does (`readMaybe`).

- [ ] **Step 4: Run to verify it passes**

Expected: green; total 96. If `aggregate Sum` returns a decode error, check the `Maybe n` decoder path and that `SUM` of bigint comes back as a plain integer literal.

- [ ] **Step 5: Commit**

```bash
git add src/Manifest/Query.hs test/QueryBuilderSpec.hs
git commit -m "feat(query): count, aggregate (Sum/Avg/Min/Max), groupBy + grouped runners"
```

---

### Task 6: Umbrella export, manual page, honest status updates

**Files:**
- Modify: `src/Manifest.hs`
- Create: `docs/queries.md`
- Modify: `docs/index.md`, `docs/entities.md`

- [ ] **Step 1: Re-export the query builder from the umbrella**

In `src/Manifest.hs`, add an export section after the query-DSL exports and a matching import:

Export list:
```haskell
    -- * Composable query builder
  , Query
  , from
  , where_
  , orderBy
  , asc
  , desc
  , limit
  , offset
  , innerJoin
  , on
  , runQuery
  , count
  , aggregate
  , Agg (..)
  , groupBy
  , countGroups
  , aggregateGroups
```

Import:
```haskell
import Manifest.Query
  ( Query, from, where_, orderBy, asc, desc, limit, offset
  , innerJoin, on, runQuery, count, aggregate, Agg (..)
  , groupBy, countGroups, aggregateGroups
  )
```

Build to confirm it resolves: `nix develop -c zinc build 2>&1 | tail -5` (no warnings).

- [ ] **Step 2: Write the `Queries` manual page**

Create `docs/queries.md` in the manual's factual voice (no em-dashes, no positioning claims, no SQLAlchemy — match the surrounding pages). Use `nav_order: 8` and bump the Tutorials section to `nav_order: 9` (Step 4 handles the tutorials index). Content:

````markdown
---
title: Queries
nav_order: 8
---

# Queries

The composable query builder assembles a typed `Query` value and runs it with
`runQuery`. It covers ordering, pagination, inner joins, and aggregates, beyond the
single-table `selectWhere` reads. A `Query` is indexed by the tables in scope, so a
condition or ordering on a table you have not brought into the query is a compile
error.

## Building and running

`from @User` starts a query over one table. Combinators pipe with `&`
(`Data.Function`):

```haskell
import Data.Function ((&))

us <- runQuery (from @User & where_ [#userName ==. "Bob"])
```

`runQuery :: Query ts r -> Db [r]` runs the query and decodes each row. Results are
plain decoded values; they are not registered in the session identity map (use
`get` / `selectWhere` when you need managed rows to edit and `save`).

## Ordering and pagination

`orderBy` takes `asc` / `desc` terms; `limit` and `offset` paginate:

```haskell
page <- runQuery (from @User & orderBy [asc #userName] & limit 20 & offset 40)
```

## Inner joins

`innerJoin @Post (on #postAuthor #userId)` joins a second table; the result row
becomes a tuple `(User, Post)`:

```haskell
pairs <- runQuery (from @User & innerJoin @Post (on #postAuthor #userId))
-- pairs :: [(User, Post)]
```

`on colL colR` equates the two columns. Chaining another `innerJoin` nests the
tuple further (`((User, Post), Comment)`).

## Aggregates

`count` returns the number of matching rows; `aggregate` applies `Sum` / `Avg` /
`Min` / `Max` to a column and returns `Maybe n` (`Nothing` for an empty set):

```haskell
n     <- count    (from @User & where_ [#userName >. "Ada"])
total <- aggregate Sum #postAuthor (from @Post)   -- :: Db (Maybe Int)
```

`groupBy` pins a key column; `countGroups` and `aggregateGroups` return one row per
key:

```haskell
perAuthor <- countGroups (from @Post & groupBy #postAuthor)   -- :: Db [(Int, Int)]
```

## Status

Built and tested: `from`, `where_`, `orderBy` / `asc` / `desc`, `limit` / `offset`,
`innerJoin` / `on`, `runQuery`, `count`, `aggregate`, `groupBy` with `countGroups` /
`aggregateGroups`. The compile-time table-in-scope check is enforced by the `Query`
index.

Planned, not built:

* **Outer joins, `HAVING`, `DISTINCT`, subqueries, and CTEs.** Only `INNER JOIN`
  and the aggregates above are built.
* **Multi-column `GROUP BY` and self-joins.** One alias is assigned per entity type,
  so a query joins each table at most once; grouping pins a single key column.
* **Session-managed results.** Builder results are plain values, not entries in the
  identity map. `selectWhere` and `get` return managed rows; `runQuery` does not.
````

- [ ] **Step 3: Update `index.md` status (joins/aggregates now built)**

In `docs/index.md`, the Status section lists "Joins and aggregates in the query Core" as the one **Planned** surface. That is no longer accurate for the builder. Replace that bullet with an honest statement that the composable builder is built, while keeping the distinction that the *relationship* `joined` strategy is separate:

Change the Status section so it reads (replace the single "Planned" bullet and the surrounding sentence):

```markdown
## Status

The Unit of Work, relationships, cascades, migrations, the Template Haskell entity
front-end, and the composable query builder (joins, ordering, pagination, and
aggregates) are built and tested. See the reference pages for each, and the
[Queries](queries.md) page for the builder.

The `mkEntity` macro generates the record, `deriving Generic`, and the `Entity`
instance from one block (see [Entities](entities.md)). It builds the core entity;
relationships and cascades are declared separately. Hand-writing the record and
instance is fully supported and is what the snippets above show.

The site is published by GitHub Pages' built-in Jekyll build of `docs/`; there is
no Actions workflow. The tutorials run as tests and require a Postgres (the suite
spins up an ephemeral one); a local Jekyll build is out of scope.
```

Also update the "Pages" list in `index.md` to include the new page, after the Cascades/Migrations line:

```markdown
- [Queries](queries.md): the composable query builder, ordering, pagination, inner
  joins, and aggregates.
```

- [ ] **Step 4: Update `entities.md`'s query-DSL "Planned" note**

`docs/entities.md` has a blockquote under `#label` column references that says the full query DSL "lives in Core Sub-project 4 and is **not built**." Update it to reflect that the builder now exists, keeping the distinction precise:

Replace that blockquote with:

```markdown
> The composable query builder (joins, ordering, pagination, aggregates) is built;
> see [Queries](queries.md). It uses the same `#label` column references. What is
> still Planned: outer joins, `HAVING`, subqueries, and CTEs (the
> [Queries](queries.md) status section lists the boundaries). Relationship loading
> uses a `LEFT JOIN` internally (the `joined` strategy); that is a separate path.
```

Leave the earlier blockquote about the query-expression `Col` case as-is (that functor is still not instantiated; the builder uses `#label`/`Column`, not a `Col` expression context).

- [ ] **Step 5: Bump the Tutorials section nav_order**

Because `queries.md` takes `nav_order: 8`, change `docs/tutorials/index.md` front-matter from `nav_order: 8` to `nav_order: 9` so the Tutorials section sorts after Queries.

- [ ] **Step 6: Full suite + honesty grep**

Run: `nix develop -c .zinc/build/spec 2>&1 | tail -2` — expect all green (96 from Task 5; this task adds no tests).
Run: `grep -rniE "no template haskell|sqlalchemy|—" docs/queries.md docs/index.md docs/entities.md` — expect no em-dashes, no SQLAlchemy, no stale claims in the touched pages.

- [ ] **Step 7: Commit**

```bash
git add src/Manifest.hs docs/queries.md docs/index.md docs/entities.md docs/tutorials/index.md
git commit -m "feat(query): export builder; Queries manual page; status updates"
```

---

## Self-Review

**1. Spec coverage** (against the chosen "composable query builder" with `from` / `where_` / `orderBy` / `limit` / `groupBy` / `innerJoin` / `aggregate` / `runQuery`):
- `from` → Task 1. `where_` → Task 2. `orderBy` (+`asc`/`desc`) / `limit` / `offset` → Task 3. `innerJoin` (+`on`) → Task 4. `groupBy` + `aggregate` (+`count` and grouped runners) → Task 5. `runQuery` → Task 1. ✓
- Compile-time table-in-scope safety → `TableElem` index, exercised by Task 2 Step 5. ✓
- Joins return tuples via composed `RowDecoder`s → Task 4. ✓
- Deferred items (outer joins, HAVING, subqueries, CTEs, multi-column GROUP BY, self-joins, managed results) are documented as Planned in `queries.md` (Task 6 Step 2), not implied to work. ✓

**2. Placeholder scan:** Each task has complete code or exact commands + expected counts. The two genuinely environment-specific spots (the exact `DbException`/`DecodeFailure` decode-throw, and confirming `COUNT`/`SUM` text decodes as `Int`) are called out with the precise file/function to copy from (`Session.decodeRowDb`, `Error.hs`) and resolved by compile/TDD feedback rather than left vague.

**3. Type consistency:**
- `Query (ts :: [Type]) r` — used consistently; `from :: Query '[a] a`, `where_ :: … -> Query ts r -> Query ts r`, `innerJoin :: … -> Query ts r -> Query (b ': ts) (r, b)`. ✓
- `Column a t = Column { colName }` and `Cond a = Cond ByteString Op SqlParam` match `Manifest.Core.Query` (verified against the live source). `#userName :: Column User Text`, `#userName ==. v :: Cond User`. ✓
- `aliasOf @a`, `qualifiedCols`, `renderWhere`, `decodeRowAs` are defined once (Tasks 1/5) and reused; `decodeRowAs` is the single decode-throw helper `runQuery`, `count`, `aggregate`, and the grouped runners all call. ✓
- `RowDecoder` is `Applicative`, so `(,) <$> qDecode <*> rowDecoder @b` (Task 4) and `(,) <$> field <*> field` (Task 5) typecheck and decode left to right, matching the SELECT column order. ✓
- `Agg (..)`, `Grouped k ts r`, `OrderTerm a`, `On` — each introduced once and used by the matching combinator/runner. ✓

**Open risks carried into execution:** (a) the decode-throw plumbing (resolved by copying `decodeRowDb`); (b) closed-type-family `TableElem` overlap order (the `a (a ': _)` then `a (_ ': ts)` ordering relies on closed-family apartness — standard, same shape as the existing relation `Member`); (c) `aggregate`'s `Maybe n` decode for `NULL`-on-empty (the existing `FromField (Maybe a)` instance covers it). All three surface immediately under TDD compile/run feedback.
