{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module QueryBuilderSpec (tests) where

import Data.List (sort)
import Fixtures (Post, PostT (..), User, UserT (..), withTestDb)
import Manifest
import Manifest.Query
import Harness

tests :: [Test]
tests = group "QueryBuilder"
  [ test "single-table select renders SELECT alias.cols FROM table AS t0" $
      assertEqual "sql"
        "SELECT t0.user_id, t0.user_name, t0.user_email FROM users AS t0"
        (fst (renderQueryM (do u <- from @User; pure u)))
  , test "where_ renders an alias-qualified, numbered condition" $
      assertEqual "sql"
        "SELECT t0.user_id, t0.user_name, t0.user_email FROM users AS t0 WHERE t0.user_name = $1"
        (fst (renderQueryM (do u <- from @User
                               where_ (u ^. #userName .== val ("Bob" :: String))
                               pure u)))
  , test "runQuery returns all rows" $
      withTestDb $ \pool -> do
        names <- withSession pool $ do
          _ <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (User { userId = 0, userName = "Bob", userEmail = Nothing } :: User)
          us <- runQuery (do u <- from @User; pure u)
          pure (sort (map userName us))
        assertEqual "names" ["Ada", "Bob"] names
  , test "where_ filters rows at runtime" $
      withTestDb $ \pool -> do
        names <- withSession pool $ do
          _ <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (User { userId = 0, userName = "Bob", userEmail = Nothing } :: User)
          us <- runQuery (do u <- from @User
                             where_ (u ^. #userName .== val ("Bob" :: String))
                             pure u)
          pure (map userName us)
        assertEqual "names" ["Bob"] names
  , test "orderBy + limit + offset render in order" $
      assertEqual "sql"
        "SELECT t0.user_id, t0.user_name, t0.user_email FROM users AS t0 ORDER BY t0.user_name DESC LIMIT 2 OFFSET 1"
        (fst (renderQueryM (do u <- from @User
                               orderBy [desc (u ^. #userName)]
                               limit 2
                               offset 1
                               pure u)))
  , test "orderBy + limit return rows in order, paginated" $
      withTestDb $ \pool -> do
        names <- withSession pool $ do
          mapM_ (\n -> add (User { userId = 0, userName = n, userEmail = Nothing } :: User))
                ["Ada","Bob","Cay","Dee"]
          us <- runQuery (do u <- from @User
                             orderBy [asc (u ^. #userName)]
                             limit 2
                             pure u)
          pure (map userName us)
        assertEqual "first two by name" ["Ada","Bob"] names
  ]
