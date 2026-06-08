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
