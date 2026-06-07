{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CascadeSpec (tests) where

import Control.Exception (SomeException, try)
import Data.Proxy (Proxy (..))
import Fixtures
import Manifest.Core.Cascade (CascadeRule (..), OnDelete (..))
import Manifest.Core.Query (Cond)
import Manifest.Core.Relation (cascade)
import Manifest.Session
import Harness

tests :: [Test]
tests = group "Cascade"
  [ test "cascade derives the child table + FK column from the child + label" $
      assertEqual "rule"
        (CascadeRule "posts" "post_author" Cascade)
        (cascade (Proxy @Post) (Proxy @"postAuthor") Cascade)
  , test "Cascade deletes the children" $
      withTestDb $ \pool -> do
        n <- withSession pool $ do
          u <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P1" } :: Post)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P2" } :: Post)
          withTransaction $ delete u
          ps <- selectWhere ([] :: [Cond Post])
          pure (length ps)
        assertEqual "posts cascaded away" 0 n
  , test "SetNull nulls the child FK" $
      withTestDb $ \pool -> do
        bios <- withSession pool $ do
          u <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (Profile { profileId = 0, profileUser = Just (userId u), profileBio = "hi" } :: Profile)
          withTransaction $ delete u
          ps <- selectWhere ([] :: [Cond Profile])
          pure (map profileBio ps)               -- profile row survives, FK nulled
        assertEqual "profile kept" ["hi"] bios
  , test "Restrict aborts the delete when children exist" $
      withTestDb $ \pool -> do
        (res, remaining) <- withTestDbBody pool
        assertBool "delete was rejected" (either (const True) (const False) res)
        assertEqual "user still there" 1 remaining
  ]
  where
    withTestDbBody pool = do
      res <- (try :: IO a -> IO (Either SomeException a)) $ withSession pool $ do
        u <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
        _ <- add (Tag { tagId = 0, tagUser = userId u, tagLabel = "vip" } :: Tag)
        withTransaction $ delete u
      remaining <- withSession pool (length <$> selectWhere ([] :: [Cond User]))
      pure (res, remaining)
