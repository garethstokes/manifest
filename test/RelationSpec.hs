{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module RelationSpec (tests) where

import qualified Data.ByteString.Char8 as BC
import Fixtures (Post, PostT (..), Profile, ProfileT (..), User, UserT (..), withTestDb)
import Manifest.Core.Relation (RelSpec (..), relSpec)
import Manifest.Relation (load)
import Manifest.Session
import Harness

tests :: [Test]
tests = group "Relation"
  [ test "relSpec for User \"posts\" is RelMany on post_author" $
      case relSpec @User @"posts" of
        RelMany fk -> assertEqual "fk" "post_author" fk
        _          -> assertBool "expected RelMany" False
  , test "relSpec for User \"profile\" is RelOpt on profile_user" $
      case relSpec @User @"profile" of
        RelOpt fk -> assertEqual "fk" "profile_user" fk
        _         -> assertBool "expected RelOpt" False
  , test "load #posts returns the user's posts (managed)" $
      withTestDb $ \pool -> do
        titles <- withSession pool $ do
          u <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P1" } :: Post)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P2" } :: Post)
          ps <- load #posts u
          pure (map postTitle ps)
        assertEqual "titles" ["P1", "P2"] titles
  , test "load #profile returns Nothing when absent, Just when present" $
      withTestDb $ \pool -> do
        (none, some) <- withSession pool $ do
          u <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          n <- load #profile u
          _ <- add (Profile { profileId = 0, profileUser = userId u, profileBio = "hi" } :: Profile)
          s <- load #profile u
          pure (fmap profileBio n, fmap profileBio s)
        assertEqual "none" Nothing none
        assertEqual "some" (Just "hi") some
  , test "a loaded child is managed: modify + save emits a minimal UPDATE" $
      withTestDb $ \pool -> do
        log' <- withSession pool $ do
          u <- add (User { userId = 0, userName = "Ada", userEmail = Nothing } :: User)
          _ <- add (Post { postId = 0, postAuthor = userId u, postTitle = "P1" } :: Post)
          ps <- load #posts u
          let p = head ps :: Post
          withTransaction $ save (p { postTitle = "Edited" } :: Post)
          statementLog
        assertEqual "minimal child update"
          ["UPDATE posts SET post_title = $1 WHERE post_id = $2"]
          (filter (BC.isPrefixOf "UPDATE" . fst) log' >>= \(s,_) -> [BC.unpack s])
  ]
