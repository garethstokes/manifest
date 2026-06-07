{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module SqlSpec (tests) where

import qualified Data.ByteString.Char8 as BC
import Fixtures (User)
import Manifest.Core.Query
import Harness

tests :: [Test]
tests = group "Query"
  [ test "builds conditions from labels with camel→snake names" $ do
      assertEqual "eq" (Cond "user_name" OpEq (Just (BC.pack "Bob")))
                       (#userName ==. ("Bob" :: String) :: Cond User)
      assertEqual "lt" (Cond "user_id" OpLt (Just (BC.pack "5")))
                       (#userId <. (5 :: Int) :: Cond User)
  , test "builds assignments from labels" $
      assertEqual "asn" (Assign "user_name" (Just (BC.pack "Bob")))
                        (#userName =. ("Bob" :: String) :: Assign User)
  ]
