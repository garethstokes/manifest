{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module JsonSpec (tests) where

import Autodocodec
import Data.Text (Text)
import Manifest (Json (..), DbType (..), Codec (..), encode)
import Manifest.Core.SqlType (SqlType (..))
import Harness (Test, group, test, assertEqual, assertBool)

data Prefs = Prefs { prefTheme :: Text, prefTags :: [Text] }
  deriving (Eq, Show)

instance HasCodec Prefs where
  codec = object "Prefs" $
    Prefs <$> requiredField "theme" "ui theme" .= prefTheme
          <*> requiredField "tags"  "tags"     .= prefTags

tests :: [Test]
tests = group "Json"
  [ test "Json column reports jsonb and round-trips its codec" $ do
      let p   = Prefs "dark" ["a", "b"]
          enc = encode (Json p)
      assertEqual "sqltype is jsonb" SqlJsonb (cSqlType (dbType @(Json Prefs)))
      assertBool  "encodes to some bytes" (enc /= Nothing)
      assertEqual "decode . encode = id"
        (Right (Json p))
        (cDecode (dbType @(Json Prefs)) enc)
  ]
