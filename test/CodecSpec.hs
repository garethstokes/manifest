module CodecSpec (tests) where

import qualified Data.ByteString.Char8 as BC
import Manifest.Core.Codec
import Manifest.Error (DecodeError (..))
import Harness

tests :: [Test]
tests = group "Codec"
  [ test "encodes scalars to text params" $ do
      assertEqual "int"     (Just (BC.pack "42")) (toField (42 :: Int))
      assertEqual "string"  (Just (BC.pack "hi")) (toField ("hi" :: String))
      assertEqual "nothing" (Nothing :: Maybe BC.ByteString) (toField (Nothing :: Maybe Int))
      assertEqual "just"    (Just (BC.pack "7")) (toField (Just (7 :: Int)))
  , test "decodes scalars from text params" $ do
      assertEqual "int"       (Right (42 :: Int)) (fromField (Just (BC.pack "42")))
      assertEqual "maybe-null"(Right (Nothing :: Maybe Int)) (fromField Nothing)
      assertBool  "bad int is Left"
        (either (const True) (const False) (fromField (Just (BC.pack "x")) :: Either DecodeError Int))
  , test "applicative RowDecoder runs left-to-right with no arity ceiling" $ do
      let dec = (,,,,) <$> field <*> field <*> field <*> field <*> field
          row = [ Just (BC.pack "1"), Just (BC.pack "a"), Nothing
                , Just (BC.pack "t"), Just (BC.pack "9") ]
      assertEqual "5-tuple"
        (Right (1 :: Int, "a" :: String, Nothing :: Maybe String, True, 9 :: Int))
        (decodeRow dec row)
  ]
