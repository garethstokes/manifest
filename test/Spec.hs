module Main (main) where

import Harness
import qualified CodecSpec
import qualified PostgresSpec

main :: IO ()
main = runTests (CodecSpec.tests ++ PostgresSpec.tests)
