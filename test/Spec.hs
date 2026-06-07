module Main (main) where

import Harness
import qualified CodecSpec
import qualified PostgresSpec
import qualified MetaSpec
import qualified SqlSpec
import qualified SessionSpec

main :: IO ()
main = runTests (CodecSpec.tests ++ PostgresSpec.tests ++ MetaSpec.tests ++ SqlSpec.tests ++ SessionSpec.tests)
