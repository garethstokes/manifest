module Main (main) where

import Harness
import qualified CodecSpec

main :: IO ()
main = runTests CodecSpec.tests
