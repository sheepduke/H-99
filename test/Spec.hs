module Main where

import qualified S1Test
import Test.HUnit

tests = S1Test.tests

main :: IO Counts
main = runTestTT tests
