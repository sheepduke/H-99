module Main where

import qualified ListSpec as List
import Test.HUnit

tests = List.tests

main :: IO Counts
main = runTestTT tests
