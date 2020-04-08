module Main where

import qualified ArithmeticSpec as Arithmetic
import qualified ListSpec as List
import Test.HUnit

tests = List.tests ++ Arithmetic.tests

main :: IO Counts
main = runTestTT . test $ tests
