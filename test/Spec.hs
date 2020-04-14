module Main where

import qualified ArithmeticSpec as Arithmetic
import qualified ListSpec as List
import qualified LogicSpec as Logic
import Test.HUnit

tests = List.tests ++ Arithmetic.tests ++ Logic.tests

main :: IO Counts
main = runTestTT . test $ tests
