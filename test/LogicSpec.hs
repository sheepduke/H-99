module LogicSpec
  ( tests
  ) where

import Logic
import Test.HUnit

tests =
  [ "P46" ~:
    [ (True, True, True)
    , (True, False, True)
    , (False, True, False)
    , (False, False, False)
    ] ~=?
    myTable (\x y -> x `myAnd` (x `myOr` not y))
  , "P49" ~: ["000", "001", "010", "011", "100", "101", "110", "111"] ~=?
    myGray 3
  ]
