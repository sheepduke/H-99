module S1Test
  ( tests
  ) where

import S1
import Test.HUnit

tests =
  test
      -- myLast
    [ "P1" ~: "myLast []" ~: (Nothing :: Maybe Int) ~=? myLast []
    , "P1" ~: "myLast [1]" ~: Just 1 ~=? myLast [1]
    , "P1" ~: "myLast [1, 2]" ~: Just 2 ~=? myLast [1, 2]
    , "P1" ~: "myLast ['x', 'y', 'z']" ~: Just 'z' ~=? myLast ['x', 'y', 'z']
    -- myButLast
    , "P2" ~: "myButLast []" ~: (Nothing :: Maybe Int) ~=? myButLast []
    , "P2" ~: "myButLast [1]" ~: (Nothing :: Maybe Int) ~=? myButLast [1]
    , "P2" ~: "myButLast [1, 2]" ~: Just 1 ~=? myButLast [1, 2]
    , "P2" ~: "myButLast [1, 2, 3]" ~: Just 2 ~=? myButLast [1, 2, 3]
    ]
