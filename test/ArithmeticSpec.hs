module ArithmeticSpec
  ( tests
  ) where

import Arithmetic
import Test.HUnit

tests =
  [ "P31" ~: False ~=? myIsPrime 1
  , "P31" ~: True ~=? myIsPrime 2
  , "P31" ~: True ~=? myIsPrime 3
  , "P31" ~: False ~=? myIsPrime 4
  , "P31" ~: True ~=? myIsPrime 5
    -- myGCD
  , "P32" ~: [9, 3, 3] ~=? [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
  , "P33" ~: True ~=? myCoprime 35 64
  , "P34" ~: 4 ~=? myTotient 10
  , "P35" ~: [3, 3, 5, 7] ~=? myPrimeFactors 315
  , "P36" ~: [(3, 2), (5, 1), (7, 1)] ~=? myPrimeFactorsMult 315
  , "P39" ~: [11, 13, 17, 19] ~=? myPrimesR 10 20
  , "P40" ~: (5, 23) ~=? myGoldbach 28
  , "P41" ~: [(3, 7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17)] ~=?
    myGoldbachList 9 20
  ]
