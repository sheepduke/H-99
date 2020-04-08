module Arithmetic
  ( myIsPrime
  , myGCD
  , myCoprime
  , myTotient
  , myPrimeFactors
  , myPrimeFactorsMult
  ) where

import Data.List

-- Problem 31
-- Determine whether a given integer number is prime.
-- Example:
-- λ> isPrime 7
-- True
--
--
myIsPrime :: Int -> Bool
myIsPrime num = num > 1 && all (\x -> num `rem` x > 0) [2 .. floorSqrt num]
  where
    floorSqrt = floor . sqrt . fromIntegral

-- Problem 32
-- Determine the greatest common divisor of two positive integer
-- numbers. Use Euclid's algorithm.
-- Example:
-- λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]
--
--
myGCD :: Int -> Int -> Int
myGCD x y
  | x < y = myGCD y x
  | otherwise =
    let remValue = rem x y
     in if remValue == 0
          then abs y
          else myGCD x remValue

-- Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers are
-- coprime if their greatest common divisor equals 1.
-- Example:
-- λ> coprime 35 64
-- True
--
--
myCoprime :: Int -> Int -> Bool
myCoprime x y = gcd x y == 1

-- Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integers r (1 <= r < m) that are coprime to m.
-- λ> totient 10
-- 4
--
--
myTotient :: Int -> Int
myTotient num = length . filter (\x -> myCoprime x num) $ [1 .. num - 1]

-- Problem 35
-- Determine the prime factors of a given positive integer. Construct a flat
-- list containing the prime factors in ascending order.
-- Example:
-- λ> primeFactors 315
-- [3, 3, 5, 7]
--
--
myPrimeFactors :: Int -> [Int]
myPrimeFactors 1 = []
myPrimeFactors num =
  let current = head . filter (\x -> rem num x == 0) $ [2 .. num]
   in current : myPrimeFactors (div num current)

-- Problem 36
-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
-- Example:
-- λ> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]
--
--
myPrimeFactorsMult :: Int -> [(Int, Int)]
myPrimeFactorsMult 1 = []
myPrimeFactorsMult num =
  map (\x -> (head x, length x)) . group $ myPrimeFactors num
