module Arithmetic
  ( myIsPrime
  , myGCD
  , myCoprime
  , myTotient
  , myPrimeFactors
  , myPrimeFactorsMult
  , myPrimesR
  , myGoldbach
  , myGoldbachList
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

-- Problem 39
-- A list of prime numbers.
-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
-- Example in Haskell:
-- λ> primesR 10 20
-- [11,13,17,19]
--
--
myPrimesR :: Int -> Int -> [Int]
myPrimesR begin end = filter myIsPrime [begin .. end]

-- Problem 40
-- Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number greater than 2 is
-- the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
-- famous facts in number theory that has not been proved to be correct in the
-- general case. It has been numerically confirmed up to very large numbers
-- (much larger than we can go with our Prolog system). Write a predicate to
-- find the two prime numbers that sum up to a given even integer.
-- Example:
-- λ> goldbach 28
-- (5, 23)
--
--
myGoldbach :: Int -> (Int, Int)
myGoldbach num =
  let first =
        head . filter (\x -> myIsPrime x && myIsPrime (num - x)) $ [2 .. num]
   in (first, num - first)

-- Problem 41
-- Given a range of integers by its lower and upper limit, print a list of all
-- even numbers and their Goldbach composition.
-- In most cases, if an even number is written as the sum of two prime numbers,
-- one of them is very small. Very rarely, the primes are both bigger than say
-- 50. Try to find out how many such cases there are in the range 2..3000.
-- Example:
-- λ> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- λ> goldbachList' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]
--
--
myGoldbachList :: Int -> Int -> [(Int, Int)]
myGoldbachList begin end = map myGoldbach . filter even $ [begin .. end]
