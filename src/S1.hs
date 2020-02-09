module S1
  ( myLast
  , myButLast
  ) where

-- Problem 1
-- Find the last element of a list.
--
--
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_:xs) = myLast xs

-- Problem 2
-- Find the last but one element of a list.
--
--
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [x] = Nothing
myButLast (x:_:[]) = Just x
myButLast (_:xs) = myButLast xs
-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
--
--
