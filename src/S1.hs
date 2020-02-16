module S1
  ( myLast
  , myButLast
  , myElementAt
  , myLength
  , myReverse
  , myIsPalindrome
  , myFlatten
  , NestedList(Elem, List)
  , myCompress
  , myPack
  , myEncode
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
myElementAt :: (Integral b) => [a] -> b -> Maybe a
myElementAt xs index
  | null xs = Nothing
  | index < 0 = Nothing
myElementAt (x:_) 1 = Just x
myElementAt (_:xs) index = myElementAt xs $ index - 1

-- Problem 4
-- Find the number of elements of a list.
--
--
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
-- Reverse a list.
--
--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or
-- backward; e.g. (x a m a x).
--
--  > isPalindrome [1,2,3]
-- False
--  > isPalindrome "madamimadam"
-- True
--  > isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
--
--
myIsPalindrome :: (Eq a) => [a] -> Bool
myIsPalindrome xs = xs == reverse xs

-- Problem 7
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by
-- replacing each list with its elements (recursively).
--
-- In Lisp:
--   (my-flatten '(a (b (c d) e)))
--   ; => (A B C D E)
--
--
data NestedList a
  = Elem a
  | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List xs) = foldl (\z -> \x -> z ++ x) [] $ map (\x -> myFlatten x) xs

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element. The order of the elements should not be changed.
--
-- In Lisp:
--   (compress '(a a a a b c c a a d e e e e))
--   ; => (A B C A D E)
--
--
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress (cur:rest) = solve cur rest
  where
    solve x [] = [x]
    solve x (y:xs)
      | x == y = solve x xs
      | otherwise = [x] ++ solve y xs

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.
-- 
-- In Lisp:
--   (pack '(a a a a b c c a a d e e e e))
--   ; => ((A A A A) (B) (C C) (A A) (D) (E E E E))
--
--
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack (x':xs') = solve [[x']] xs'
  where
    solve taken [] = taken
    solve taken (x:xs)
      | x == (head . last) taken = solve (init taken ++ [last taken ++ [x]]) xs
      | otherwise = solve (taken ++ [[x]]) xs

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.
--
--
myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode [] = []
myEncode xs = [(length list, head list) | list <- myPack xs]
