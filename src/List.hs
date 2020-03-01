module List
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
  , EncodedListItem(Single, Multiple)
  , myEncodeModified
  , myDecodeModified
  , myEncodeDirect
  , myDupli
  , myRepli
  , myDropEvery
  , mySplit
  , mySlice
  , myRotate
  , myRemoveAt
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
myButLast [_] = Nothing
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
  deriving (Show)

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
-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--
--
myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode [] = []
myEncode xs = [(length list, head list) | list <- myPack xs]

-- Problem 11
-- Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.
--
-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
--
--
data EncodedListItem a
  = Multiple Int a
  | Single a
  deriving (Show, Eq)

myEncodeModified :: (Eq a) => [a] -> [EncodedListItem a]
myEncodeModified xs = map makeEncodedList (myEncode xs)
  where
    makeEncodedList (1, x) = Single x
    makeEncodedList (n, x) = Multiple n x

-- myEncodeModified = map makeEncodedList . myEncode
-- Problem 12
-- Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct
-- its uncompressed version.
--
-- λ> decodeModified
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
--
--
myDecodeModified :: [EncodedListItem a] -> [a]
myDecodeModified xs = foldl (++) [] $ map decode xs
  where
    decode (Single x) = [x]
    decode (Multiple n x) = take n $ repeat x

-- Problem 13
-- Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method
-- directly. I.e. don't explicitly create the sublists containing the
-- duplicates, as in problem 9, but only count them. As in problem P11,
-- simplify the result list by replacing the singleton lists (1 X) by X.
--
-- (encode-direct '(a a a a b c c a a d e e e e))
-- ; => ((4 A) B (2 C) (2 A) D (4 E))
--
--
myEncodeDirect :: (Eq a) => [a] -> [EncodedListItem a]
myEncodeDirect [] = []
myEncodeDirect (x:xs) = map encode $ group [(1, x)] xs
  where
    group taken [] = taken
    group taken (first:rest) =
      let (num, val) = last taken
       in group
            (if val == first
               then init taken ++ [(num + 1, val)]
               else taken ++ [(1, first)])
            rest
    encode (1, x) = Single x
    encode (n, x) = Multiple n x

-- Problem 14
-- Duplicate the elements of a list.
--
--
myDupli :: [a] -> [a]
myDupli [] = []
myDupli (x:xs) = [x, x] ++ myDupli xs

-- Problem 15
-- Replicate the elements of a list a given number of times.
--
--
myRepli :: [a] -> Int -> [a]
myRepli [] _ = []
myRepli (x:xs) n = (take n $ repeat x) ++ myRepli xs n

-- Problem 16
-- Drop every N'th element from a list.
--
--
myDropEvery :: [a] -> Int -> [a]
myDropEvery list n
  | n <= 0 = list
myDropEvery list initial = solve list initial
  where
    solve [] _ = []
    solve (x:xs) 1 = solve xs initial
    solve (x:xs) n = [x] ++ solve xs (n - 1)

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
--
--
mySplit :: [a] -> Int -> ([a], [a])
mySplit list index
  | index <= 0 = ([], list)
mySplit [] _ = ([], [])
mySplit (x:xs) n =
  let result = mySplit xs (n - 1)
   in (x : (fst result), snd result)

-- Problem 18
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits
-- included). Start counting the elements with 1.
--
--
mySlice :: [a] -> Int -> Int -> [a]
mySlice [] _ _ = []
mySlice (x:xs) begin end
  | begin > 1 = mySlice xs (begin - 1) (end - 1)
  | end == 0 = []
  | otherwise = x : mySlice xs 1 (end - 1)

-- Problem 19
-- Rotate a list N places to the left.
--
--
myRotate :: [a] -> Int -> [a]
myRotate [] _ = []
myRotate list@(x:xs) n
  | n == 0 = list
  | n > 0 = myRotate (xs ++ [x]) (n - 1)
  | n < 0 = myRotate (last list : x : init xs) (n + 1)

-- Problem 20
-- Remove the K'th element from a list.
--
--
myRemoveAt :: [a] -> Int -> [a]
myRemoveAt [] _ = []
myRemoveAt list@(x:xs) n
  | n <= 0 = list
  | n == 1 = xs
  | otherwise = x : myRemoveAt xs (n - 1)
