module ListSpec
  ( tests
  ) where

import Data.List
import List
import Test.HUnit

tests =
  [ "P1" ~: Nothing ~=? myLast ""
  , "P1" ~: Just 1 ~=? myLast [1]
  , "P1" ~: Just 2 ~=? myLast [1, 2]
  , "P1" ~: Just 'z' ~=? myLast ['x', 'y', 'z']
    -- myButLast
  , "P2" ~: Nothing ~=? myButLast ""
  , "P2" ~: Nothing ~=? myButLast [1]
  , "P2" ~: Just 1 ~=? myButLast [1, 2]
  , "P2" ~: Just 2 ~=? myButLast [1, 2, 3]
    -- myElementAt
  , "P3" ~: Nothing ~=? myElementAt "" 1
  , "P3" ~: Nothing ~=? myElementAt [1] (-1)
  , "P3" ~: Nothing ~=? myElementAt [1] 0
  , "P3" ~: Just 1 ~=? myElementAt [1] 1
  , "P3" ~: Nothing ~=? myElementAt [1] 2
  , "P3" ~: Just 2 ~=? myElementAt [1, 2] 2
  , "P3" ~: Nothing ~=? myElementAt [1, 2] 3
    -- myLength
  , "P4" ~: 0 ~=? myLength []
  , "P4" ~: 1 ~=? myLength [1]
  , "P4" ~: 2 ~=? myLength [1, 2]
    -- myReverse
  , "P5" ~: ([] :: [Int]) ~=? myReverse []
  , "P5" ~: [1] ~=? myReverse [1]
  , "P5" ~: [2, 1] ~=? myReverse [1, 2]
  , "P5" ~: [3, 2, 1] ~=? myReverse [1, 2, 3]
  , "P5" ~: "!amanap ,lanac a ,nalp a ,nam A" ~=?
    myReverse "A man, a plan, a canal, panama!"
    -- myIsPalindrome
  , "P6" ~: False ~=? myIsPalindrome [1, 2, 3]
  , "P6" ~: True ~=? myIsPalindrome [1, 2, 1]
  , "P6" ~: True ~=? myIsPalindrome "AsDfDsA"
  , "P6" ~: False ~=? myIsPalindrome "macadamize"
    -- myFlatten
  , "P7" ~: [1] ~=? myFlatten (Elem 1)
  , "P7" ~: ([] :: [Int]) ~=? myFlatten (List [])
  , "P7" ~: [1, 2, 3, 4, 5] ~=?
    myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    -- myCompress
  , "P8" ~: "" ~=? myCompress ""
  , "P8" ~: [1] ~=? myCompress [1]
  , "P8" ~: [1, 2] ~=? myCompress [1, 2]
  , "P8" ~: [1, 2] ~=? myCompress [1, 2, 2]
  , "P8" ~: "helo" ~=? myCompress "hello"
    -- myPack
  , "P9" ~: [] ~=? myPack ""
  , "P9" ~: ["aa", "b", "c"] ~=? myPack "aabc"
  , "P9" ~: [[1, 1, 1, 1], [2], [3, 3], [1, 1]] ~=?
    myPack [1, 1, 1, 1, 2, 3, 3, 1, 1]
    -- myEncode
  , "P10" ~: [] ~=? myEncode ""
  , "P10" ~: [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a')] ~=? myEncode "aaaabccaa"
    -- myEncodeModified
  , "P11" ~: [] ~=? myEncode ""
  , "P11" ~:
    [ Multiple 4 'a'
    , Single 'b'
    , Multiple 2 'c'
    , Multiple 2 'a'
    , Single 'd'
    , Multiple 4 'e'
    ] ~=?
    myEncodeModified "aaaabccaadeeee"
    -- myDecodeModified
  , "P12" ~: "" ~=? myDecodeModified []
  , "P12" ~: "aaabbcaa" ~=?
    myDecodeModified
      [Multiple 3 'a', Multiple 2 'b', Single 'c', Multiple 2 'a']
    -- myEncodeDirect
  , "P13" ~:
    [ Multiple 4 'a'
    , Single 'b'
    , Multiple 2 'c'
    , Multiple 2 'a'
    , Single 'd'
    , Multiple 4 'e'
    ] ~=?
    myEncodeDirect "aaaabccaadeeee"
    -- myDupli
  , "P14" ~: "" ~=? myDupli ""
  , "P14" ~: [1, 1] ~=? myDupli [1]
  , "P14" ~: "aabbcc" ~=? myDupli "abc"
    -- myRepli
  , "P15" ~: "" ~=? myRepli "" 3
  , "P15" ~: [1, 1, 2, 2, 3, 3, 3, 3] ~=? myRepli [1, 2, 3, 3] 2
    -- myDropEvery
  , "P16" ~: "" ~=? myDropEvery "" 1
  , "P16" ~: [1, 2, 3] ~=? myDropEvery [1, 2, 3] 0
  , "P16" ~: [1, 2, 4, 5] ~=? myDropEvery [1, 2, 3, 4, 5, 6] 3
    -- mySplit
  , "P17" ~: ("", "") ~=? mySplit "" 1
  , "P17" ~: ("", "a") ~=? mySplit "a" 0
  , "P17" ~: ("a", "") ~=? mySplit "a" 1
  , "P17" ~: ("abc", "defg") ~=? mySplit "abcdefg" 3
    -- mySlice
  , "P18" ~: "cdefg" ~=? mySlice "abcdefgh" 3 7
  , "P18" ~: [1] ~=? mySlice [1, 2] 1 1
  , "P18" ~: [2, 3] ~=? mySlice [1, 2, 3] 2 3
    -- myRotate
  , "P19" ~: [3, 4, 5, 1, 2] ~=? myRotate [1, 2, 3, 4, 5] 2
  , "P19" ~: [4, 5, 1, 2, 3] ~=? myRotate [1, 2, 3, 4, 5] (-2)
    -- myRemoveAt
  , "P20" ~: [2, 3, 4, 5] ~=? myRemoveAt [1, 2, 3, 4, 5] 1
  , "P20" ~: [1, 2, 3, 5] ~=? myRemoveAt [1, 2, 3, 4, 5] 4
  , "P20" ~: [1, 2, 3, 4] ~=? myRemoveAt [1, 2, 3, 4, 5] 5
    -- myInsert20
  , "P21" ~: [1, 2, 3, 4] ~=? myInsertAt 4 [1, 2, 3] 4
  , "P21" ~: [1, 2, 3, 4] ~=? myInsertAt 3 [1, 2, 4] 3
  , "P21" ~: [1, 2, 3, 4] ~=? myInsertAt 1 [2, 3, 4] 1
    -- myRange
  , "P22" ~: [1, 2, 3] ~=? myRange 1 3
  , "P22" ~: [4, 5, 6, 7] ~=? myRange 4 7
    -- myRndSelect
  , TestCase
      (do let list = [1, 3, 5, 7, 9]
              n = 3
          result <- myRndSelect list n
          assertBool "Should return N elements" (length result == n)
          assertBool
            "Should contain elements from list"
            (all (`elem` list) result))
    -- myDiffSelect
  , TestCase
      (do let count = 6
              end = 49
          result <- myDiffSelect count end
          assertBool "Should return N elements" (length result == count)
          assertBool
            "Should contain elements in range"
            (all (`elem` [1 .. end]) result))
    -- myRndPermu
  , TestCase
      (do let list = [1, 2, 3, 4]
          result <- myRndPermu list
          assertEqual
            "Result should equal to origin after both sorted"
            (sort result)
            (sort list))
    -- myLsort
  , "P28-a" ~: ["a", "bc", "abc", "def"] ~=? myLsort ["abc", "a", "bc", "def"]
    -- myLfsort
  , "P28-b" ~: ["abcde", "abc", "bcd", "ab", "cd", "ef"] ~=?
    myLfsort ["ab", "abc", "cd", "bcd", "ef", "abcde"]
  ]
