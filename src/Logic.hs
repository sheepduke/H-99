module Logic
  ( myAnd
  , myOr
  , myNand
  , myNor
  , myXor
  , myTable
  , myGray
  , myHuffman
  ) where

-- Problem 46
-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
-- logical equivalence) which succeed or fail according to the result of their
-- respective operations; e.g. and(A,B) will succeed, if and only if both A and
-- B succeed.
-- A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
-- Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
-- Example:
-- λ> table (\a b -> (and' a (or' a b)))
-- True True True
-- True False True
-- False True False
-- False False False
--
--
type TruthTableRow = (Bool, Bool, Bool)

type TruthTable = [TruthTableRow]

myAnd :: Bool -> Bool -> Bool
myAnd = (&&)

myOr :: Bool -> Bool -> Bool
myOr = (||)

myNand :: Bool -> Bool -> Bool
myNand x y = not (x && y)

myNor :: Bool -> Bool -> Bool
myNor x y = not (x || y)

myXor :: Bool -> Bool -> Bool
myXor x y = (x || y) && not (x && y)

myTable :: (Bool -> Bool -> Bool) -> TruthTable
myTable fun =
  [solve True True, solve True False, solve False True, solve False False]
  where
    solve x y = (x, y, fun x y)

-- Problem 49
-- Gray codes.
-- An n-bit Gray code is a sequence of n-bit strings constructed according to
-- certain rules. For example,
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
-- Find out the construction rules and write a predicate with the following specification:
-- Example in Haskell:
-- λ> gray 3
-- ["000","001","011","010","110","111","101","100"]
--
--
myGray 1 = ["0", "1"]
myGray n =
  let lastGray = myGray (n - 1)
   in map ("0" ++) lastGray ++ map ("1" ++) lastGray
