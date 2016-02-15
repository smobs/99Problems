module Problems.Lists (tests)
 where

import qualified Data.List             as L
import           Solutions.Lists
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Types

type TestType = Int

tests :: TestTree
tests = testGroup "List Problems"
                  [ testProperty "Return the last element in the list" lastProp
                  , testProperty "Return all but the last element in the list" initProp
                  , testProperty "Return the kth element in the list" kthProp
                  , testProperty "Find the number of elements in the list" lengthProp
                  , testProperty "Reverse a list" reverseProp
                  , testProperty "Find out whether a list is a palindrome" palindromeProperty
                  , testProperty "Flatten a nested list" flattenProp
                  , testProperty "Eliminate duplicates" dupProp
                  , testProperty "Pack consecutive duplicates of list elements into sublists." packListProp
                  , testProperty "Run-length encoding of a list." runLengthProp
                  ]

lastProp :: [TestType] -> Bool
lastProp  xs = last xs == solution1 xs

initProp :: [TestType] -> Bool
initProp xs = init xs == solution2 xs

kthProp :: Int -> [TestType] -> Property
kthProp i xs = i == 0 ==>
        xs !! i === solution3 i xs

lengthProp :: [TestType] -> Bool
lengthProp xs = length xs == solution4 xs

reverseProp :: [TestType] -> Bool
reverseProp xs = reverse xs == solution5 xs

palindromeProperty :: Property
palindromeProperty = palinProp solution6 .&&.  (notPalindrone (\xs -> solution6 xs))

palinProp :: ([TestType] -> Bool) ->  [TestType] -> Property
palinProp f [] = property $ f []
palinProp f [x] = property $ f [x]
palinProp f xs =  (xs /= []) ==> (f p1) && (f p2)
              where p1 = xs ++ (reverse xs)
                    p2 = xs ++ (tail (reverse xs))

notPalindrone ::  ([TestType] -> Bool) -> Positive Int -> Positive Int -> Property
notPalindrone f (Positive i) (Positive n) = counterexample ("Unexpected palindrome: " ++ show notPal) (not $ f notPal)
              where notPal = take (n + 1) [i ..]


flattenProp :: NestedList TestType -> Bool
flattenProp xs = tflatten xs == solution7 xs

tflatten :: NestedList a -> [a]
tflatten (Elem x) = [x]
tflatten (List x) = concatMap tflatten x

dupProp :: [TestType] -> Bool
dupProp xs = L.nub xs == solution8 xs

packListProp :: [TestType] -> Bool
packListProp xs = tPack xs == solution9 xs

tPack :: Eq a => [a] -> [[a]]
tPack (x:xs) = let (first,rest) = span (==x) xs
                 in (x:first) : tPack rest
tPack [] = []

runLengthProp :: [TestType] -> Bool
runLengthProp xs = tRun xs == solution10 xs

tRun :: Eq a => [a] -> [(Int, a)]
tRun xs = let enc = tPack xs in
     map (\x -> (length x, head x)) enc
