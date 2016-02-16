module Problems.Lists (tests)
 where

import qualified Data.List             as L
import           Solutions.Lists
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Types
import           Utils

tests :: TestTree
tests = testGroup "List Problems" $ reverse
                  [ testProperty "1. Return the last element in the list" lastProp
                  , testProperty "2. Return all but the last element in the list" initProp
                  , testProperty "3. Return the kth element in the list" kthProp
                  , testProperty "4. Find the number of elements in the list" (sameFunctionList length solution4)
                  , testProperty "5. Reverse a list" (sameFunctionList reverse solution5)
                  , testProperty "6. Find out whether a list is a palindrome" palindromeProperty
                  , testProperty "7. Flatten a nested list" flattenProp
                  , testProperty "8. Eliminate duplicates" (sameFunctionList L.nub solution8)
                  , testProperty "9. Pack consecutive duplicates of list elements into sublists." (sameFunctionList tPack solution9)
                  , testProperty "10. Run-length encoding of a list."
                                 (sameFunctionList tRun solution10)
                  , testProperty "11. Modified run length encoding"
                                 (sameFunctionList tEncodeModified solution11)
                  , testProperty "12. Decode run length encoding" (sameFunctionListItem tDecodeRunEncoding solution12)
                  ]


sameFunctionList :: (Show a, Eq a) => ([TestType] -> a) -> ([TestType] -> a) -> [TestType] -> Bool
sameFunctionList = sameFunction

sameFunctionListItem :: (Show a, Eq a) => ([ListItem TestType] -> a) -> ([ListItem TestType] -> a) -> [ListItem TestType] -> Bool
sameFunctionListItem = sameFunction

lastProp :: [TestType] -> Bool
lastProp [] = True
lastProp xs = last xs == solution1 xs

initProp :: [TestType] -> Bool
initProp [] = True
initProp xs = init xs == solution2 xs

kthProp :: Int -> [TestType] -> Property
kthProp i xs = i == 0 ==>
        xs !! i === solution3 i xs

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

tPack :: Eq a => [a] -> [[a]]
tPack (x:xs) = let (first,rest) = span (==x) xs
                 in (x:first) : tPack rest
tPack [] = []

tRun :: Eq a => [a] -> [(Int, a)]
tRun xs = let enc = tPack xs in
     map (\x -> (length x, head x)) enc

tEncodeModified :: Eq a => [a] -> [ListItem a]
tEncodeModified = map encodeHelper . tRun
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

tDecodeRunEncoding :: [ListItem a] -> [a]
tDecodeRunEncoding = undefined
