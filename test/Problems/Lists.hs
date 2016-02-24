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
                  , testProperty "8. Eliminate duplicates" (sameFunctionList tDeduplicate solution8)
                  , testProperty "9. Pack consecutive duplicates of list elements into sublists." (sameFunctionList tPack solution9)
                  , testProperty "10. Run-length encoding of a list."
                                 (sameFunctionList tRun solution10)
                  ,testProperty "11. Modified run length encoding"
                                (sameFunctionList tEncodeModified solution11)
                  , testProperty "12. Decode run length encoding" (sameFunctionListItem decodeModified solution12)
                  , testProperty "13. Modified run length encoding without sublists"
                                 (sameFunctionList tEncodeModified solution13)
                  , testProperty "14. Duplicate all elements of the list."  (sameFunctionList tDuplicate solution14)
                  , testProperty "15. Replicate the element a given number of times" repeatProp
                  , testProperty "16. Drop every nth element" dropNthProp
                  , testProperty "17. Split a list into two parts" splitProp
                  , testProperty "18. Splice a list." sliceProp
                  , testProperty "19. Rotate list to the left." rotateProp
                  , testProperty "20. Remove element at." removeProp]


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

kthProp :: Positive Int -> [TestType] -> Property
kthProp (Positive i) xs = i < length xs  ==>
        xs !! i === solution3 i xs

tDeduplicate = map head . L.group
        
palindromeProperty :: Property
palindromeProperty = palinProp solution6 .&&.  (notPalindrone (\xs -> solution6 xs))

palinProp :: ([TestType] -> Bool) ->  [TestType] -> Property
palinProp f [] = property $ f []
palinProp f [x] = property $ f [x]
palinProp f xs = property $ (f p1) && (f p2)
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

decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x

tDuplicate list = concat [[x,x] | x <- list]

repeatProp :: Positive Int -> [TestType] ->  Bool
repeatProp (Positive i) = sameFunctionList (r i) (`solution15` i)
           where
                r :: Int -> [TestType] -> [TestType]
                r = concatMap . replicate

dropNthProp :: Positive Int -> [TestType] ->  Bool
dropNthProp  (Positive i) = sameFunctionList (r i) (`solution16` i)
                                where
                                     r :: Int -> [TestType] -> [TestType]
                                     r =  \n -> map snd . filter ((n/=) . fst) . zip (cycle [1..n])

splitProp :: Positive Int -> [TestType] ->  Property
splitProp (Positive i) xs = i < length xs ==>(splitAt i xs ) == (solution17 xs i)

sliceProp :: Int -> Int -> [TestType] ->  Bool
sliceProp (i) (j) xs =  (slice xs i j) ==  (solution18 xs i j)
          where
          slice [] _ _ = Just []
          slice xs k n  | k == n = Just []
                        | k > n || k > length xs ||
                          n > length xs || k < 0 || n < 0 = Nothing
                        | k == 0 = Just (take n xs)
                        | otherwise = Just (drop (k-1) $ take n xs)

rotateProp :: Int -> [TestType] -> Bool
rotateProp i = sameFunctionList (rotate i) (`solution19` i)
           where
               rotate n xs = take (length xs) $ drop (length xs + n) $ cycle xs

removeProp :: Positive Int -> [TestType] -> Property
removeProp (Positive i) xs = i < length xs ==> sameFunction (snd . removeAt i) (`solution20`i) xs
           where
           removeAt n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)
