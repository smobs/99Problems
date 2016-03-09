module Problems.Lists (tests)
 where

import qualified Data.List             as L
import qualified Data.Set              as S
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
                  , testProperty "20. Remove element at." removeProp
                  , testProperty "21. Insert element at" insertProp
                  , testProperty "22. List all integers in range" rangeProp
                  , testProperty "23. Select x elements at random." randomSelectProp
                  , testProperty "23.2 Elements should be random"  (\(Positive i, xs) -> i < length xs - 1  ==> isNondeterministic (solution23 i xs))
                  , testProperty "24.1 Lotto is a set in range" randomSet
                  , testProperty "24.2 Lotto is random" (\i n -> (isNondeterministic (solution24 n i)))
                  , testProperty "25.1 Same elements" $ sameElementsProp solution25
                  , testProperty "25.2 Perm is random" (\xs -> isNondeterministic (solution25 xs))
                  , testProperty "26. Select committee from list" selectCommitteeProp
                  , testProperty "27a Group 9 workers into 2,3,4 disjoint sets " (\xs -> length xs == 9 ==> sameFunctionList (groupWorkers [2,3,4]) solution27a xs)
                  , testProperty "27b Group workers generally" groupWorkersProp]


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

insertProp :: TestType -> Positive Int -> [TestType] -> Property
insertProp t (Positive i) xs = i < length xs ==> sameFunction ((\(a,b) -> a ++ [t] ++ b)  . L.splitAt i) (solution21 t i)

rangeProp :: Int -> Int -> Property
rangeProp x y = x <= y ==> [x .. y] == solution22 x y

randomSelectProp :: Positive Int -> [TestType] ->  Property
randomSelectProp (Positive i) xs = i < length xs ==>
                    (ioProperty $ do
                          rs <- solution23 i xs
                          return (length rs == i && (all id . map (`L.elem` xs)) rs))

randomSet :: Positive Int -> Positive Int -> Property
randomSet (Positive m) (Positive i) = i < m ==> ioProperty $ do
        s <- S.fromList <$> solution24 m i
        return $ length s == i && all f s
    where f x = x < m

isNondeterministic :: (Eq (t TestType), Foldable t) => IO (t TestType) ->  Property
isNondeterministic  =  expectFailure . deterministic

deterministic :: (Eq (t TestType), Foldable t) =>  IO (t TestType)  -> Property
deterministic f  =
                   ioProperty $ do
                       rs <- f
                       rs' <- f
                       return $ rs == rs'

sameElementsProp :: ([TestType] -> IO [TestType]) -> [TestType] -> Property
sameElementsProp f xs = ioProperty $ do
                 perm <- f xs
                 return $ S.fromList perm == S.fromList xs

selectCommitteeProp :: (Positive Int) -> [TestType] -> Property
selectCommitteeProp (Positive i) xs = i < length xs ==> S.fromList (solution26 i xs) == (S.fromList $ f i xs) 
                     where 
                           f :: Int -> [a] -> [[a]]
                           f i' xs' = [ y:ys | y:xs'' <- L.tails xs'
                                             , ys <- f (i - 1) xs'']
                    

groupWorkersProp :: [Int] -> [TestType] -> Property
groupWorkersProp is xs  = sum is == length xs ==>
                 sameFunction (groupWorkers is) (solution27b is) xs

groupWorkers :: [Int] -> [TestType] -> [[[TestType]]]
groupWorkers [] = const [[]]
groupWorkers (n:ns) = concatMap (uncurry $ (. groupWorkers ns) . map . (:)) . combination n
             where
             combination :: Int -> [a] -> [([a], [a])]
             combination 0 xs     = [([],xs)]
             combination n []     = []
             combination n (x:xs) = ts ++ ds
               where
                 ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
                 ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]
