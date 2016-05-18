module Trees where
import qualified Data.List as L 
data Tree a = Empty
            | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

cbalTree :: Int -> [Tree Int]
cbalTree n =
    if n < 1
    then [Empty]
    else 
      let left = n `div` 2
          right = n - 1 - left
      in L.nub $ concat [[Branch n l r, Branch n r l] |
          l <- cbalTree  left,
          r <- cbalTree right]
        
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch _ al ar) (Branch _ bl br) =
  mirror al br && mirror ar bl

symmetric :: Tree a -> Bool
symmetric x = mirror x x


binaryTree :: (Ord a, Show a) => [a] -> Tree a
binaryTree [] = Empty
binaryTree (x : xs) = Branch x
                      (binaryTree $ filter (< x) xs)
                      (binaryTree $ filter (> x) xs)

testSymmetric :: (Ord a, Show a) => [a] -> Bool
testSymmetric = symmetric . binaryTree

symCbal :: Int -> [Tree Int]
symCbal = filter symmetric .  cbalTree

hBal :: Int -> [Tree Int]
hBal i
  | i == 0    = [Empty]
  | i < 0     = []
  | otherwise = [Branch i l r
                | (il, ir) <- [(i - 1, i -1), (i-2, i -1), (i-1, i-2)]
                , l <- hBal il
                , r <- hBal ir
                ]

minN :: Int -> Int
minN 0 = 0
minN 1 = 1
minN h =
    let h_1 = minN (h-1)
        h_2 = minN (h-2)
    in
       min (h_1 + h_1 + 1) (h_1 + h_2 + 1) 

maxN :: Int -> Int
maxN 0 = 0
maxN n = 1 + 2 * (maxN (n - 1))

maxH :: Int -> Int
maxH x = L.last [ i | i <- [1..x], minN i <= x ]

hBalN :: Int -> [Tree Int]
hBalN n =
  let h = maxH n
  in filter (\t -> n == countNodes t) $ concat $ map hBal $ L.dropWhile (\x -> n > maxN x) $ [0 .. h]
  where
    countNodes :: Tree a -> Int
    countNodes Empty = 0
    countNodes (Branch _ l r) = 1 + (countNodes l) + (countNodes r)
