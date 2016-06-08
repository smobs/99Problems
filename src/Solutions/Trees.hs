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

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ l r) = leaves l ++ leaves r 

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)                        

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch v l r) = v : internals l ++ internals r

level :: Tree a -> Int -> [a]
level Empty _ = []
level (Branch v  _ _) 1 = [v]
level (Branch _ l r) i = level l (i-1) ++ level r (i-1)

completeBinaryTree :: Int -> Tree Int
completeBinaryTree = completeBinaryTree' 1

completeBinaryTree' x n
  | x > n = Empty
  | otherwise = Branch x (completeBinaryTree' (2*x) n) (completeBinaryTree' ((2*x)+1) n)

layout :: Tree a -> Tree (a, (Int, Int))
layout = fst . layout' 1 1

layout' :: Int -> Int -> Tree a -> (Tree (a, (Int, Int)), Int)
layout' x y Empty = (Empty, x)
layout' x y (Branch a l r) = (Branch (a, (x1, y)) t1 t2, x2)
      where (t1, x1) = layout' x (y+1) l
            (t2, x2) = layout' (x1+1) (y+1) r

tree65 = Branch 'n'
         (Branch 'k'
          (Branch 'c'
           (Branch 'a' Empty Empty)
           (Branch 'e'
            (Branch 'd' Empty Empty)
            (Branch 'g' Empty Empty)
           )
          )
          (Branch 'm' Empty Empty)
         )
         (Branch 'u'
          (Branch 'p'
           Empty
           (Branch 'q' Empty Empty)
          )
          Empty
         )

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Branch _ left right) = max (treeDepth left) (treeDepth right) + 1


evenLayout :: Tree a -> Tree (a, (Int, Int))
evenLayout tree = let depth = treeDepth tree
   in layout'' (2^ depth) 0 tree depth

layout'' x y Empty _ = Empty
layout'' x y (Branch a l r) depth  = Branch (a, (x, y)) t1 t2
  where t1 = layout'' (x - 2^(depth-1)) (y+1) l (depth-1)
        t2 = layout'' (x + 2^(depth-1)) (y+1) r (depth-1)



