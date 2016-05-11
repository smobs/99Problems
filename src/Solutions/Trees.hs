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

