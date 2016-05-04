module Solutions.Logic 
where

import qualified Data.List as L
  
foo = "hlel"

and2 :: Bool -> Bool -> Bool
and2 True  True = True
and2 _ _ = False

infixl 4 ||||

(||||) = or2 

or2 False False = False
or2 _ _ = True

nand2 True True = False
nand2 _ _ = True

nor2 False False = True
nor2 _ _ = False

xor2 True False  = True 

xor2 False True  = True
xor2 _ _  = False

impl True False = False
impl _ _ = True

equiv :: Bool -> Bool -> Bool
equiv = (==)


table :: (Bool -> Bool -> Bool) -> [[Bool]]
table f =  [[a , b , f a b] | a <- [True, False], b <- [True, False]]

printTable :: [[Bool]] -> IO ()
printTable = mapM_ (\x -> putStrLn  (show x) ) 

truthTable :: (Bool -> Bool -> Bool) -> IO ()
truthTable = printTable . table 

perm 0 = [[]]
perm n = (map (\x -> True: x) (perm (n-1)))
               ++
           (map (\x -> False: x) (perm (n-1)))
           
tableN :: Int -> ([Bool] -> Bool) -> [[Bool]]
tableN n f =
    map (\x -> x ++ [f x]) (perm n) 

grey :: Int -> [[Bool]]
grey 0 = [[]]
grey n = (map (\x -> [True] ++ x) g ) ++ (map (\x -> [False] ++ x) (reverse g))
      where
         g = grey (n-1) 


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

huffman :: [(Char, Int)] -> [(Char, [Bool])]
huffman  = L.sortBy (\(x,_) (y,_) -> compare x y) . unsorted
    where 
           unsorted = flip generateCode [] .  huffman' . sortHuffman . map (\(x, i) -> (Leaf x, i))

huffman' :: [(Tree Char, Int)] -> Tree Char
huffman' [(x, _)] = x
huffman' ((x, u):(y, v):xs) = let newPair = (Node x y, u +v) in
  huffman' . sortHuffman $ (newPair : xs)

sortHuffman =  L.sortBy (\(_, i) (_,j) -> compare i j)

generateCode :: Tree Char  -> [Bool] -> [(Char, [Bool])]
generateCode (Leaf x) resultSoFar =[(x, resultSoFar)]
generateCode (Node x y) resultsSoFar  = generateCode x (resultsSoFar ++ [False]) ++ generateCode y (resultsSoFar ++ [True]) 
