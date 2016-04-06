module Solutions.Lists where

import           Types
import qualified Data.List as L
import System.Random
import Control.Applicative

solution1 :: [a] -> a
solution1 = head . reverse

solution2 :: [a] -> [a]
solution2 = reverse . tail . reverse

solution3 :: Int -> [a] -> a
solution3 0 xs = head xs
solution3 n xs = solution3 (n - 1) (tail xs)

solution4 :: [a] -> Int
solution4 = foldr (const (+ 1) ) 0

solution5 :: [a] -> [a]
solution5 = foldl (flip (:)) []

solution6 :: Eq a => [a] -> Bool
solution6 [] = True
solution6 [a] = True
solution6 (h : xs) = (head (reverse xs)) == h && solution6 (init xs)  

solution7 :: NestedList a -> [a]
solution7 (Elem x) = [x] 
solution7 (List xs) = concatMap solution7 xs 


solution8 :: Eq a => [a] -> [a]
solution8 (h1 : rs@(h2 : _)) = if h1 == h2 then solution8 rs else h1 : (solution8 rs)
solution8 h = h

solution9 :: Eq a => [a] -> [[a]]
solution9 arg @ (h : t) =  (takeWhile (== h) arg) : solution9 (dropWhile (== h) arg)
solution9 [] = []

solution10 :: Eq a => [a] -> [(Int, a)]
solution10 = map (\x -> (length x, head x) ) . L.group 

solution11 :: Eq a => [a] -> [ListItem a]
solution11 = map f . solution10
    where f (1, xs) = Single xs
          f p = uncurry Multiple p

solution12 :: [ListItem a] -> [a]
solution12 = concatMap f 
    where f (Multiple i x) = replicate i x 
          f (Single x) = [x]

solution13 :: Eq a => [a] -> [ListItem a]
solution13 [] = []
solution13 (h:t) = f h 1 t
  where
    f h i [] = [(gn h i)]
    f h i (h2:t) 
      | h==h2 = f h (i+1) t  
      | otherwise = gn h i : f h2 1 t
    gn h 1 = Single h
    gn h i = Multiple i h
        

solution14 :: [a] -> [a]
solution14 [] = []
solution14 (h:t) = [h, h] ++ solution14 t

solution15 :: [a] -> Int -> [a]
solution15 xs i = xs <**> (replicate i id) 

solution16 :: [a] -> Int -> [a]
solution16 l x = f l x
     where
       f [] _ = []
       f (h:t) 1 = f t x
       f (h:t) y = h : (f t (y-1))

solution17 :: [a] -> Int -> ([a], [a])
solution17 l n = (take n l, drop n l)

solution18 :: [a] -> Int -> Int -> Maybe [a]
solution18 l s e 
    | (s<=0 || e<=0) = Nothing 
    | (e>(length l)) = Nothing
    | (e<s)          = Nothing
    | otherwise      = Just (take ((e+1)-s) (drop (s-1) l))

solution19 :: [a] -> Int -> [a]
solution19 [] _ = []
solution19 l 0  = l
solution19 (h : t) r 
    | (r > 0)        = solution19 (t++[h]) (r-1)
    | otherwise      = solution19 (h : t) (r + length(h:t))

solution20 :: [a] -> Int -> (a, [a])
solution20 l i = (r , xs ++ rs)
    where
        (xs, (r:rs)) = splitAt (i - 1) l
        
solution21 :: a -> Int -> [a] -> [a]
solution21 n p l = xs ++ [n] ++ rs
     where
        (xs, rs) = splitAt (p - 1) l

solution22 :: Int -> Int -> [Int]
solution22 s e 
        | s == e = [e]
        | otherwise = s : solution22 (s + 1) e
        

solution23 :: Int -> [a] -> IO [a]
solution23 i xs = do
    gen <- newStdGen
    return $ f i xs gen 
    where 
        p :: RandomGen g => g -> [a] -> (a, [a], g)
        p r xs = let (i, r') = randomR (0, length xs - 1) r in
                 let (x', xs') = solution20 xs i in
                 (x', xs', r')
        f :: RandomGen g => Int -> [a] -> g -> [a]
        f 0 _ _ = []
        f i xs gen = let (x, xs', gen') = p gen xs in
                     x : f (i - 1) xs' gen'
                     
solution24 :: Int -> Int -> IO [Int]
solution24 m n = let xs = [1..m]
                 in solution23 n xs 

solution25 :: [a] -> IO [a]
solution25 xs = do 
                 let l = length xs 
                 indices <- solution24 l l 
                 return $ map (\x -> xs !! (x-1)) indices   

solution26 :: Int -> [a] -> [[a]]
solution26 0 _ = [[]]
solution26 k [] = []
solution26 k (h : xs) =
                solution26 k xs ++ map (\y -> (h : y)) (solution26 (k-1) xs)


solution27a :: [a] -> [[[a]]] 
solution27a = undefined

solution27b :: [Int] -> [a] -> [[[a]]]
solution27b = undefined

solution28a :: [[a]] -> [[a]]
solution28a = undefined

solution28b :: [[a]] -> [[a]]
solution28b = undefined
