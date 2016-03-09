module Solutions.Lists where

import           Types
import qualified Data.List as L
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
solution20 l i = (l !! (i-1), uncurry (\ a b -> a++(tail b)) (splitAt (i-1) l))

solution21 :: a -> Int -> [a] -> [a]
solution21 = undefined

solution22 :: Int -> Int -> [Int]
solution22 = undefined

solution23 :: Int -> [a] -> IO [a]
solution23 = undefined

solution24 :: Int -> Int -> IO [Int]
solution24 = undefined

solution25 :: [a] -> IO [a]
solution25 = undefined

solution26 :: Int -> [a] -> [[a]]
solution26 = undefined

solution27a :: [a] -> [[[a]]] 
solution27a = undefined

solution27b :: [Int] -> [a] -> [[[a]]]
solution27b = undefined

solution28a :: [[a]] -> [[a]]
solution28a = undefined

solution28b :: [[a]] -> [[a]]
solution28b = undefined

solution29 = undefined

solution30 = undefined
