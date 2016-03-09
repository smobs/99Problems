module Problems.Arithmetic (tests) where

import Solutions.Arithmetic
import Test.Tasty
import Test.Tasty.QuickCheck
import Utils

tests :: TestTree
tests = testGroup "Arithmetic properties" $ reverse
                  [ testProperty "31. is prime" $ (sameFunctionInt tIsPrime solution31)
                  , testProperty "32. Greatest common divisor" $ sameFunctionTwoInt (tGcd) (uncurry solution32)
                  , testProperty "33. Coprime" $ sameFunctionTwoInt (tCoprime) (uncurry solution33)
                  , testProperty "34. Totient-phi" $ sameFunctionInt tTotient solution34 
                  ]

sameFunctionInt :: (Eq a, Show a) => (Int -> a) -> (Int -> a) -> Positive Int -> Bool
sameFunctionInt f h (Positive i) = sameFunction f h i

sameFunctionTwoInt :: (Eq a, Show a) => ((Int, Int) -> a) -> ((Int, Int) -> a) -> (Positive Int, Positive Int)  -> Bool 
sameFunctionTwoInt f1 f2 (Positive x, Positive y) = sameFunction f1 f2 (x, y)

tIsPrime :: Integral a => a -> Bool
tIsPrime i | i < 2 = False 
           | otherwise = all (\x -> (mod i x) /= 0) [2 .. (div i 2)]

tGcd :: (Int, Int) -> Int
tGcd  = uncurry f
     where 
     f a 0 = a
     f a b = f b (mod a b)

tCoprime :: (Int, Int) -> Bool
tCoprime = (1 == ) . tGcd

tTotient :: Int -> Int
tTotient i = length . filter (\x -> tCoprime (i, x)) $ [1 .. i - 1]
