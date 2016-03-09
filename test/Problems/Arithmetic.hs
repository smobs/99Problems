module Problems.Arithmetic (tests) where

import Solutions.Arithmetic
import Test.Tasty
import Test.Tasty.QuickCheck
import Utils

tests :: TestTree
tests = testGroup "Arithmetic properties" $ reverse
                  [ testProperty "31. is prime" $ (sameFunctionInt tIsPrime solution31)
                  , testProperty "32. Greatest common divisor" $ sameFunctionTwoInt (tGcd) (uncurry solution32)
                  ]

sameFunctionInt :: (Eq a, Show a) => (Int -> a) -> (Int -> a) -> Int -> Bool
sameFunctionInt = sameFunction

sameFunctionTwoInt :: (Eq a, Show a) => ((Int, Int) -> a) -> ((Int, Int) -> a) -> (Int, Int)  -> Bool 
sameFunctionTwoInt = sameFunction

tIsPrime :: Integral a => a -> Bool
tIsPrime i | i < 2 = False 
           | otherwise = all (\x -> (mod i x) /= 0) [2 .. (div i 2)]

tGcd :: (Int, Int) -> Int
tGcd  = uncurry f
     where 
     f a 0 = a
     f a b = f b (mod a b)
