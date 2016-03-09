module Problems.Arithmetic (tests) where

import Solutions.Arithmetic
import Test.Tasty
import Test.Tasty.QuickCheck
import Utils

tests :: TestTree
tests = testGroup "Arithmetic properties" 
                  [testProperty "31. is prime" $ (sameFunctionInt tIsPrime solution31)]

sameFunctionInt :: (Eq a, Show a) => (Int -> a) -> (Int -> a) -> Int -> Bool
sameFunctionInt = sameFunction

tIsPrime :: Integral a => a -> Bool
tIsPrime i | i < 2 = False 
           | otherwise = all (\x -> (mod i x) /= 0) [2 .. (div i 2)]
