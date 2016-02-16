module Utils  where

import           Test.Tasty.QuickCheck

type TestType = Int

sameFunction :: (Show a, Eq a, Eq b, Show b, Arbitrary b) => (b -> a) -> (b -> a) -> b -> Bool
sameFunction f1 f2 xs = f1 xs == f2 xs
