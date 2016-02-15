module Types
where
import           Test.Tasty.QuickCheck

data NestedList a = Elem a | List [NestedList a] deriving (Show, Eq, Ord)

instance Arbitrary a => Arbitrary (NestedList a) where
         arbitrary = oneof
                      [ Elem <$> arbitrary
                      , List <$> arbitrary
                      ]
