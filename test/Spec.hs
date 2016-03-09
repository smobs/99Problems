
import qualified Problems.Lists               as Lists
import           Test.Tasty
import           Test.Tasty.Ingredients.Basic
import           Test.Tasty.Options
import qualified Problems.Arithmetic as Arithmetic

main :: IO ()
main = defaultMain (localOption (HideSuccesses True) tests)

tests :: TestTree
tests = testGroup "99 Problems" $ reverse [ Lists.tests
                                          , Arithmetic.tests]
