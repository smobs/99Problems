
import qualified Problems.Lists as Lists
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "99 Problems" $ reverse [Lists.tests]
