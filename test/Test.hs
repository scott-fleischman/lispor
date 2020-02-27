import qualified TestOrAndVector
import Test.Hspec

main :: IO ()
main = hspec $ do
  TestOrAndVector.spec
