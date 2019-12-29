import Test.Hspec
import Test.QuickCheck
import Library hiding (main)
import qualified Data.Vector as V
import qualified Data.Set as S
import Test.QuickCheck.Instances.Vector ()

mkOr :: [Type] -> Type
mkOr = Or . V.fromList

mkAnd :: [Type] -> Type
mkAnd = And . V.fromList

main = hspec $ do
  let
    v0 = mkOr []
    v1 = mkAnd []
    v2 = mkOr [v1, v1]
    v3 = mkOr [v1, v1, v1]

  describe "calculateSize" $ do
    it "empty or" $ calculateSize (mkOr []) `shouldBe` 0
    it "empty and" $ calculateSize (mkAnd []) `shouldBe` 1
    it "and [or []]" $ calculateSize (mkAnd [mkOr []]) `shouldBe` 0
    it "1 or 1" $ calculateSize (mkOr [v1, v1]) `shouldBe` 2
    it "or [1,1,1] = 3" $ calculateSize (mkOr [v1, v1, v1]) `shouldBe` 3
    it "or [1,2] = 3" $ calculateSize (mkOr [v1, v2]) `shouldBe` 3
    it "and [1,2] = 2" $ calculateSize (mkAnd [v1, v2]) `shouldBe` 2
    it "and [2,2] = 4" $ calculateSize (mkAnd [v2, v2]) `shouldBe` 4
    it "or [2,3] = 5" $ calculateSize (mkOr [v2, v3]) `shouldBe` 5
    it "and [2,3] = 6" $ calculateSize (mkAnd [v2, v3]) `shouldBe` 6
    it "big" $ calculateSize
      (mkOr [mkAnd [],mkOr [mkAnd [mkOr [mkOr [mkOr [mkAnd [mkAnd [mkAnd []],mkOr [mkOr []]]],mkAnd [mkOr []]],mkAnd [mkOr [mkOr [mkOr []]],mkAnd [mkOr []]]]],mkAnd [mkAnd [mkAnd [],mkOr [mkOr [mkOr [],mkOr [mkAnd [mkAnd [mkOr []],mkAnd [mkOr [mkAnd [],mkAnd []],mkAnd [mkAnd []]]],mkAnd [mkAnd [mkOr []]]]],mkOr [mkOr [mkAnd [mkOr []],mkOr [mkOr [mkOr [mkAnd [mkAnd [],mkAnd [mkAnd []]],mkAnd [mkOr [],mkOr [mkAnd []]]]]]],mkAnd [mkAnd [mkAnd [mkAnd [mkAnd [mkOr [mkAnd [mkOr [mkAnd [mkAnd [mkOr [mkAnd []],mkOr [mkAnd [mkAnd [],mkAnd [mkAnd [mkAnd [mkOr []]],mkOr [mkAnd []]]]]]],mkAnd []],mkAnd [mkOr [mkOr [mkAnd [mkAnd [mkAnd [mkAnd [mkAnd [mkOr []]]]]]],mkAnd []]]]]],mkOr [mkAnd [mkAnd [mkAnd [mkAnd []],mkOr [mkAnd []]]]]],mkAnd []]]]]]],mkOr []]]])
      `shouldBe` 1

  let enumerateSet = S.fromList . V.toList . enumerate

  describe "enumerate" $ do
    it "empty or" $ enumerateSet (mkOr []) `shouldBe` S.fromList []
    it "empty and" $ enumerateSet (mkAnd []) `shouldBe` S.fromList [0]
    it "and [or []]" $ enumerateSet (mkAnd [mkOr []]) `shouldBe` S.fromList []
    it "or [1,1]" $ enumerateSet (mkOr [v1,v1]) `shouldBe` S.fromList [0,1]
    it "or [1,1,1]" $ enumerateSet (mkOr [v1,v1,v1]) `shouldBe` S.fromList [0,1,2]
    it "or [1,2]" $ enumerateSet (mkOr [v1,v2]) `shouldBe` S.fromList [0,1,2]
    it "and [2,3] = 6" $ enumerateSet (mkAnd [v2, v3]) `shouldBe` S.fromList [0..5]
    it "and [6,3] = 18" $ enumerateSet (mkAnd [mkAnd [v2, v3], v3]) `shouldBe` S.fromList [0..17]
    it "And [Or [And [And [],Or []]]]" $ enumerateSet (mkAnd [mkOr [mkAnd [mkAnd [],mkOr []]]]) `shouldBe` S.fromList []
    it "big" $ enumerateSet
      (mkOr [mkAnd [],mkOr [mkAnd [mkOr [mkOr [mkOr [mkAnd [mkAnd [mkAnd []],mkOr [mkOr []]]],mkAnd [mkOr []]],mkAnd [mkOr [mkOr [mkOr []]],mkAnd [mkOr []]]]],mkAnd [mkAnd [mkAnd [],mkOr [mkOr [mkOr [],mkOr [mkAnd [mkAnd [mkOr []],mkAnd [mkOr [mkAnd [],mkAnd []],mkAnd [mkAnd []]]],mkAnd [mkAnd [mkOr []]]]],mkOr [mkOr [mkAnd [mkOr []],mkOr [mkOr [mkOr [mkAnd [mkAnd [],mkAnd [mkAnd []]],mkAnd [mkOr [],mkOr [mkAnd []]]]]]],mkAnd [mkAnd [mkAnd [mkAnd [mkAnd [mkOr [mkAnd [mkOr [mkAnd [mkAnd [mkOr [mkAnd []],mkOr [mkAnd [mkAnd [],mkAnd [mkAnd [mkAnd [mkOr []]],mkOr [mkAnd []]]]]]],mkAnd []],mkAnd [mkOr [mkOr [mkAnd [mkAnd [mkAnd [mkAnd [mkAnd [mkOr []]]]]]],mkAnd []]]]]],mkOr [mkAnd [mkAnd [mkAnd [mkAnd []],mkOr [mkAnd []]]]]],mkAnd []]]]]]],mkOr []]]])
      `shouldBe` S.fromList [0]

  describe "calculateSize and enumerate" $ do
    -- it "equal sizes" $ do
    --   labelledExamples $ withMaxSuccess 4 $ property $ \x -> collect x $ fromIntegral (calculateSize x) == length (enumerate x)
    --   True `shouldBe` False
    it "equal sizes" $ withMaxSuccess 3 $ property $ \x -> fromIntegral (calculateSize x) == length (enumerate x)

instance Arbitrary Type where
  arbitrary = oneof [fmap mkOr arbitrary, fmap And arbitrary]
