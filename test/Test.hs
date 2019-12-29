import Test.Hspec
import Library hiding (main)

main = hspec $ do
  let
    v0 = Or []
    v1 = And []
    v2 = Or [v1, v1]
    v3 = Or [v1, v1, v1]

  describe "calculateSize" $ do
    it "empty Or" $ calculateSize (Or []) `shouldBe` 0
    it "empty And" $ calculateSize (And []) `shouldBe` 1
    it "1 or 1" $ calculateSize (Or [v1, v1]) `shouldBe` 2
    it "or [1,1,1] = 3" $ calculateSize (Or [v1, v1, v1]) `shouldBe` 3
    it "or [1,2] = 3" $ calculateSize (Or [v1, v2]) `shouldBe` 3
    it "and [1,2] = 2" $ calculateSize (And [v1, v2]) `shouldBe` 2
    it "and [2,2] = 4" $ calculateSize (And [v2, v2]) `shouldBe` 4
    it "or [2,3] = 5" $ calculateSize (Or [v2, v3]) `shouldBe` 5
    it "and [2,3] = 6" $ calculateSize (And [v2, v3]) `shouldBe` 6

  describe "enumerate" $ do
    it "empty Or" $ enumerate (Or []) `shouldBe` []
    it "empty And" $ enumerate (And []) `shouldBe` [0]
    it "or [1,1]" $ enumerate (Or [v1,v1]) `shouldBe` [0,1]
    it "or [1,1,1]" $ enumerate (Or [v1,v1,v1]) `shouldBe` [0,1,2]
    it "or [1,2]" $ enumerate (Or [v1,v2]) `shouldBe` [0,1,2]
    it "and [2,3] = 6" $ enumerate (And [v2, v3]) `shouldBe` [0..5]
