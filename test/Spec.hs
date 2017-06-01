module Main where

import Data.List
import Lib
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "combinations" $ do
      it "small case" $ do combinations 1 [1, 2] `shouldBe` [[1], [2]]
      it "choose 0 " $ do
        combinations 0 [1, 2, 3] `shouldBe` ([[]] :: [[Integer]])
      it "basic test" $ do
        combinations 2 [1, 2, 3, 4] `shouldBe`
          ([[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]])
      it "empty list" $ do combinations 5 [] `shouldBe` ([] :: [[Integer]])
      it "out of range number" $ do
        combinations 10 [1, 2, 3] `shouldBe` ([] :: [[Integer]])
      it "choose length of list" $ do
        combinations 4 [1, 2, 3, 4] `shouldBe` [[1, 2, 3, 4]]
      it "big choice" $ do length (combinations 4 [1 .. 10]) `shouldBe` 210
    describe "canonical form" $ do
      it "regarde" $ do canonicalForm "regardé" `shouldBe` "adeegrr"
      it "fete" $ do canonicalForm "fête" `shouldBe` "eeft"
    describe "buildMapping" $ do
      let mapping = buildMapping ["regardé", "regarde", "fête"]
      it "map to multiple" $ do mapping "adeegrr" `shouldBe` ["regarde", "regardé"]
      it "map to single" $ do mapping "eeft" `shouldBe` ["fête"]
      it "map to nothing" $ do mapping "asdfasdf" `shouldBe` ([] :: [String])
