module ExamplesTest where

import Control.Lens
import Data.Aeson.Lens
import Prismatic
import Test.Tasty
import Test.Tasty.Hspec

jsonExample :: String
jsonExample = "{\"foo\":[{\"bar\":{\"baz\":\"#223344\"}},{\"a\":\"#443322\"}]}"

main :: IO ()
main = defaultMain . testGroup "manual" . pure
  =<< testSpec "tests" spec_lambda

spec_lambda :: Spec
spec_lambda = do
  describe "#223344" $ do
    let c = "#223344"
    it "produces an RGB" $
      c ^? _RGB `shouldBe` Just (RGB 34 51 68)
    it "produces an HSL" $
      c ^? _HSL `shouldBe` Just (HSL 210 (-0.34) 51)

    describe "incrementing red" $
      it "produces the right value" $
        (c & red +~ 1) `shouldBe` "#233344"

    describe "incrementing green" $
      it "produces the right value" $
        (c & green +~ 1) `shouldBe` "#223444"

    describe "incrementing blue" $
      it "produces the right value" $
        (c & blue +~ 1) `shouldBe` "#223345"

  describe "accessing all colors in nested JSON" $ do
    describe "reading" $ do
      it "works with _String" $
        (jsonExample ^.. cosmos . _String . _RGB)
          `shouldBe` [RGB 34 51 68, RGB 68 51 34]

      -- TODO: get this working
      -- it "works without _String" $
      --   (jsonExample ^.. cosmos . _String . _RGB)
      --     `shouldBe` [RGB 34 51 68, RGB 68 51 34]

    -- describe "reading2" $ do
    --   it "works with _String" $
    --     (jsonExample ^.. cosmos . _String . _RGB . colors)
    --       `shouldBe` []

    -- describe "writing" $ do
    --   it "works with _String" $
    --     (jsonExample & deep . _String . _RGB . colors *~ 2)
    --       `shouldBe` ""
