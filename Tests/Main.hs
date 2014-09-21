{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Fixtures (testStatus)

import Control.Applicative ((<$>))
import Control.Lens.Operators ((^.))
import Web.Twitter.LtxBot (extractStatusMentions, stripEntities)

import qualified Web.Twitter.Types.Lens as TL


main :: IO ()
main = hspec $ do
    describe "extractStatusMentions" $ do
        it "should extract all mentions" $ do
          let mentions = extractStatusMentions testStatus
          let screenNames = (\m -> m ^. TL.userEntityUserScreenName) <$> mentions

          screenNames `shouldBe` ["ltxbot", "passy"]

    describe "stripEntities" $ do
        it "should remove entities from a string" $ do
            let text = "this is a test string"
            let indices = [[5, 6], [8, 8], [14, 20]]

            stripEntities indices text `shouldBe` "this test"
