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
          let screenNames = (^. TLuserEntityUserScreenName) <$> mentions

          screenNames `shouldBe` ["ltxbot", "passy"]

    describe "stripEntities" $ do
        it "should remove leading entities from a string" $ do
            let text = "not a test"
            let indices = [[0, 3]]

            stripEntities indices text `shouldBe` "a test"

        it "should remove all entities from a string" $ do
            let text = "this is a test string"
            let indices = [[5, 7], [8, 8], [9, 9], [14, 20]]

            stripEntities indices text `shouldBe` "this test"
