{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Fixtures (testStatus)

import Control.Applicative ((<$>))
import Control.Lens.Operators ((^.))
import Web.Twitter.LtxBot (extractStatusMentions)

import qualified Web.Twitter.Types.Lens as TL


main :: IO ()
main = hspec $ do
    describe "extractStatusMentions" $ do
        it "should extract all mentions" $ do
          let mentions = extractStatusMentions testStatus
          let screenNames = (\m -> m ^. TL.userEntityUserScreenName) <$> mentions

          screenNames `shouldBe` ["ltxbot", "passy"]
