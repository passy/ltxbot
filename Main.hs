{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Common (runTwitterFromEnv')
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import System.Environment (getArgs)
import Web.Twitter.Conduit (stream, statusesFilterByTrack)
import Web.Twitter.Types (StreamingAPI(..))
import Web.Twitter.Types.Lens (AsStatus(..), userScreenName)

main :: IO ()
main = do
    [confFile] <- getArgs
    -- TODO: Instead of passing around conf, could
    -- I wrap all this in a ReaderT?
    conf <- Conf.load [Conf.Required confFile]
    username <- Conf.lookupDefault "" conf "userName"

    runTwitterFromEnv' conf $ do
        src <- stream $ statusesFilterByTrack username
        src C.$$+- CL.mapM_ (^! act (liftIO . printTL))

    return ()

printTL ::
    StreamingAPI ->
    IO ()
printTL (SStatus s) = T.putStrLn $ showStatus s
printTL _ = T.putStrLn "Other event"

showStatus :: AsStatus s => s -> T.Text
showStatus s = T.concat [ s ^. user . userScreenName
                        , ":"
                        , s ^. text
                        ]
