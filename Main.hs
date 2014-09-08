{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE

import Common (runTwitterFromEnv')
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Data.ByteString (ByteString)
import Data.Conduit (ResumableSource)
import Network.HTTP.Conduit (parseUrl, withManager, http, urlEncodedBody, Request, Response)
import System.Environment (getArgs)
import Web.Authenticate.OAuth (OAuth(..), Credential, signOAuth)
import Web.Twitter.Conduit (stream, userstream)
import Web.Twitter.Types (StreamingAPI(..))
import Web.Twitter.Types.Lens (AsStatus(..), userScreenName)

statusesUrl :: String
statusesUrl = "https://api.twitter.com/1.1/statuses/update.json"

main :: IO ()
main = do
    [confFile] <- getArgs
    conf <- Conf.load [Conf.Required confFile]

    runTwitterFromEnv' conf $ do
        src <- stream userstream
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

postTweet ::
    OAuth ->
    Credential ->
    T.Text ->
    IO (Response (ResumableSource (ResourceT IO) ByteString))
postTweet oauth cred tweet = do
    let params = [("status", TE.encodeUtf8 tweet)]
    req <- makeRequest statusesUrl params
    -- Error handling?
    executeOAuthRequest oauth cred req

makeRequest ::
    MonadThrow m =>
    String ->
    [(ByteString, ByteString)] ->
    m Request
makeRequest url params = do
    req <- parseUrl url
    return $ urlEncodedBody params req

executeOAuthRequest ::
    OAuth ->
    Credential ->
    Request ->
    IO (Response (ResumableSource (ResourceT IO) ByteString))
executeOAuthRequest oauth cred request =
    withManager $ \manager -> do
        signed <- signOAuth oauth cred request
        http signed manager
