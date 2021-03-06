{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader

import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen =
  replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI  :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

shortyAlreadyExists :: TL.Text -> TL.Text
shortyAlreadyExists short =
  TL.concat ["shorty ", short, " already exists!"]

handleGet :: Either R.Reply (Maybe BC.ByteString)
                  -> (TL.Text -> ActionM ())
                  -> ActionM ()
                  -> ActionM ()
handleGet resp found notFound =
  case resp of
    Left reply -> text (TL.pack (show reply))
    Right mbBS -> case mbBS of
      Nothing -> notFound
      Just bs ->
        found tbs
          where tbs :: TL.Text
                tbs = TL.fromStrict (decodeUtf8 bs)

app :: ReaderT R.Connection ScottyM ()
app = do
  rConn <- ask
  lift $ get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _  -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)
        getResp <- liftIO (getURI rConn shorty)
        handleGet getResp found (notFound shorty uri')
          where found :: TL.Text -> ActionM ()
                found sh = html (shortyAlreadyExists sh)
                notFound :: BC.ByteString -> BC.ByteString -> ActionM()
                notFound shorty uri = do
                  resp <- liftIO (saveURI rConn shorty uri)
                  html (shortyCreated resp (BC.unpack shorty))
      Nothing -> text (shortyAintUri uri)
  lift $ get "/:short" $ do
    short <- param "short"
    resp <- liftIO (getURI rConn short)
    handleGet resp found notFound
      where found :: TL.Text -> ActionM ()
            found uri = html (shortyFound uri)
            notFound :: ActionM ()
            notFound = text "uri not found"
main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (runReaderT app rConn)
