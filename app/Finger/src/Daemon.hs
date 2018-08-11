{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated =
        T.concat $
        intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc name = do
  maybeUser <- getUser dbConn (T.strip name)
  case maybeUser of
    Nothing -> do
      putStrLn
        ("Couldn't find matching user\
        \ for username: "
        ++ show name)
      return ()
    Just user ->
      sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name ->
      returnUser dbConn soc
      (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  close soc

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo
                (Just (defaultHints
                {addrFlags = [AI_PASSIVE]}))
                Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
          Stream defaultProtocol
  Network.Socket.bind sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  close sock
