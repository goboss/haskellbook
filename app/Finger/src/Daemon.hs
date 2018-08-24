{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as S
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

type Port = Int

withBoundSocket :: Port -> (Socket -> IO a) -> IO a
withBoundSocket p f =
  withSocketsDo $ do
    addrinfos <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing 
                  (Just (show p))
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr)
                    Stream defaultProtocol
    Network.Socket.bind sock (addrAddress serveraddr)
    f sock

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

handleControlMsg :: Socket -> IO ()
handleControlMsg sock = do
  msg <- readMsg []
  putStrLn "Received:"
  putStrLn (T.unpack (decodeUtf8 msg)) -- TODO: decode msg into Cmd as in the cli interface
    where 
      readMsg acc = do
        buf <- recv sock 1024
        if S.null buf then
          return $ S.concat (reverse acc) -- TODO: use Sequence
        else
          readMsg (buf : acc)

handleControl :: Socket -> IO ()
handleControl sock = forever $ do
  (inSock, _) <- accept sock
  putStrLn "Connection on control socket accepted"
  handleControlMsg inSock
  close inSock

control :: IO ()
control = withBoundSocket 8080 $ \sock -> do
  listen sock 1
  handleControl sock
  close sock

main :: IO ()
main = withBoundSocket 79 $ \sock -> do
  putStrLn "Server is ready to accept connections"
  forkIO control
  listen sock 1
  -- only one connection open at a time
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  close sock
