{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Finger.User where

import           Control.Exception

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import           Data.Typeable

import           Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite

import           Text.RawString.QQ

data User =
  User {
    username      :: Text
  , shell         :: Text
  , homeDirectory :: Text
  , realName      :: Text
  , phone         :: Text
  } deriving (Eq, Show)

data UserUpdate =
  UserUpdate {
    newShell         :: Maybe Text
  , newHomeDirectory :: Maybe Text
  , newRealName      :: Maybe Text
  , newPhone         :: Maybe Text
  } deriving (Eq, Show)

type UserName = Text

data Cmd = 
    UserAdd User
  | UserMod UserName UserUpdate
  | UserDel UserName
  deriving (Eq, Show)
  
instance FromRow User where
  fromRow = User 
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field

instance ToRow User where
  toRow User{..} =
    toRow (username, shell, homeDirectory, realName, phone)

createUsers :: Query
createUsers = [r|
  CREATE TABLE IF NOT EXISTS users (
    username TEXT UNIQUE,
    shell TEXT, 
    homeDirectory TEXT,
    realName TEXT,
    phone TEXT
  )
|]

insertUser :: Query
insertUser =
  "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?)"

updateUser :: Query
updateUser = [r|
  UPDATE users 
  SET username = ?, shell = ?, homeDirectory = ?, realName = ?, phone = ?
  WHERE username = ?
|]

deleteUser :: Query
deleteUser = "DELETE FROM users WHERE username = ?"

allUsers :: Query
allUsers =
  "SELECT * from users"

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where meRow :: UserRow
        meRow =
          ( "callen", "/bin/zsh",
            "/home/callen", "Chris Allen",
            "555-123-4567")

formatUser :: User -> ByteString
formatUser User{..} =
  BS.concat
  [ "Login: ", e username, "\t\t\t\t",
    "Name: ", e realName, "\n",
    "Directory: ", e homeDirectory, "\t\t\t",
    "Shell: ", e shell, "\n"]
  where e = encodeUtf8

withDb :: (Connection -> IO a) -> IO a
withDb = withConnection "finger.db"

userAdd :: User -> IO ()
userAdd user = withDb (\conn -> do
  execute conn insertUser (toRow user)
  putStrLn ("User " ++ T.unpack (username user) ++ " has been added"))

userDel :: UserName -> IO ()
userDel name = withDb (\conn -> do
  users <- query conn getUserQuery (Only name)
  case users :: [User] of
    [_] -> execDelete conn
    []  -> putStrLn("No such user " ++ T.unpack name)
    _   -> throwIO DuplicateData
  )
  where execDelete :: Connection -> IO ()
        execDelete conn = do
          execute conn deleteUser (Only name)
          putStrLn ("User " ++ T.unpack name ++ " has been deleted")

userMod :: UserName -> UserUpdate -> IO ()
userMod name update = withDb(\conn -> do
  users <- query conn getUserQuery (Only name)
  case users of
    [user] -> execUpdate conn user update 
    []     -> putStrLn("No such user " ++ T.unpack name)
    _      -> throwIO DuplicateData
  )
  where execUpdate :: Connection -> User -> UserUpdate -> IO ()
        execUpdate conn User{..} UserUpdate{..} = do
          let row = toRow 
                ( username
                , fromMaybe shell newShell
                , fromMaybe homeDirectory newHomeDirectory
                , fromMaybe realName newRealName
                , fromMaybe phone newPhone
                , username
                )
          execute conn updateUser row
          putStrLn ("User " ++ T.unpack username ++ " has been updated")

executeCommand :: Cmd -> IO ()
executeCommand cmd =
  case cmd of
    UserAdd user   -> userAdd user
    UserMod n upd  -> userMod n upd
    UserDel n      -> userDel n
          