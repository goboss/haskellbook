{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Exception
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Text.RawString.QQ

data User =
  User {
    username      :: Text
  , shell         :: Text
  , homeDirectory :: Text
  , realName      :: Text
  , phone         :: Text
  } deriving (Eq, Show)

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
