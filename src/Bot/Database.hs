{-# LANGUAGE OverloadedStrings #-}
module Bot.Database where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

type Username = String
type Channel  = String

data DBChannel = DBChannel { chanId   :: Int
                           , chanName :: String
                           } deriving (Show, Eq)

data DBUser = DBUser { userId            :: Int
                     , userName          :: String
                     , userChannelId     :: Int
                     , userQuantityVomit :: Int
                     } deriving (Show, Eq)

data DBVomit = DBVomit { vomitId     :: Int
                       , vomitPath   :: String
                       , vomitMD5    :: String
                       , vomitUserId :: Int
                       , vomitLink   :: Maybe String
                       } deriving (Show, Eq)

-- TODO Derive somehow
instance FromRow DBChannel where
  fromRow = DBChannel <$> field <*> field

instance FromRow DBUser where
  fromRow = DBUser <$> field <*> field <*> field <*> field

instance FromRow DBVomit where
  fromRow = DBVomit <$> field <*> field <*> field <*> field <*> field

addVomit :: Username -> Channel -> String -> String -> IO ()
addVomit nick chan md5 filepath = do
  conn <- open "./data/vomits.db"
  executeNamed conn "INSERT INTO vomits (filepath, vomit_md5, user_id) VALUES (:filepath, :md5, (SELECT id FROM user WHERE username=:uname AND channel_id=(SELECT id FROM channels where name=:cname)))" [":filepath" := filepath, ":md5" := md5, ":uname" := nick, ":cname" := chan]
  close conn

updateLink :: String -> String -> IO ()
updateLink filepath link = do
  conn <- open "./data/vomits.db"
  executeNamed conn "UPDATE vomits SET link=:link WHERE filepath=:path" [":link" := link, ":path" := filepath]
  close conn

getRandomVomitPath :: Username-> Channel-> IO String
getRandomVomitPath user chan = do
  conn <- open "./data/vomits.db"
  vom  <- queryNamed conn "SELECT * FROM vomits WHERE user_id=(SELECT id FROM user WHERE username=:uname AND channel_id=(SELECT id FROM channels WHERE name=:cname)) ORDER BY RANDOM() LIMIT 1;" [":uname" := user, ":cname" := chan] :: IO [DBVomit]
  close conn
  return . vomitPath . head $ vom

getRouletteVomit :: Channel -> IO String
getRouletteVomit chan = do
  conn <- open "./data/vomits.db"
  vom  <- queryNamed conn "SELECT * FROM vomits WHERE user_id=(SELECT id FROM user WHERE channel_id=(SELECT id FROM channels WHERE name=:cname) AND quantity_of_vomits>=1 ORDER BY RANDOM() LIMIT 1) ORDER BY RANDOM() LIMIT 1;" [":cname" := chan] :: IO [DBVomit]
  close conn
  return . vomitPath . head $ vom
