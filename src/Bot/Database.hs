{-# LANGUAGE OverloadedStrings #-}
module Bot.Database(addUser
                   ,addVomit
                   ,updateLink
                   ,getRandomVomitPath
                   ,getRouletteVomit) where

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


addUser :: Username -> Channel -> IO ()
addUser user chan = withConnection "./data/vomits.db" $
  \conn -> executeNamed conn "INSERT INTO user (username, channel_id, quantity_of_vomits) VALUES (:uname, (SELECT id FROM channels WHERE name=:cname), 0)" [":uname" := user, ":cname" := chan]

addVomit :: Username -> Channel -> String -> String -> IO ()
addVomit nick chan md5 filepath = withConnection "./data/vomits.db" $
  \conn -> executeNamed conn "INSERT INTO vomits (filepath, vomit_md5, user_id) VALUES (:filepath, :md5, (SELECT id FROM user WHERE username=:uname AND channel_id=(SELECT id FROM channels where name=:cname)))" [":filepath" := filepath, ":md5" := md5, ":uname" := nick, ":cname" := chan]

updateLink :: String -> String -> IO ()
updateLink filepath link = withConnection "./data/vomits.db" $
  \conn -> executeNamed conn "UPDATE vomits SET link=:link WHERE filepath=:path" [":link" := link, ":path" := filepath]

getRandomVomitPath :: Username-> Channel-> IO String
getRandomVomitPath user chan = withConnection "./data/vomits.db" $ \conn -> do
  vom  <- queryNamed conn "SELECT * FROM vomits WHERE user_id=(SELECT id FROM user WHERE username=:uname AND channel_id=(SELECT id FROM channels WHERE name=:cname)) ORDER BY RANDOM() LIMIT 1;" [":uname" := user, ":cname" := chan] :: IO [DBVomit]
  return . vomitPath . head $ vom

getRouletteVomit :: Channel -> IO String
getRouletteVomit chan = withConnection "./data/vomits.db" $ \conn -> do
  vom  <- queryNamed conn "SELECT * FROM vomits WHERE user_id=(SELECT id FROM user WHERE channel_id=(SELECT id FROM channels WHERE name=:cname) AND quantity_of_vomits>=1 ORDER BY RANDOM() LIMIT 1) ORDER BY RANDOM() LIMIT 1;" [":cname" := chan] :: IO [DBVomit]
  return . vomitPath . head $ vom
