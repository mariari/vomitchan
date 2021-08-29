{-# LANGUAGE OverloadedStrings #-}
module Bot.Database(addUser
                   ,addVomit
                   ,updateLink
                   ,updateUserQuantityOfVomits
                   ,succUserQuantityOfVomits
                   ,getLink
                   ,getRandomVomitPath
                   ,getRouletteVomit
                   ,nukeVomitByMD5
                   ,nukeUserFromDb
                   ,nukeVomitsOfUserFromDb
                   ,getUserQuantityOfVomits
                   ,getRandomVomit) where

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

ifEmpty :: [a] -> b -> (a -> b) -> b
ifEmpty (x:_) _ f = f x
ifEmpty [] def _  = def

addUser :: Username -> Channel -> IO ()
addUser user chan = withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "INSERT INTO user (username, channel_id, quantity_of_vomits)\
        \ VALUES (:uname, (SELECT id FROM channels WHERE name=:cname), 0)"
        [":uname" := user, ":cname" := chan]

addVomit :: Username -> Channel -> String -> String -> IO ()
addVomit nick chan md5 filepath = withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "INSERT INTO vomits (filepath, vomit_md5, user_id, link)\
        \ VALUES (:filepath, :md5, (SELECT id FROM user WHERE username=:uname\
        \ AND channel_id=(SELECT id FROM channels where name=:cname)),\
        \ (SELECT link FROM vomits WHERE vomit_md5=:md5))"
        [":filepath" := filepath, ":md5" := md5, ":uname" := nick, ":cname" := chan]

updateLink :: String -> String -> IO ()
updateLink filepath link = withConnection "./data/vomits.db" $
  \conn -> do
    executeNamed
        conn
        "UPDATE vomits SET link=:link WHERE vomit_md5=(SELECT vomit_md5 FROM vomits\
        \ WHERE filepath=:path)"
        [":link" := link, ":path" := filepath]

decUserQuantityOfVomits :: Username -> Channel -> IO ()
decUserQuantityOfVomits user chan = withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "UPDATE user SET quantity_of_vomits = quantity_of_vomits - 1\
        \ WHERE username=:uname\
        \ AND channel_id=(SELECT id FROM channels WHERE name=:cname)"
        [":uname" := user, ":cname" := chan]

succUserQuantityOfVomits :: Username -> Channel -> IO ()
succUserQuantityOfVomits user chan = withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "UPDATE user SET quantity_of_vomits = quantity_of_vomits + 1\
        \ WHERE username=:uname\
        \ AND channel_id=(SELECT id FROM channels WHERE name=:cname)"
        [":uname" := user, ":cname" := chan]

updateUserQuantityOfVomits :: Username -> Channel -> Int -> IO ()
updateUserQuantityOfVomits user chan new = withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "UPDATE user SET quantity_of_vomits=:voms\
        \ WHERE username=:uname\
        \ AND channel_id=(SELECT id FROM channels WHERE name=:cname);"
        [":voms" := new, ":uname" := user, ":cname" := chan]

getUserQuantityOfVomits :: Username -> Channel -> IO Int
getUserQuantityOfVomits username chan = withConnection "./data/vomits.db" $
  \conn -> do
    user <- queryNamed
                conn
                "SELECT * FROM user WHERE username=:uname\
                \ AND channel_id=(SELECT id FROM channels WHERE name=:cname)\
                \ LIMIT 1;"
                [":uname" := username, ":cname" := chan] :: IO [DBUser]
    return $ ifEmpty user 0 userQuantityVomit

getLink :: String -> IO (Maybe String)
getLink filepath = withConnection "./data/vomits.db" $
  \conn -> do
    vom <- queryNamed
                conn
                "SELECT * FROM vomits WHERE filepath=:path LIMIT 1;"
                [":path" := filepath] :: IO [DBVomit]
    return $ ifEmpty vom Nothing vomitLink

getRandomVomit :: Username-> Channel-> IO [DBVomit]
getRandomVomit user chan = withConnection "./data/vomits.db" $
  \conn -> do
    vom  <- queryNamed
                conn
                "SELECT * FROM vomits\
                \ WHERE user_id=(SELECT id FROM user WHERE username=:uname\
                \ AND channel_id=(SELECT id FROM channels WHERE name=:cname))\
                \ ORDER BY RANDOM() LIMIT 1;"
                [":uname" := user, ":cname" := chan] :: IO [DBVomit]
    return vom

getRandomVomitPath :: Username-> Channel-> IO String
getRandomVomitPath user chan = withConnection "./data/vomits.db" $
  \conn -> do
    vom  <- queryNamed
                conn
                "SELECT * FROM vomits\
                \ WHERE user_id=(SELECT id FROM user WHERE username=:uname\
                \ AND channel_id=(SELECT id FROM channels WHERE name=:cname))\
                \ ORDER BY RANDOM() LIMIT 1;"
                [":uname" := user, ":cname" := chan] :: IO [DBVomit]
    return $ ifEmpty vom "" vomitPath

getRouletteVomit :: Channel -> IO String
getRouletteVomit chan = withConnection "./data/vomits.db" $
  \conn -> do
    vom  <- queryNamed
                conn
                "SELECT * FROM vomits\
                \ WHERE user_id=(SELECT id FROM user\
                \ WHERE channel_id=(SELECT id FROM channels WHERE name=:cname)\
                \ AND quantity_of_vomits>=1 ORDER BY RANDOM() LIMIT 1)\
                \ ORDER BY RANDOM() LIMIT 1;"
                [":cname" := chan] :: IO [DBVomit]
    return $ ifEmpty vom "" vomitPath

nukeVomitByMD5 :: String -> IO [String]
nukeVomitByMD5 md5_orig = withConnection "./data/vomits.db" $
  \conn -> do
    voms <- queryNamed
                conn
                "SELECT * FROM vomits\
                \ WHERE vomit_md5=:md5"
                [":md5" := md5] :: IO [DBVomit]
    _ <- executeNamed
                conn
                "DELETE FROM vomits\
                \ WHERE vomit_md5=:md5"
                [":md5" := md5]

    _ <- traverse (fixVomitCount conn . vomitUserId) voms
    return $ vomitPath <$> voms
    where
      md5 = md5_orig <> "\n"
      fixVomitCount conn user_id = executeNamed
                                        conn
                                        "UPDATE user SET quantity_of_vomits = quantity_of_vomits - 1\
                                        \ WHERE id=:user_id"
                                        [":user_id" := user_id]

nukeVomitsOfUserFromDb :: Username -> Channel -> IO ()
nukeVomitsOfUserFromDb user chan = withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "DELETE FROM vomits\
        \ WHERE user_id=(SELECT id FROM user\
        \ WHERE channel_id=(SELECT id FROM channels WHERE name=:cname)\
        \ AND username=:uname);"
        [":uname" := user, ":cname" := chan]

nukeUserFromDb :: Username -> Channel -> IO ()
nukeUserFromDb user chan = withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "DELETE FROM user\
        \ WHERE username=:uname\
        \ AND channel_id=(SELECT id FROM channels WHERE name=:cname);"
        [":uname" := user, ":cname" := chan]
