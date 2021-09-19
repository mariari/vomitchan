{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
                   ,getRandomVomit
                   ,genDb) where

import Database.SQLite.Simple

import Control.Concurrent
import Control.Exception

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

data RetryException = RetryException deriving (Show)
instance Exception RetryException

-- TODO Derive somehow
instance FromRow DBChannel where
  fromRow = DBChannel <$> field <*> field

instance FromRow DBUser where
  fromRow = DBUser <$> field <*> field <*> field <*> field

instance FromRow DBVomit where
  fromRow = DBVomit <$> field <*> field <*> field <*> field <*> field

-- Helpers
ifEmpty :: [a] -> b -> (a -> b) -> b
ifEmpty (x:_) _ f = f x
ifEmpty [] def _  = def

retryGen :: Integer -> Integer -> IO a -> IO a
retryGen n current action
  | current >= n = throw RetryException
  | otherwise     = action `catch` (\(_ :: SomeException) -> threadDelay 10000 >> retryGen n (current + 1) action)

retry :: IO a -> IO a
retry = retryGen 5 0

-- Db functions
genDb :: IO ()
genDb = withConnection "./data/vomits.db" $ \conn -> do
  execute_ conn
           "CREATE TABLE IF NOT EXISTS channels\
           \ (id INTEGER PRIMARY KEY, name text);"
  execute_ conn
           "CREATE TABLE IF NOT EXISTS user\
           \ (id INTEGER PRIMARY KEY,\
           \  username text NOT NULL,\
           \  channel_id INTEGER NOT NULL,\
           \  quantity_of_vomits NOT NULL,\
           \  FOREIGN KEY (channel_id) REFERENCES channels (id));"
  execute_ conn
           "CREATE TABLE IF NOT EXISTS vomits\
           \ (id INTEGER PRIMARY KEY,\
           \  filepath text NOT NULL,\
           \  vomit_md5 text NOT NULL,\
           \  user_id INTEGER NOT NULL,\
           \  link text,\
           \ FOREIGN KEY (user_id) REFERENCES user (id));"

addUser :: Username -> Channel -> IO ()
addUser user chan = retry . withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "INSERT INTO user (username, channel_id, quantity_of_vomits)\
        \ VALUES (:uname, (SELECT id FROM channels WHERE name=:cname), 0)"
        [":uname" := user, ":cname" := chan]

addVomit :: Username -> Channel -> String -> String -> IO ()
addVomit nick chan md5 filepath = retry . withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "INSERT INTO vomits (filepath, vomit_md5, user_id, link)\
        \ VALUES (:filepath, :md5, (SELECT id FROM user WHERE username=:uname\
        \ AND channel_id=(SELECT id FROM channels where name=:cname)),\
        \ (SELECT link FROM vomits WHERE vomit_md5=:md5))"
        [":filepath" := filepath, ":md5" := md5, ":uname" := nick, ":cname" := chan]

updateLink :: String -> String -> IO ()
updateLink filepath link = retry . withConnection "./data/vomits.db" $
  \conn -> do
    executeNamed
        conn
        "UPDATE vomits SET link=:link WHERE vomit_md5=(SELECT vomit_md5 FROM vomits\
        \ WHERE filepath=:path)"
        [":link" := link, ":path" := filepath]

succUserQuantityOfVomits :: Username -> Channel -> IO ()
succUserQuantityOfVomits user chan = retry . withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "UPDATE user SET quantity_of_vomits = quantity_of_vomits + 1\
        \ WHERE username=:uname\
        \ AND channel_id=(SELECT id FROM channels WHERE name=:cname)"
        [":uname" := user, ":cname" := chan]

updateUserQuantityOfVomits :: Username -> Channel -> Int -> IO ()
updateUserQuantityOfVomits user chan new = retry . withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "UPDATE user SET quantity_of_vomits=:voms\
        \ WHERE username=:uname\
        \ AND channel_id=(SELECT id FROM channels WHERE name=:cname);"
        [":voms" := new, ":uname" := user, ":cname" := chan]

getUserQuantityOfVomits :: Username -> Channel -> IO Int
getUserQuantityOfVomits username chan = retry . withConnection "./data/vomits.db" $
  \conn -> do
    user <- queryNamed
                conn
                "SELECT * FROM user WHERE username=:uname\
                \ AND channel_id=(SELECT id FROM channels WHERE name=:cname)\
                \ LIMIT 1;"
                [":uname" := username, ":cname" := chan] :: IO [DBUser]
    return $ ifEmpty user 0 userQuantityVomit

getLink :: String -> IO (Maybe String)
getLink filepath = retry . withConnection "./data/vomits.db" $
  \conn -> do
    vom <- queryNamed
                conn
                "SELECT * FROM vomits WHERE filepath=:path LIMIT 1;"
                [":path" := filepath] :: IO [DBVomit]
    return $ ifEmpty vom Nothing vomitLink

getRandomVomit :: Username-> Channel-> IO [DBVomit]
getRandomVomit user chan = retry . withConnection "./data/vomits.db" $
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
getRandomVomitPath user chan = retry . withConnection "./data/vomits.db" $
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
getRouletteVomit chan = retry . withConnection "./data/vomits.db" $
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
nukeVomitByMD5 md5 = do
  withNewline <- nukeVomitByMD5Fix $ md5 <> "\n"
  withoutNewline <- nukeVomitByMD5Fix md5
  return $ withNewline <> withoutNewline

nukeVomitByMD5Fix :: String -> IO [String]
nukeVomitByMD5Fix md5 = retry . withConnection "./data/vomits.db" $
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
      fixVomitCount conn user_id = executeNamed
                                        conn
                                        "UPDATE user SET quantity_of_vomits = quantity_of_vomits - 1\
                                        \ WHERE id=:user_id"
                                        [":user_id" := user_id]

nukeVomitsOfUserFromDb :: Username -> Channel -> IO ()
nukeVomitsOfUserFromDb user chan = retry . withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "DELETE FROM vomits\
        \ WHERE user_id=(SELECT id FROM user\
        \ WHERE channel_id=(SELECT id FROM channels WHERE name=:cname)\
        \ AND username=:uname);"
        [":uname" := user, ":cname" := chan]

nukeUserFromDb :: Username -> Channel -> IO ()
nukeUserFromDb user chan = retry . withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "DELETE FROM user\
        \ WHERE username=:uname\
        \ AND channel_id=(SELECT id FROM channels WHERE name=:cname);"
        [":uname" := user, ":cname" := chan]
