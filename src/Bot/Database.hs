{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Database(addUser
                   ,addVomit
                   ,addChannel
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
                   ,genDb
                   ,fixQuantityOfVomits
                   ,nukeVomitsLinkUserFromDb) where

import Database.SQLite.Simple hiding (fold)
import Database.SQLite.Simple.FromField

import Control.Concurrent
import Control.Exception
import Control.Monad

import           Data.Foldable
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import           Turtle       hiding (FilePath, fold)
import qualified Turtle.Bytes as TB

import qualified System.Directory as D
import qualified System.Random    as R

type Username = String
type Channel  = String

newtype Count = Count Integer
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
instance FromRow Count where
  fromRow = Count <$> field

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
genDb = do
  dbExists <- D.doesFileExist "./data/vomits.db"
  unless dbExists $ do
    withConnection "./data/vomits.db" $ \conn -> do
      execute_ conn
        "CREATE TABLE IF NOT EXISTS channels\
        \ (id INTEGER PRIMARY KEY, name text, UNIQUE(name));"
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

      directories <- D.listDirectory "./data/logs"
      users_ <- traverse D.listDirectory (fmap ("./data/logs/" <>) directories)
      let users = zip users_ directories
      vomits <- traverse generator ((\(names, chan) -> (\name -> (name, chan, "./data/logs/" <> chan <> "/" <> name)) <$> names) =<< users)

      _ <- traverse addChannel directories
      _ <- traverse usersAdd users
      _ <- traverse vomitAdd vomits
      _ <- traverse fixQuantity vomits
      return ()

      where
        usersAdd :: ([FilePath], FilePath) -> IO ()
        usersAdd (xs, x) = traverse (flip addUser x) xs >> return ()

        vomitAdd :: (FilePath, FilePath, [FilePath]) -> IO ()
        vomitAdd (name, chan, paths) = do
          traverse (\x -> do
                       unless ("Links.log" `T.isSuffixOf` (T.pack x)) $ do
                         (_, md5) <- TB.shellStrict (fold ["md5sum '", T.pack x, "' | cut -d ' ' -f 1"]) empty
                         addVomit name chan (T.unpack . T.stripEnd . TE.decodeUtf8 $ md5) x
                   ) paths >> return ()

        fixQuantity :: (FilePath, FilePath, [FilePath]) -> IO ()
        fixQuantity (name, chan, paths) = updateUserQuantityOfVomits name chan
          (length . filter (not . T.isSuffixOf "Links.log") $ T.pack <$> paths)

        generator :: (FilePath, FilePath, FilePath) -> IO (FilePath, FilePath, [FilePath])
        generator (name, chan, dir) = do
          vomits <- D.listDirectory dir
          return (name, chan, ((("./data/logs/" <> chan <> "/" <> name <> "/") <>) <$> vomits))

addUser :: Username -> Channel -> IO ()
addUser user chan = retry . withConnection "./data/vomits.db" $
  \conn -> do
    addChannel chan
    executeNamed
        conn
        "INSERT INTO user (username, channel_id, quantity_of_vomits)\
        \ VALUES (:uname, (SELECT id FROM channels WHERE name=:cname), 0)"
        [":uname" := user, ":cname" := chan]

fixQuantityOfVomits :: IO ()
fixQuantityOfVomits = retry . withConnection "./data/vomits.db" $
  \conn ->
    execute_
        conn
        "UPDATE user SET quantity_of_vomits=(SELECT COUNT(md5) FROM vomits WHERE user.id = vomit.id)"

addChannel :: Channel -> IO ()
addChannel chan = retry . withConnection "./data/vomits.db" $
  \conn ->
    executeNamed
        conn
        "INSERT OR IGNORE INTO channels (name) VALUES (:cname)"
        [":cname" := chan]

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
    vomOff <- queryNamed
                conn
                "SELECT COUNT(*) FROM vomits\
                \ WHERE user_id=(SELECT id FROM user WHERE username=:uname\
                \ AND channel_id=(SELECT id FROM channels WHERE name=:cname));"
                [":uname" := user, ":cname" := chan] :: IO [Count]
    let (Count maxNum) = ifEmpty vomOff (Count 1) id
    select <- R.randomRIO (0,  maxNum - 1)
    vom <- queryNamed
                conn
                "SELECT * FROM vomits\
                \ WHERE user_id=(SELECT id FROM user WHERE username=:uname\
                \ AND channel_id=(SELECT id FROM channels WHERE name=:cname))\
                \ LIMIT 1 OFFSET :off;"
                [":uname" := user, ":cname" := chan, ":off" := select] :: IO [DBVomit]
    return $ ifEmpty vom "" vomitPath

getRouletteVomit :: Channel -> IO String
getRouletteVomit chan = retry . withConnection "./data/vomits.db" $
  \conn -> do
    vomOff  <- queryNamed
                conn
                "SELECT Count(*) FROM vomits\
                \ WHERE user_id IN (SELECT id FROM user\
                \ WHERE channel_id=(SELECT id FROM channels WHERE name=:cname)\
                \ AND quantity_of_vomits>=1)"
                [":cname" := chan] :: IO [Count]
    let (Count maxNum) = ifEmpty vomOff (Count 1) id
    select <- R.randomRIO (0, maxNum - 1)
    vom  <- queryNamed
                conn
                "SELECT * FROM vomits\
                \ WHERE user_id IN (SELECT id FROM user\
                \ WHERE channel_id=(SELECT id FROM channels WHERE name=:cname)\
                \ AND quantity_of_vomits>=1)\
                \ LIMIT 1 OFFSET :off;"
                [":cname" := chan, ":off" := select] :: IO [DBVomit]
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

nukeVomitsLinkUserFromDb :: T.Text -> Username -> Channel -> IO [String]
nukeVomitsLinkUserFromDb link user chan = retry . withConnection "./data/vomits.db" $
  \conn -> do
    voms <- queryNamed
          conn
          "SELECT * FROM vomits\
          \ WHERE link=:ulink\
          \ AND user_id=(SELECT id FROM user\
          \ WHERE channel_id=(SELECT id FROM channels where name=:cname)\
          \ AND username=:uname);"
          [":uname" := user, ":ulink" := link, ":cname" := chan] :: IO [DBVomit]

    _ <- executeNamed
          conn
          "DELETE FROM vomits\
          \ WHERE link=:ulink\
          \ AND user_id=(SELECT id FROM user\
          \ WHERE channel_id=(SELECT id FROM channels where name=:cname)\
          \ AND username=:uname);"
          [":uname" := user, ":ulink" := link, ":cname" := chan]

    return $ vomitPath <$> voms

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
