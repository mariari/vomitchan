{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Database(
  -- * Original API (unchanged signatures)
    addUser
  , addVomit
  , addChannel
  , updateLink
  , updateUserQuantityOfVomits
  , succUserQuantityOfVomits
  , getLink
  , getRandomVomitPath
  , getRouletteVomit
  , nukeVomitByMD5
  , nukeUserFromDb
  , nukeVomitsOfUserFromDb
  , getUserQuantityOfVomits
  , getRandomVomit
  , genDb
  , fixQuantityOfVomits
  , nukeVomitsLinkUserFromDb
  , nukeVomitsMD5UserFromDb
  , updateNSFW
  , updateNSFL
  , checkFilenameMetadata
  -- * Testable variants (take Connection as first arg)
  , createSchema
  , addUserConn
  , addVomitConn
  , addChannelConn
  , updateLinkConn
  , updateUserQuantityOfVomitsConn
  , succUserQuantityOfVomitsConn
  , getLinkConn
  , getRandomVomitPathConn
  , getRouletteVomitConn
  , nukeUserFromDbConn
  , nukeVomitsOfUserFromDbConn
  , getUserQuantityOfVomitsConn
  , getRandomVomitConn
  , fixQuantityOfVomitsConn
  , nukeVomitsLinkUserFromDbConn
  , nukeVomitsMD5UserFromDbFixConn
  , nukeVomitByMD5FixConn
  -- * Types
  , DBUser(..)
  , DBVomit(..)
  , DBChannel(..)
  , Count(..)
  , Username
  , Channel
  -- * Constants
  , dbPath
  ) where

import Database.SQLite.Simple hiding (fold)
import Database.SQLite.Simple.FromField

import Control.Concurrent
import Control.Exception
import Control.Monad

import           Data.Foldable
import           Data.Maybe         (listToMaybe)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import           Turtle       hiding (FilePath, fold)
import qualified Turtle.Bytes as TB

import qualified System.Directory as D

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

-- Constants

dbPath :: FilePath
dbPath = "./data/vomits.db"

-- Helpers
checkFilenameMetadata :: T.Text -> T.Text -> Bool
checkFilenameMetadata = T.isInfixOf

getExtension :: String -> String
getExtension = reverse . takeWhile (/= '.') . reverse

dropExtensions :: String -> String
dropExtensions = reverse . drop 1 . dropWhile (/= '.') . reverse

appendToFilename :: String -> String -> String
appendToFilename path str = let ext      = getExtension path
                                filename = dropExtensions path
                                in filename <> str <> "." <> ext

retryGen :: Integer -> Integer -> IO a -> IO a
retryGen n current action
  | current >= n = throw RetryException
  | otherwise     = action `catch` (\(_ :: SQLError) -> threadDelay 10000 >> retryGen n (current + 1) action)

retry :: IO a -> IO a
retry = retryGen 5 0

-- Schema

createSchema :: Connection -> IO ()
createSchema conn = do
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

-- Db functions

genDb :: IO ()
genDb = do
  dbExists <- D.doesFileExist dbPath
  unless dbExists $ do
    withConnection dbPath $ \conn -> do
      createSchema conn

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

-- Conn variants + wrappers

addChannelConn :: Connection -> Channel -> IO ()
addChannelConn conn chan =
  executeNamed
      conn
      "INSERT OR IGNORE INTO channels (name) VALUES (:cname)"
      [":cname" := chan]

addChannel :: Channel -> IO ()
addChannel chan = retry . withConnection dbPath $
  \conn -> addChannelConn conn chan

addUserConn :: Connection -> Username -> Channel -> IO ()
addUserConn conn user chan = do
  addChannelConn conn chan
  executeNamed
      conn
      "INSERT INTO user (username, channel_id, quantity_of_vomits)\
      \ VALUES (:uname, (SELECT id FROM channels WHERE name=:cname), 0)"
      [":uname" := user, ":cname" := chan]

addUser :: Username -> Channel -> IO ()
addUser user chan = retry . withConnection dbPath $
  \conn -> addUserConn conn user chan

addVomitConn :: Connection -> Username -> Channel -> String -> String -> IO ()
addVomitConn conn nick chan md5 filepath =
  executeNamed
      conn
      "INSERT INTO vomits (filepath, vomit_md5, user_id, link)\
      \ VALUES (:filepath, :md5, (SELECT id FROM user WHERE username=:uname\
      \ AND channel_id=(SELECT id FROM channels where name=:cname)),\
      \ (SELECT link FROM vomits WHERE vomit_md5=:md5))"
      [":filepath" := filepath, ":md5" := md5, ":uname" := nick, ":cname" := chan]

addVomit :: Username -> Channel -> String -> String -> IO ()
addVomit nick chan md5 filepath = retry . withConnection dbPath $
  \conn -> addVomitConn conn nick chan md5 filepath

fixQuantityOfVomitsConn :: Connection -> IO ()
fixQuantityOfVomitsConn conn =
  execute_
      conn
      "UPDATE user SET quantity_of_vomits=(SELECT COUNT(*) FROM vomits WHERE user.id = vomits.user_id)"

fixQuantityOfVomits :: IO ()
fixQuantityOfVomits = retry . withConnection dbPath $
  \conn -> fixQuantityOfVomitsConn conn

updateLinkConn :: Connection -> String -> String -> IO ()
updateLinkConn conn filepath link =
  executeNamed
      conn
      "UPDATE vomits SET link=:link WHERE vomit_md5=(SELECT vomit_md5 FROM vomits\
      \ WHERE filepath=:path)"
      [":link" := link, ":path" := filepath]

updateLink :: String -> String -> IO ()
updateLink filepath link = retry . withConnection dbPath $
  \conn -> updateLinkConn conn filepath link

succUserQuantityOfVomitsConn :: Connection -> Username -> Channel -> IO ()
succUserQuantityOfVomitsConn conn user chan =
  executeNamed
      conn
      "UPDATE user SET quantity_of_vomits = quantity_of_vomits + 1\
      \ WHERE username=:uname\
      \ AND channel_id=(SELECT id FROM channels WHERE name=:cname)"
      [":uname" := user, ":cname" := chan]

succUserQuantityOfVomits :: Username -> Channel -> IO ()
succUserQuantityOfVomits user chan = retry . withConnection dbPath $
  \conn -> succUserQuantityOfVomitsConn conn user chan

updateUserQuantityOfVomitsConn :: Connection -> Username -> Channel -> Int -> IO ()
updateUserQuantityOfVomitsConn conn user chan new =
  executeNamed
      conn
      "UPDATE user SET quantity_of_vomits=:voms\
      \ WHERE username=:uname\
      \ AND channel_id=(SELECT id FROM channels WHERE name=:cname);"
      [":voms" := new, ":uname" := user, ":cname" := chan]

updateUserQuantityOfVomits :: Username -> Channel -> Int -> IO ()
updateUserQuantityOfVomits user chan new = retry . withConnection dbPath $
  \conn -> updateUserQuantityOfVomitsConn conn user chan new

getUserQuantityOfVomitsConn :: Connection -> Username -> Channel -> IO Int
getUserQuantityOfVomitsConn conn username chan = do
  user <- queryNamed
              conn
              "SELECT * FROM user WHERE username=:uname\
              \ AND channel_id=(SELECT id FROM channels WHERE name=:cname)\
              \ LIMIT 1;"
              [":uname" := username, ":cname" := chan] :: IO [DBUser]
  return $ maybe 0 userQuantityVomit (listToMaybe user)

getUserQuantityOfVomits :: Username -> Channel -> IO Int
getUserQuantityOfVomits username chan = retry . withConnection dbPath $
  \conn -> getUserQuantityOfVomitsConn conn username chan

updateNSFW :: String -> Username -> Channel -> IO Int
updateNSFW = updateMetadata "-nsfw"

updateNSFL :: String -> Username -> Channel -> IO Int
updateNSFL = updateMetadata "-nsfl"

updateMetadata :: String -> String -> Username -> Channel -> IO Int
updateMetadata meta link username chan = retry . withConnection dbPath $
  \conn -> do
   vomits <- queryNamed
            conn
            "SELECT * FROM vomits\
            \ WHERE user_id=(SELECT id FROM user WHERE username=:uname\
            \ AND channel_id=(SELECT id FROM channels WHERE name=:cname))\
            \ AND link=:link"
            [":uname" := username, ":cname" := chan, ":link" := link] :: IO [DBVomit]
   let paths = filter (not . checkFilenameMetadata (T.pack meta) . T.pack) (vomitPath <$> vomits)
   thrash <- traverse renameFile paths
   traverse_ (updateFilepath conn) paths
   return $ length thrash

   where
     renameFile :: String -> IO ()
     renameFile path = D.renameFile path (appendToFilename path meta)

     updateFilepath conn path =
       executeNamed
           conn
           "UPDATE vomits SET filepath=:nfp WHERE filepath=:fp"
           [":nfp" := appendToFilename path meta, ":fp" := path]

getLinkConn :: Connection -> String -> IO (Maybe String)
getLinkConn conn filepath = do
  vom <- queryNamed
              conn
              "SELECT * FROM vomits WHERE filepath=:path LIMIT 1;"
              [":path" := filepath] :: IO [DBVomit]
  return $ vomitLink =<< listToMaybe vom

getLink :: String -> IO (Maybe String)
getLink filepath = retry . withConnection dbPath $
  \conn -> getLinkConn conn filepath

getRandomVomitConn :: Connection -> Username -> Channel -> IO [DBVomit]
getRandomVomitConn conn user chan = do
  vom  <- queryNamed
              conn
              "SELECT * FROM vomits\
              \ WHERE user_id=(SELECT id FROM user WHERE username=:uname\
              \ AND channel_id=(SELECT id FROM channels WHERE name=:cname))\
              \ ORDER BY RANDOM() LIMIT 1;"
              [":uname" := user, ":cname" := chan] :: IO [DBVomit]
  return vom

getRandomVomit :: Username-> Channel-> IO [DBVomit]
getRandomVomit user chan = retry . withConnection dbPath $
  \conn -> getRandomVomitConn conn user chan

getRandomVomitPathConn :: Connection -> Username -> Channel -> IO String
getRandomVomitPathConn conn user chan = do
  vom <- queryNamed
              conn
              "SELECT * FROM vomits\
              \ WHERE user_id=(SELECT id FROM user WHERE username=:uname\
              \ AND channel_id=(SELECT id FROM channels WHERE name=:cname))\
              \ ORDER BY RANDOM() LIMIT 1;"
              [":uname" := user, ":cname" := chan] :: IO [DBVomit]
  return $ maybe "" vomitPath (listToMaybe vom)

getRandomVomitPath :: Username-> Channel-> IO String
getRandomVomitPath user chan = retry . withConnection dbPath $
  \conn -> getRandomVomitPathConn conn user chan

getRouletteVomitConn :: Connection -> Channel -> IO String
getRouletteVomitConn conn chan = do
  vom  <- queryNamed
              conn
              "SELECT * FROM vomits\
              \ WHERE user_id IN (SELECT id FROM user\
              \ WHERE channel_id=(SELECT id FROM channels WHERE name=:cname)\
              \ AND quantity_of_vomits>=1)\
              \ ORDER BY RANDOM() LIMIT 1;"
              [":cname" := chan] :: IO [DBVomit]
  return $ maybe "" vomitPath (listToMaybe vom)

getRouletteVomit :: Channel -> IO String
getRouletteVomit chan = retry . withConnection dbPath $
  \conn -> getRouletteVomitConn conn chan


nukeVomitByMD5 :: String -> IO [String]
nukeVomitByMD5 md5 = do
  withNewline <- nukeVomitByMD5Fix $ md5 <> "\n"
  withoutNewline <- nukeVomitByMD5Fix md5
  return $ withNewline <> withoutNewline

nukeVomitByMD5FixConn :: Connection -> String -> IO [String]
nukeVomitByMD5FixConn conn md5 = do
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

  traverse_ (fixVomitCount conn . vomitUserId) voms
  return $ vomitPath <$> voms
  where
    fixVomitCount conn user_id = executeNamed
                                      conn
                                      "UPDATE user SET quantity_of_vomits = quantity_of_vomits - 1\
                                      \ WHERE id=:user_id"
                                      [":user_id" := user_id]

nukeVomitByMD5Fix :: String -> IO [String]
nukeVomitByMD5Fix md5 = retry . withConnection dbPath $
  \conn -> nukeVomitByMD5FixConn conn md5

nukeVomitsLinkUserFromDbConn :: Connection -> T.Text -> Username -> Channel -> IO [String]
nukeVomitsLinkUserFromDbConn conn link user chan = do
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

nukeVomitsLinkUserFromDb :: T.Text -> Username -> Channel -> IO [String]
nukeVomitsLinkUserFromDb link user chan = retry . withConnection dbPath $
  \conn -> nukeVomitsLinkUserFromDbConn conn link user chan

nukeVomitsMD5UserFromDb :: T.Text -> Username -> Channel -> IO [String]
nukeVomitsMD5UserFromDb md5 user chan = do
  withNewline <- nukeVomitsMD5UserFromDbFix (md5 <> "\n") user chan
  withoutNewline <- nukeVomitsMD5UserFromDbFix md5 user chan
  return $ withNewline <> withoutNewline

nukeVomitsMD5UserFromDbFixConn :: Connection -> T.Text -> Username -> Channel -> IO [String]
nukeVomitsMD5UserFromDbFixConn conn md5 user chan = do
  voms <- queryNamed
        conn
        "SELECT * FROM vomits\
        \ WHERE vomit_md5=:md5\
        \ AND user_id=(SELECT id FROM user\
        \ WHERE channel_id=(SELECT id FROM channels where name=:cname)\
        \ AND username=:uname);"
        [":uname" := user, ":md5" := md5, ":cname" := chan] :: IO [DBVomit]

  _ <- executeNamed
        conn
        "DELETE FROM vomits\
        \ WHERE vomit_md5=:md5\
        \ AND user_id=(SELECT id FROM user\
        \ WHERE channel_id=(SELECT id FROM channels where name=:cname)\
        \ AND username=:uname);"
        [":uname" := user, ":md5" := md5, ":cname" := chan]

  return $ vomitPath <$> voms

nukeVomitsMD5UserFromDbFix :: T.Text -> Username -> Channel -> IO [String]
nukeVomitsMD5UserFromDbFix md5 user chan = retry . withConnection dbPath $
  \conn -> nukeVomitsMD5UserFromDbFixConn conn md5 user chan

nukeVomitsOfUserFromDbConn :: Connection -> Username -> Channel -> IO ()
nukeVomitsOfUserFromDbConn conn user chan =
  executeNamed
      conn
      "DELETE FROM vomits\
      \ WHERE user_id=(SELECT id FROM user\
      \ WHERE channel_id=(SELECT id FROM channels WHERE name=:cname)\
      \ AND username=:uname);"
      [":uname" := user, ":cname" := chan]

nukeVomitsOfUserFromDb :: Username -> Channel -> IO ()
nukeVomitsOfUserFromDb user chan = retry . withConnection dbPath $
  \conn -> nukeVomitsOfUserFromDbConn conn user chan

nukeUserFromDbConn :: Connection -> Username -> Channel -> IO ()
nukeUserFromDbConn conn user chan =
  executeNamed
      conn
      "DELETE FROM user\
      \ WHERE username=:uname\
      \ AND channel_id=(SELECT id FROM channels WHERE name=:cname);"
      [":uname" := user, ":cname" := chan]

nukeUserFromDb :: Username -> Channel -> IO ()
nukeUserFromDb user chan = retry . withConnection dbPath $
  \conn -> nukeUserFromDbConn conn user chan
