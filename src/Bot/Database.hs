{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Database(
  -- * Types
    Count(..)
  , DBChannel(..)
  , DBUser(..)
  , DBVomit(..)
  , Username
  , Channel
  -- * Connection helpers
  , withDb
  -- * Constants
  , dbPath
  -- * Helpers
  , checkFilenameMetadata
  -- * Schema
  , createSchema
  -- * Migration
  , genDb
  -- * Channels
  , addChannelConn
  -- * Users
  , addUserConn
  , addUser
  , getUserQuantityOfVomitsConn
  , getUserQuantityOfVomits
  , succUserQuantityOfVomitsConn
  , succUserQuantityOfVomits
  , updateUserQuantityOfVomitsConn
  , updateUserQuantityOfVomits
  , fixQuantityOfVomitsConn
  , nukeUserFromDbConn
  -- * Vomits
  , addVomitConn
  , addVomit
  , getLinkConn
  , getLink
  , updateLinkConn
  , updateLink
  , getRandomVomitConn
  , getRandomVomitPathConn
  , getRandomVomitPath
  , getRouletteVomitConn
  , getRouletteVomit
  , nukeVomitByMD5FixConn
  , nukeVomitByMD5
  , nukeVomitsLinkUserFromDbConn
  , nukeVomitsLinkUserFromDb
  , nukeVomitsMD5UserFromDbFixConn
  , nukeVomitsMD5UserFromDb
  , nukeVomitsOfUserFromDbConn
  -- * Metadata
  , updateNSFW
  , updateNSFL
  ) where

import Database.SQLite.Simple hiding (fold)
import Database.SQLite.Simple.FromField

import Control.Concurrent
import Control.Exception
import Control.Monad

import           Data.Foldable
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.String        (IsString)
import qualified Data.Text          as T
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as TE

import           Turtle       hiding (FilePath, fold)
import qualified Turtle.Bytes as TB

import qualified System.Directory as D

--- TYPES ---------------------------------------------------------------------------

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

--- CONSTANTS -----------------------------------------------------------------------

dbPath :: FilePath
dbPath = "./data/vomits.db"

--- HELPERS -------------------------------------------------------------------------

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

withDb :: (Connection -> IO a) -> IO a
withDb = retry . withConnection dbPath

withNewlineHack :: (IsString s, Semigroup s) => (s -> IO [a]) -> s -> IO [a]
withNewlineHack f md5 = (<>) <$> f (md5 <> "\n") <*> f md5

--- SCHEMA --------------------------------------------------------------------------

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

--- MIGRATION -----------------------------------------------------------------------

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

      traverse_ addChannel directories
      traverse_ usersAdd users
      traverse_ vomitAdd vomits
      traverse_ fixQuantity vomits

      where
        usersAdd :: ([FilePath], FilePath) -> IO ()
        usersAdd (xs, x) = traverse_ (flip addUser x) xs

        vomitAdd :: (FilePath, FilePath, [FilePath]) -> IO ()
        vomitAdd (name, chan, paths) =
          traverse_ (\x -> do
                       unless ("Links.log" `T.isSuffixOf` (T.pack x)) $ do
                         (_, md5Out) <- TB.procStrict "md5sum" [T.pack x] empty
                         let md5 = T.stripEnd . TE.decodeUtf8 . BS.takeWhile (/= 0x20) $ md5Out
                         addVomit name chan (T.unpack md5) x
                   ) paths

        fixQuantity :: (FilePath, FilePath, [FilePath]) -> IO ()
        fixQuantity (name, chan, paths) = updateUserQuantityOfVomits name chan
          (length . filter (not . T.isSuffixOf "Links.log") $ T.pack <$> paths)

        generator :: (FilePath, FilePath, FilePath) -> IO (FilePath, FilePath, [FilePath])
        generator (name, chan, dir) = do
          vomits <- D.listDirectory dir
          return (name, chan, ((("./data/logs/" <> chan <> "/" <> name <> "/") <>) <$> vomits))

--- CHANNELS ------------------------------------------------------------------------

addChannelConn :: Connection -> Channel -> IO ()
addChannelConn conn chan =
  executeNamed
      conn
      "INSERT OR IGNORE INTO channels (name) VALUES (:cname)"
      [":cname" := chan]

addChannel :: Channel -> IO ()
addChannel chan = withDb $ \conn -> addChannelConn conn chan

--- USERS ---------------------------------------------------------------------------

addUserConn :: Connection -> Username -> Channel -> IO ()
addUserConn conn user chan = do
  addChannelConn conn chan
  executeNamed
      conn
      "INSERT INTO user (username, channel_id, quantity_of_vomits)\
      \ VALUES (:uname, (SELECT id FROM channels WHERE name=:cname), 0)"
      [":uname" := user, ":cname" := chan]

addUser :: Username -> Channel -> IO ()
addUser user chan = withDb $ \conn -> addUserConn conn user chan

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
getUserQuantityOfVomits username chan = withDb $ \conn -> getUserQuantityOfVomitsConn conn username chan

succUserQuantityOfVomitsConn :: Connection -> Username -> Channel -> IO ()
succUserQuantityOfVomitsConn conn user chan =
  executeNamed
      conn
      "UPDATE user SET quantity_of_vomits = quantity_of_vomits + 1\
      \ WHERE username=:uname\
      \ AND channel_id=(SELECT id FROM channels WHERE name=:cname)"
      [":uname" := user, ":cname" := chan]

succUserQuantityOfVomits :: Username -> Channel -> IO ()
succUserQuantityOfVomits user chan = withDb $ \conn -> succUserQuantityOfVomitsConn conn user chan

updateUserQuantityOfVomitsConn :: Connection -> Username -> Channel -> Int -> IO ()
updateUserQuantityOfVomitsConn conn user chan new =
  executeNamed
      conn
      "UPDATE user SET quantity_of_vomits=:voms\
      \ WHERE username=:uname\
      \ AND channel_id=(SELECT id FROM channels WHERE name=:cname);"
      [":voms" := new, ":uname" := user, ":cname" := chan]

updateUserQuantityOfVomits :: Username -> Channel -> Int -> IO ()
updateUserQuantityOfVomits user chan new = withDb $ \conn -> updateUserQuantityOfVomitsConn conn user chan new

fixQuantityOfVomitsConn :: Connection -> IO ()
fixQuantityOfVomitsConn conn =
  execute_
      conn
      "UPDATE user SET quantity_of_vomits=(SELECT COUNT(*) FROM vomits WHERE user.id = vomits.user_id)"

fixQuantityOfVomits :: IO ()
fixQuantityOfVomits = withDb fixQuantityOfVomitsConn

nukeUserFromDbConn :: Connection -> Username -> Channel -> IO ()
nukeUserFromDbConn conn user chan =
  executeNamed
      conn
      "DELETE FROM user\
      \ WHERE username=:uname\
      \ AND channel_id=(SELECT id FROM channels WHERE name=:cname);"
      [":uname" := user, ":cname" := chan]

nukeUserFromDb :: Username -> Channel -> IO ()
nukeUserFromDb user chan = withDb $ \conn -> nukeUserFromDbConn conn user chan

--- VOMITS --------------------------------------------------------------------------

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
addVomit nick chan md5 filepath = withDb $ \conn -> addVomitConn conn nick chan md5 filepath

getLinkConn :: Connection -> String -> IO (Maybe String)
getLinkConn conn filepath = do
  vom <- queryNamed
              conn
              "SELECT * FROM vomits WHERE filepath=:path LIMIT 1;"
              [":path" := filepath] :: IO [DBVomit]
  return $ vomitLink =<< listToMaybe vom

getLink :: String -> IO (Maybe String)
getLink filepath = withDb $ \conn -> getLinkConn conn filepath

updateLinkConn :: Connection -> String -> String -> IO ()
updateLinkConn conn filepath link =
  executeNamed
      conn
      "UPDATE vomits SET link=:link WHERE vomit_md5=(SELECT vomit_md5 FROM vomits\
      \ WHERE filepath=:path)"
      [":link" := link, ":path" := filepath]

updateLink :: String -> String -> IO ()
updateLink filepath link = withDb $ \conn -> updateLinkConn conn filepath link

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
getRandomVomit user chan = withDb $ \conn -> getRandomVomitConn conn user chan

getRandomVomitPathConn :: Connection -> Username -> Channel -> IO String
getRandomVomitPathConn conn user chan =
  maybe "" vomitPath . listToMaybe <$> getRandomVomitConn conn user chan

getRandomVomitPath :: Username-> Channel-> IO String
getRandomVomitPath user chan = withDb $ \conn -> getRandomVomitPathConn conn user chan

getRouletteVomitConn :: Connection -> Channel -> IO (String, String)
getRouletteVomitConn conn chan = do
  result <- queryNamed
              conn
              "SELECT v.filepath, u.username FROM vomits v\
              \ JOIN user u ON v.user_id = u.id\
              \ WHERE u.channel_id=(SELECT id FROM channels WHERE name=:cname)\
              \ AND u.quantity_of_vomits>=1\
              \ ORDER BY RANDOM() LIMIT 1;"
              [":cname" := chan] :: IO [(String, String)]
  return $ fromMaybe ("", "") (listToMaybe result)

getRouletteVomit :: Channel -> IO (String, String)
getRouletteVomit chan = withDb $ \conn -> getRouletteVomitConn conn chan

nukeVomitByMD5 :: String -> IO [String]
nukeVomitByMD5 = withNewlineHack nukeVomitByMD5Fix

nukeVomitsWhere :: Connection -> Query -> [NamedParam] -> IO [DBVomit]
nukeVomitsWhere conn whereClause params = do
  voms <- queryNamed conn ("SELECT * FROM vomits WHERE " <> whereClause) params
  _ <- executeNamed conn ("DELETE FROM vomits WHERE " <> whereClause) params
  return voms

nukeVomitByMD5FixConn :: Connection -> String -> IO [String]
nukeVomitByMD5FixConn conn md5 = do
  voms <- nukeVomitsWhere conn "vomit_md5=:md5" [":md5" := md5]
  traverse_ (fixVomitCount conn . vomitUserId) voms
  return $ vomitPath <$> voms
  where
    fixVomitCount conn' user_id = executeNamed
                                      conn'
                                      "UPDATE user SET quantity_of_vomits = quantity_of_vomits - 1\
                                      \ WHERE id=:user_id"
                                      [":user_id" := user_id]

nukeVomitByMD5Fix :: String -> IO [String]
nukeVomitByMD5Fix md5 = withDb $ \conn -> nukeVomitByMD5FixConn conn md5

nukeVomitsLinkUserFromDbConn :: Connection -> T.Text -> Username -> Channel -> IO [String]
nukeVomitsLinkUserFromDbConn conn link user chan =
  fmap vomitPath <$> nukeVomitsWhere conn
    "link=:ulink\
    \ AND user_id=(SELECT id FROM user\
    \ WHERE channel_id=(SELECT id FROM channels where name=:cname)\
    \ AND username=:uname);"
    [":uname" := user, ":ulink" := link, ":cname" := chan]

nukeVomitsLinkUserFromDb :: T.Text -> Username -> Channel -> IO [String]
nukeVomitsLinkUserFromDb link user chan = withDb $ \conn -> nukeVomitsLinkUserFromDbConn conn link user chan

nukeVomitsMD5UserFromDb :: T.Text -> Username -> Channel -> IO [String]
nukeVomitsMD5UserFromDb md5 user chan =
  withNewlineHack (\m -> nukeVomitsMD5UserFromDbFix m user chan) md5

nukeVomitsMD5UserFromDbFixConn :: Connection -> T.Text -> Username -> Channel -> IO [String]
nukeVomitsMD5UserFromDbFixConn conn md5 user chan =
  fmap vomitPath <$> nukeVomitsWhere conn
    "vomit_md5=:md5\
    \ AND user_id=(SELECT id FROM user\
    \ WHERE channel_id=(SELECT id FROM channels where name=:cname)\
    \ AND username=:uname);"
    [":uname" := user, ":md5" := md5, ":cname" := chan]

nukeVomitsMD5UserFromDbFix :: T.Text -> Username -> Channel -> IO [String]
nukeVomitsMD5UserFromDbFix md5 user chan = withDb $ \conn -> nukeVomitsMD5UserFromDbFixConn conn md5 user chan

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
nukeVomitsOfUserFromDb user chan = withDb $ \conn -> nukeVomitsOfUserFromDbConn conn user chan

--- METADATA ------------------------------------------------------------------------

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
