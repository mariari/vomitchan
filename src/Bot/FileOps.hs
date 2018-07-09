{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.FileOps (
  createUsrFldr,
  appendLog,
  dwnUsrFile,
  usrFldrNoLog,
  upUsrFile,
  getUsrFldr,
  getUsrFldrT
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Monoid
import           Bot.MessageType
import           Data.Foldable     (fold)

import           System.Directory
import           Turtle             hiding (FilePath, fold)
import           Data.List
-- FUNCTIONS ----------------------------------------------------------------------------------

-- Creates a folder of the irc channel and a user inside of it
createUsrFldr :: Message -> IO ()
createUsrFldr = createDirectoryIfMissing True . getUsrFldr


--appends the log file for posted links for the user
appendLog :: Message -> Text -> IO ()
appendLog msg = T.appendFile (getUsrFldr msg <> "Links.log")


-- Downloads the requested file to the users path
dwnUsrFile :: MonadIO io => Message -> Text -> io ExitCode
dwnUsrFile msg url = shell ("cd " <> getUsrFldrT msg <>
                            " && curl --max-filesize 104857600 -O " <> url) empty

upUsrFile :: MonadIO m => T.Text -> m T.Text
upUsrFile file = check <$> procStrict "curl" ["-F", "file=@" <> file, "https://0x0.st"] empty
  where check (_,n)
          | T.isPrefixOf "http" n = T.init n
          | otherwise             = ""

-- HELPER FUNCTIONS ---------------------------------------------------------------------------

getUsrFldrT :: Message -> T.Text
getUsrFldrT msg = fold ["./data/logs/", msgChan msg, "/", msgNick msg, "/"]

-- Gets folder path based on Message (Chan <> Nick)
getUsrFldr :: Message -> FilePath
getUsrFldr = T.unpack . getUsrFldrT

-- Lists all the files in the users directory
listUsrFldr :: Message -> IO [FilePath]
listUsrFldr msg = doesDirectoryExist usrfldr >>= lsFldr
  where lsFldr dirp
          | dirp      = listDirectory usrfldr
          | otherwise = return []
        usrfldr = getUsrFldr msg

-- Lists all the files except the .log files
usrFldrNoLog :: Message -> IO [FilePath]
usrFldrNoLog msg = filter (not . isSuffixOf ".log") <$> listUsrFldr msg
