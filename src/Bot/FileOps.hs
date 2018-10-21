module Bot.FileOps (
  createUsrFldr,
  appendLog,
  appendError,
  dwnUsrFile,
  usrFldrNoLog,
  upUsrFile,
  getUsrFldr,
  getUsrFldrT
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as TE
import           Data.Foldable     (fold)

import           System.Directory
import           Turtle             hiding (FilePath, fold)
import           Data.List

import Bot.MessageType

-- TODO:: Instead of working off PrivMsg, work off UserI and Chan/Target instead!

-- FUNCTIONS ----------------------------------------------------------------------------------

-- Creates a folder of the irc channel and a user inside of it
createUsrFldr :: PrivMsg -> IO ()
createUsrFldr = createDirectoryIfMissing True . getUsrFldr


--appends the log file for posted links for the user
appendLog :: PrivMsg -> Text -> IO ()
appendLog msg = T.appendFile (getUsrFldr msg <> "Links.log")

-- appends an error log file for whatever command tripped it
appendError :: String -> BS.ByteString -> IO ()
appendError err txt = T.appendFile ("./data/errors.txt") (T.pack err <> " \n" <> TE.decodeUtf8 txt)

-- Downloads the requested file to the users path
dwnUsrFile :: MonadIO io => PrivMsg -> Text -> io ExitCode
dwnUsrFile msg url = shell ("cd " <> getUsrFldrT msg <>
                            " && curl --max-filesize 104857600 --range 0-104857600 -O " <> url) empty

upUsrFile :: MonadIO m => T.Text -> m T.Text
upUsrFile file = check <$> procStrict "curl" ["-F", "file=@" <> file, "https://0x0.st"] empty
  where check (_,n)
          | T.isPrefixOf "http" n = T.init n
          | otherwise             = ""

-- HELPER FUNCTIONS ---------------------------------------------------------------------------

getUsrFldrT :: PrivMsg -> T.Text
getUsrFldrT msg = fold ["./data/logs/", msgChan msg, "/", msgNick msg, "/"]

-- Gets folder path based on PrivMsg (Chan <> Nick)
getUsrFldr :: PrivMsg -> FilePath
getUsrFldr = T.unpack . getUsrFldrT

-- Lists all the files in the users directory
listUsrFldr :: PrivMsg -> IO [FilePath]
listUsrFldr msg = doesDirectoryExist usrfldr >>= lsFldr
  where lsFldr dirp
          | dirp      = listDirectory usrfldr
          | otherwise = return []
        usrfldr = getUsrFldr msg

-- Lists all the files except the .log files
usrFldrNoLog :: PrivMsg -> IO [FilePath]
usrFldrNoLog msg = filter (not . isSuffixOf ".log") <$> listUsrFldr msg
