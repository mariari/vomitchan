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
import qualified Data.Maybe         as Maybe
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
appendError err txt = T.appendFile "./data/errors.txt" (T.pack err <> " \n" <> TE.decodeUtf8 txt)

-- Downloads the requested file to the users path
dwnUsrFile :: MonadIO io => PrivMsg -> Text -> io ExitCode
dwnUsrFile msg url = do
  validatedFile <- validateUrl url
  shell ("cd "
         <> getUsrFldrT msg
         <> " && curl -f --max-filesize 104857600 --range 0-104857600 -o "
         <> validatedFile <> " " <> url) empty

validateUrl :: MonadIO m => Text -> m Text
validateUrl url = do
  (_, time) <- procStrict "date" ["+%s"] empty
  pure (T.init time <> "-" <> validatedFileName)
  where
    fileName = last (T.splitOn "/" url)
    validatedFileName
      | T.isSuffixOf ":large" fileName = T.dropEnd (length (":large" :: String)) fileName
      | T.isSuffixOf "#nsfw" fileName  = T.dropEnd (length ("#nsfw" :: String))  fileName
      | otherwise                      = fileName

upUsrFile :: (Alternative m, MonadIO m) => Text -> m Text
upUsrFile t = do
  res <- lainUpload t <|> w1r3Upload t
  pure (Maybe.fromMaybe "" res)

lainUpload :: MonadIO m => T.Text -> m (Maybe T.Text)
lainUpload file =
  fmap filesToF . check
    <$> shellStrict
         (fold ["curl"
               , " -F "
               , "files[]=@"
               , file
               , " https://pomf.lain.la/upload.php"
               , "| jq -c -r \".files[].url\""
               ])
         empty
  where check (_,n)
          | T.isPrefixOf "http" n = Just (T.init n)
          | otherwise             = Nothing
        filesToF = T.replace "/files/" "/f/"

w1r3Upload :: MonadIO m => T.Text -> m (Maybe T.Text)
w1r3Upload file = check <$> procStrict "curl" ["-F", "upload=@" <> file, "https://w1r3.net"] empty
  where check (_,n)
          | T.isPrefixOf "http" n = Just (T.init n)
          | otherwise             = Nothing


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
