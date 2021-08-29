{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bot.FileOps (
  createUsrFldr,
  appendLog,
  appendError,
  usrFldrNoLog,
  upUsrFile,
  getUsrFldr,
  getUsrFldrT,
  dwnUsrFileExtension,
  getRandomUsrFldr,
  pathFldrNoLog,
  shredFile
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text                             as T
import qualified Turtle.Bytes                          as TB
import qualified Data.Text.IO                          as T
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.Text.Encoding                    as TE
import qualified Data.Maybe                            as Maybe
import           Control.Monad.Catch
import           Data.Foldable                         (fold)
import qualified Data.Aeson                            as JSON
import qualified Data.Aeson.TH                         as TH
import           GHC.Generics
import           System.Random
import           Control.Monad
import           System.Exit
import           Network.HTTP.Types.Status
import qualified Network.HTTP.Client                   as H
import qualified Network.HTTP.Client.TLS               as HT
import qualified Network.HTTP.Client.MultipartFormData as MFD

import           System.Directory
import           Turtle             hiding (FilePath, fold)
import           Data.List

import Bot.MessageType
import Bot.Database
import Bot.NetworkType
import Bot.Misc (randElemList)

-- TODO:: Instead of working off PrivMsg, work off UserI and Chan/Target instead!

data PomfFormat
  = Pomf {success :: Bool,
    files :: [FileFormat]
  }
  | Fail {
    success :: Bool,
    errorcode :: Int,
    description :: T.Text
  } deriving (Show, Generic)


data FileFormat = Fm {
  hash :: T.Text,
  name :: T.Text,
  url  :: T.Text,
  size :: Int
} deriving (Show, Generic)

instance JSON.FromJSON FileFormat
instance JSON.ToJSON FileFormat

TH.deriveJSON (JSON.defaultOptions { JSON.sumEncoding = JSON.UntaggedValue }) ''PomfFormat

type Extension = Text

-- FUNCTIONS ----------------------------------------------------------------------------------

-- Creates a folder of the irc channel and a user inside of it
createUsrFldr :: PrivMsg -> IO ()
createUsrFldr msg = do
  exists <- doesDirectoryExist . getUsrFldr $ msg
  unless exists $ do
    createDirectoryIfMissing True . getUsrFldr $ msg
    addUser (T.unpack . msgNick $ msg) (T.unpack . msgChan $ msg)

--appends the log file for posted links for the user
appendLog :: PrivMsg -> Text -> IO ()
appendLog msg = T.appendFile (getUsrFldr msg <> "Links.log")

-- appends an error log file for whatever command tripped it
appendError :: String -> BS.ByteString -> IO ()
appendError err txt = T.appendFile "./data/errors.txt" (T.pack err <> " \n" <> TE.decodeUtf8 txt)

shredFile :: FilePath -> IO ()
shredFile path = procStrict "shred" ["-uzn", "64", T.pack path] empty >> return ()

-- | check if file is network banned
isBanned :: InfoPriv -> MD5 -> Bool
isBanned info md5 = md5 `elem` (netBans . network $ info)

dwnUsrFileExtension :: MonadIO io => InfoPriv -> PrivMsg -> Text -> Extension -> io ExitCode
dwnUsrFileExtension info msg url extension = do
  uniqueURL <- uniqueURL url extension
  let url' = escapeUrl url
  let filepath = escapeNick (getUsrFldrT msg) <> uniqueURL
  ret <- shell ("cd "
         -- escapeNick fixes bash errors like cd [czar]
         <> escapeNick (getUsrFldrT msg)
         <> " && curl -fL --max-filesize 104857600 --range 0-104857600 -o "
         <> uniqueURL <> " " <> url') empty
  (_, md5) <- TB.shellStrict (fold ["md5sum ", filepath, " | cut -d ' ' -f 1"]) empty
  liftIO $ ifBanned (T.unpack filepath) (T.stripEnd . TE.decodeUtf8 $ md5) $ do
    liftIO $ do
      addVomit (T.unpack . msgNick $ msg) (T.unpack . msgChan $ msg) (T.unpack . TE.decodeUtf8 $ md5) (T.unpack filepath)
      succUserQuantityOfVomits (T.unpack . msgNick $ msg) (T.unpack . msgChan $ msg)
    return ret

  where
    ifBanned filepath md5 cont
      | isBanned info md5 = shredFile filepath >> liftIO exitSuccess
      | otherwise         = cont

-- | 'currentDate' - gets the current unix time stamp
currentDate :: MonadIO m => m Text
currentDate =
  fmap snd (procStrict "date" ["+%s"] empty)

dropExtension' :: T.Text -> T.Text
dropExtension' file =
  case T.breakOnEnd "." file of
    ("", file)   -> file
    (file, _ext) -> T.init file

uniqueURL :: MonadIO m => Text -> Text -> m Text
uniqueURL url extension = do
  time <- currentDate
  pure $ escapeUrl (T.init time <> "-" <> dropExtension' fileName <> "." <> extension)
  where
    fileName = last (T.splitOn "/" url)


escapeNick :: Text -> Text
escapeNick  = escape "[" . escape "]"

escape str =
  T.intercalate ("\\" <> str) . T.splitOn str

escapeUrl :: Text -> Text
escapeUrl = escape "&"

escapeComma :: Text -> Text
escapeComma = escape ","

-- this probably exists as <|> somehow, but it does not do the proper thing without the lift
(<<|>>) :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
(<<|>>) x y = do
  v <- x
  case v of
    Nothing -> y
    Just v -> pure (Just v)

upUsrFile :: (Alternative m, MonadIO m, MonadThrow m) => Text -> m Text
upUsrFile "" = pure ""
upUsrFile t  = do
  res <- cacheUploader t <<|>> lainUpload t <<|>> w1r3Upload t <<|>> ifyouWorkUpload t
  let link = (Maybe.fromMaybe "" res)
  liftIO $ updateLink (T.unpack t) (T.unpack link)
  pure link

cacheUploader :: MonadIO m => T.Text -> m (Maybe T.Text)
cacheUploader file = do
  cachedLink <- liftIO $ getLink (T.unpack file)
  case cachedLink of
    (Just link) -> checkValidity link
    Nothing     -> return Nothing
  where
    isOK status link
      | status == status200 = Just link
      | otherwise           = Nothing

    checkValidity ""   = return Nothing
    checkValidity link = liftIO $ do
      manager <- liftIO $ H.newManager HT.tlsManagerSettings
      req <- H.parseRequest $ "GET " <> link
      response <- H.httpNoBody req manager
      return $ isOK (H.responseStatus response) (T.pack link)

multiPartFileUpload :: (MonadIO m, MonadThrow m) => T.Text -> String -> T.Text -> m (LBS.ByteString)
multiPartFileUpload input link file = do
  let part = MFD.partFileSource input (T.unpack file)
  manager <- liftIO $ H.newManager HT.tlsManagerSettings
  initialReq <- H.parseRequest link
  req <- MFD.formDataBody [part] initialReq
  msg <- liftIO $ H.httpLbs req manager
  return $ H.responseBody msg

pomfUploader :: (MonadIO m, MonadThrow m) => T.Text -> m (Maybe T.Text)
pomfUploader file = do
  msg <- multiPartFileUpload "files[]" "https://pomf.lain.la/upload.php" file
  pure $
    case JSON.decodeStrict (LBS.toStrict msg) of
      Just Pomf {files = Fm {url} : _} -> Just url
      Just Pomf {files = []}           -> Nothing
      Nothing                          -> Nothing
      Just Fail {}                     -> Nothing

lainUpload :: (MonadIO m, MonadThrow m) => T.Text -> m (Maybe T.Text)
lainUpload = fmap (fmap filesToF) . pomfUploader
  where
    filesToF = T.replace "/files/" "/f/"

ifyouWorkUpload :: (MonadIO m, MonadThrow m) => T.Text -> m (Maybe T.Text)
ifyouWorkUpload = pomfUploader

-- This site is down so w/e
w1r3Upload :: (MonadIO m, MonadThrow m) => T.Text -> m (Maybe T.Text)
w1r3Upload file = check <$> procStrict "curl" ["-F", "upload=@" <> file, "https://w1r3.net"] empty
  where check (_,n)
          | T.isPrefixOf "http" n = Just (T.init n)
          | otherwise             = Nothing


-- HELPER FUNCTIONS ---------------------------------------------------------------------------

getUsrFldrT :: PrivMsg -> T.Text
getUsrFldrT msg = fold ["./data/logs/", msgChan msg, "/", msgNick msg, "/"]

filterDirs :: FilePath -> [FilePath] -> IO [FilePath]
filterDirs base xs = filterM f xs
  where
    f dir = do
      files <- listDirectory (base <> dir)
      return $ (length files) > 1

--Gets a random folder of a user
getRandomUsrFldr :: PrivMsg -> IO FilePath
getRandomUsrFldr msg = do
  dirs <- listDirectory (T.unpack base)
  f_dirs <- filterDirs (T.unpack base) dirs
  seed <- randomRIO (0, 5000000)
  return $ T.unpack (fold [base ,T.pack (randElemList f_dirs seed), "/"])
  where
    base = fold ["./data/logs/", msgChan msg, "/"]

-- Gets folder path based on PrivMsg (Chan <> Nick)
getUsrFldr :: PrivMsg -> FilePath
getUsrFldr = T.unpack . getUsrFldrT

-- Lists all the files in the users directory
listUsrFldr :: FilePath -> IO [FilePath]
listUsrFldr usrfldr = doesDirectoryExist usrfldr >>= lsFldr
  where lsFldr dirp
          | dirp      = listDirectory usrfldr
          | otherwise = return []

-- Lists all the files except the .log files
usrFldrNoLog :: PrivMsg -> IO [FilePath]
usrFldrNoLog = pathFldrNoLog . getUsrFldr

pathFldrNoLog :: FilePath -> IO [FilePath]
pathFldrNoLog path  = filter (not . isSuffixOf ".log") <$> listUsrFldr path
