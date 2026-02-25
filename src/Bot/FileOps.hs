{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  shredFile,
  nekoUpload,
  pomfSecretUpload,
  uploaderFor
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as TE
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe           as Maybe
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.TH        as TH
import           Data.List
import           Data.Foldable        (fold)
import           Data.Maybe           (fromMaybe)

import           Turtle       hiding (FilePath, fold)
import qualified Turtle.Bytes as TB

import           Control.Monad
import           Control.Monad.Catch

import           GHC.Generics

import           System.Random
import           System.Exit
import           System.Directory

import           Network.HTTP.Types.Status
import qualified Network.HTTP.Client                   as H
import qualified Network.HTTP.Client.TLS               as HT
import qualified Network.HTTP.Client.MultipartFormData as MFD

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

shouldDownload :: T.Text -> Bool
shouldDownload msg = not (T.isPrefixOf "*cut*" msg) && not (T.isPrefixOf ".set-" msg)

dwnUsrFileExtension :: MonadIO io => InfoPriv -> PrivMsg -> Text -> Extension -> io ExitCode
dwnUsrFileExtension info msg url extension
  | shouldDownload (msgContent msg) == False = return ExitSuccess
  | otherwise = do
      uniqueURL <- uniqueURL (msgContent msg) url extension
      let filepath = getUsrFldrT msg <> uniqueURL
      ret <- proc "curl"
                  ["-fL", "--max-filesize", "504857600", "--range", "0-104857600"
                  , "-o", filepath, url] empty
      (_, md5Out) <- TB.procStrict "md5sum" [filepath] empty
      let md5 = T.stripEnd . TE.decodeUtf8 . BS.takeWhile (/= 0x20) $ md5Out
      liftIO $ ifBanned (T.unpack filepath) md5 $ do
        liftIO $ do
          addVomit (T.unpack . msgNick $ msg) (T.unpack . msgChan $ msg) (T.unpack md5) (T.unpack filepath)
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

uniqueURL :: MonadIO m => Text -> Text -> Text -> m Text
uniqueURL msg url extension = do
  time <- currentDate
  pure $ T.init time <> "-" <> dropExtension' fileName <> nsfwEnable <> nsflEnable <> "." <> extension
  where
    fileName = T.takeWhileEnd (/= '/') url
    nsfwEnable = metadataEnable "nsfw"
    nsflEnable = metadataEnable "nsfl"

    metadataEnable str
      | str `elem` (T.words . T.toLower $ msg) = "-" <> str
      | otherwise                              = ""

-- this probably exists as <|> somehow, but it does not do the proper thing without the lift
(<<|>>) :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
(<<|>>) x y = do
  v <- x
  case v of
    Nothing -> y
    Just v -> pure (Just v)

upUsrFile :: (Alternative m, MonadIO m, MonadThrow m, MonadCatch m) => (T.Text -> H.Manager -> m (Maybe T.Text)) -> H.Manager -> Text -> m Text
upUsrFile _ _ "" = pure ""
upUsrFile upload manager t  = do
  res <-  cacheUploader t manager
    <<|>> upload t manager
  let link = (Maybe.fromMaybe "" res)
  liftIO $ updateLink (T.unpack t) (T.unpack link)
  pure link

cacheUploader :: (MonadIO m, MonadCatch m) => T.Text -> H.Manager -> m (Maybe T.Text)
cacheUploader file manager = catch work (\ (_ :: SomeException) -> pure Nothing)
  where
  work = do
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
          req <- H.parseRequest $ "GET " <> link
          response <- H.httpNoBody req manager
          return $ isOK (H.responseStatus response) (T.pack link)

filePart :: T.Text -> MFD.Part
filePart file = MFD.partFileSource "files[]" (T.unpack file)

secretPart :: Maybe T.Text -> [MFD.Part]
secretPart = maybe [] (\s -> [MFD.partBS "secret" (TE.encodeUtf8 s)])

uploaderPart :: T.Text -> [MFD.Part]
uploaderPart name = [MFD.partBS "uploader" (TE.encodeUtf8 name)]

postParts :: (MonadIO m, MonadThrow m) => String -> [MFD.Part] -> H.Manager -> m LBS.ByteString
postParts url parts manager = do
  initialReq <- H.parseRequest url
  req <- MFD.formDataBody parts initialReq
  H.responseBody <$> liftIO (H.httpLbs req manager)

decodePomfUrl :: LBS.ByteString -> Maybe T.Text
decodePomfUrl msg = case JSON.decodeStrict (LBS.toStrict msg) of
  Just Pomf {files = Fm {url} : _} -> Just url
  _                                -> Nothing

pomfUpload :: (MonadIO m, MonadThrow m, MonadCatch m) => [MFD.Part] -> String -> T.Text -> H.Manager -> m (Maybe T.Text)
pomfUpload extras url file manager = catch work (\(_ :: SomeException) -> pure Nothing)
  where
  work = decodePomfUrl <$> postParts url (filePart file : extras) manager

pomfUploader :: (MonadIO m, MonadThrow m, MonadCatch m) => T.Text -> String -> H.Manager -> m (Maybe T.Text)
pomfUploader file url = pomfUpload [] url file

pomfSecretUpload :: (MonadIO m, MonadThrow m, MonadCatch m) => Maybe T.Text -> T.Text -> T.Text -> T.Text -> H.Manager -> m (Maybe T.Text)
pomfSecretUpload secret uploader url =
  pomfUpload (secretPart secret <> uploaderPart uploader) (T.unpack ("POST " <> url))

catboxUpload :: (MonadIO m, MonadThrow m) => Maybe T.Text -> T.Text -> H.Manager -> m (Maybe T.Text)
catboxUpload secret file _ =
  check <$> procStrict "curl" (
    ["-F", "reqtype=fileupload", "-F", "fileToUpload=@" <> file]
    <> maybe [] (\s -> ["-F", "userhash=" <> s]) secret
    <> ["https://catbox.moe/user/api.php"]
  ) empty
  where check (_,n)
          | T.isPrefixOf "http" n = Just n
          | otherwise             = Nothing

uploaderFor :: (MonadIO m, MonadThrow m, MonadCatch m) => IRCNetwork -> T.Text -> T.Text -> H.Manager -> m (Maybe T.Text)
uploaderFor net nick = case netUpload net of
  Nothing  -> \f m -> catboxUpload Nothing f m
  Just cfg -> case uploadService cfg of
    "catbox" -> catboxUpload (uploadSecret cfg)
    "neko"   -> \f m -> nekoUpload f m
    "lain"   -> \f m -> lainUpload f m
    _        -> pomfSecretUpload (uploadSecret cfg) nick (uploadUrl cfg)

nekoUpload :: (MonadIO m, MonadThrow m, MonadCatch m) => T.Text -> H.Manager -> m (Maybe T.Text)
nekoUpload file = pomfUploader file "https://img.neko.airforce/upload.php"

lainUpload :: (MonadIO m, MonadThrow m, MonadCatch m) => T.Text -> H.Manager -> m (Maybe T.Text)
lainUpload t = fmap (fmap filesToF) . pomfUploader t "https://pomf.lain.la/upload.php"
  where
    filesToF = T.replace "/files/" "/f/"

ifyouWorkUpload :: (MonadIO m, MonadThrow m, MonadCatch m) => T.Text -> H.Manager -> m (Maybe T.Text)
ifyouWorkUpload = lainUpload

-- This site is down so w/e
w1r3Upload :: (MonadIO m, MonadThrow m) => T.Text -> H.Manager -> m (Maybe T.Text)
w1r3Upload file _ = check <$> procStrict "curl" ["-F", "upload=@" <> file, "https://w1r3.net"] empty
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
