{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.Message (
  respond
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text as T
import           Control.Concurrent.STM
import qualified Data.ByteString as BS
import           Data.Foldable
import           Control.Monad.Reader
import qualified Data.Set as S
import           Turtle hiding (fold)

import Bot.Commands
import Bot.MessageType
import Bot.FileOps
import Bot.StateType
import Bot.EffType
import Bot.NetworkType
--- DATA --------------------------------------------------------------------------------------

-- Lists the type of webpages that are logged
cmdWbPg :: [T.Text]
cmdWbPg = ["http", "ftp"]

cmdPic :: [T.Text]
cmdPic = ["jpg", "png", "jpeg", "gif", "jpg:large", "png:large", "jpeg:large"]

cmdVid :: [T.Text]
cmdVid = ["webm", "mp4", "flv", "ogv", "wmv", "gifv"]

cmdMus :: [T.Text]
cmdMus = ["flac", "mp3", "tta", "ogg", "wma"]

cmdMisc :: [T.Text]
cmdMisc = ["pdf", "epub", "djvu", "txt", "hs", "lisp", "cpp", "c", "java", "rs"]

cmdAll :: [T.Text]
cmdAll = fold [cmdPic, cmdVid, cmdMus, cmdMisc]

cmdAllS :: S.Set Text
cmdAllS = S.fromList cmdAll

--- FUNCTIONS ---------------------------------------------------------------------------------

-- takes an IRC message and generates the correct response
respond :: BS.ByteString -> AllServers -> Either String Command -> Server -> VomState -> IO Func
respond s _    (Left err)   server state = appendError err s >> print err >> return NoResponse
respond _ allS (Right priv) server state = f priv
  where
    f (PING (Ping s)) = return $ Response ("PONG", s)
    f (PRIVMSG priv)  = runReaderT (allLogsM >> runCmd) (Info priv server state allS)
    f _               = return NoResponse

--- LOGGING -----------------------------------------------------------------------------------

allLogsM :: CmdImp m => m ()
allLogsM = traverse_ (toReaderImp . (. message)) [cmdFldr, cmdLog, cmdLogFile]

-- Logs any links posted and appends them to the users .log file
cmdLog :: PrivMsg -> IO ()
cmdLog = traverse_ . appendLog <*> linLn
  where linLn = fmap (<> "\n") . allLinks


-- Downloads any file and saves it to the user folder
cmdLogFile :: PrivMsg -> IO ()
cmdLogFile = traverse_ . dwnUsrFile <*> allImg
  where allImg m = filter (isSuffix cmdAllS) (allLinks m)

cmdFldr :: PrivMsg -> IO ()
cmdFldr = createUsrFldr

--- HELPER ------------------------------------------------------------------------------------

-- creates a list of all links
allLinks :: PrivMsg -> [T.Text]
allLinks = (cmdWbPg >>=) . specWord


isSuffix :: S.Set Text -> Text -> Bool
isSuffix legalExtensions txt = extension `S.member` legalExtensions
  where
    (_,extension) = T.breakOnEnd "." txt
