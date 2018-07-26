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
import           Data.Foldable
import           Control.Monad.Reader
import qualified Data.Set as S
import           Turtle hiding (fold)

import Bot.Commands
import Bot.MessageType
import Bot.FileOps
import Bot.StateType
import Bot.EffType
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
cmdMisc = ["pdf", "epub", "djvu", "txt", "hs", "lisp", "cpp", "c", "java"]

cmdAll :: [T.Text]
cmdAll = fold [cmdPic, cmdVid, cmdMus, cmdMisc]

cmdAllS :: S.Set Text
cmdAllS = S.fromList cmdAll

--- FUNCTIONS ---------------------------------------------------------------------------------

-- takes an IRC message and generates the correct response
respond :: CmdImp m => T.Text -> m Func
respond txt
      | "PING"   `T.isPrefixOf` txt = return $ Response ("PONG", T.drop 5 txt)
      | "PRIVMSG" `T.isInfixOf` txt = allLogsM >> runCmd
      | otherwise                   = return NoResponse

--- LOGGING -----------------------------------------------------------------------------------

allLogsM :: CmdImp m => m ()
allLogsM = traverse_ toReaderImp [cmdFldr, cmdLog, cmdLogFile]

-- Logs any links posted and appends them to the users .log file
cmdLog :: Message -> IO ()
cmdLog = traverse_ . appendLog <*> linLn
  where linLn = fmap (<> "\n") . allLinks


-- Downloads any file and saves it to the user folder
cmdLogFile :: Message -> IO ()
cmdLogFile = traverse_ . dwnUsrFile <*> allImg
  where allImg m = filter (isSuffix cmdAllS) (allLinks m)

cmdFldr :: Message -> IO ()
cmdFldr = createUsrFldr

--- HELPER ------------------------------------------------------------------------------------

-- creates a list of all links
allLinks :: Message -> [T.Text]
allLinks = (cmdWbPg >>=) . specWord


isSuffix :: S.Set Text -> Text -> Bool
isSuffix legalExtensions txt = extension `S.member` legalExtensions
  where
    (_,extension) = T.breakOnEnd "." txt
