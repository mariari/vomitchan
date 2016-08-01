{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.FileOps (
  createUsrFldr,
  appendLog,
  dwnUsrFile,
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Monoid
import           Bot.MessageType
import           Data.Foldable     (fold)

import           System.Directory   as S
import           Turtle             hiding (FilePath, fold)
import Data.String
-- FUNCTIONS ----------------------------------------------------------------------------------

-- Creates a folder of the irc channel and a user inside of it
createUsrFldr :: Message -> IO ()
createUsrFldr msg = createDirectoryIfMissing True $ getUsrFldr msg


--appends the log file for posted links for the user
appendLog :: Message -> T.Text -> IO ()
appendLog msg = T.appendFile (getUsrFldr msg <> "Links.log") 


-- Downloads the requested file to the users path
dwnUsrFile :: MonadIO io => Message -> T.Text -> io ExitCode
dwnUsrFile msg url = proc "wget" ["-P", (fromString . getUsrFldr) msg, url] empty

-- HELPER FUNCTIONS ---------------------------------------------------------------------------

-- Gets folder path based on Message (Chan <> Nick)
getUsrFldr :: Message -> FilePath
getUsrFldr msg = (fromString . T.unpack . fold) ["./data/logs/", msgChan msg, "/", msgNick msg, "/"]
