{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

--- MODULE DEFINITION ---
module Bot.FileOps (
  createUsrFldr,
  appendLog,
) where


--- IMPORTS ---
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Monoid
import           Bot.MessageType

import           System.Environment
import           System.Directory   as S
import           System.IO
import Data.String

-- FUNCTIONS ---

-- Creates a folder of the irc channel and a user inside of it
createUsrFldr :: Message -> IO ()
createUsrFldr msg = createDirectoryIfMissing True $ getUsrFldr msg

--appends the log file for posted links for the user
appendLog :: Message -> T.Text -> IO ()
appendLog msg = T.appendFile (getUsrFldr msg <> "Links.log") 


-- HELPER FUNCTIONS ---
-- Gets folder path based on Message (Chan <> Nick)
getUsrFldr :: Message -> FilePath
getUsrFldr msg = (fromString . T.unpack) ("./data/logs/" <> msgChan msg <> "/" <> msgNick msg <> "/")
