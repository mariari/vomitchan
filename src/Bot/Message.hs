{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.Message (
  respond
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text        as T

import           Bot.Commands
import           Bot.MessageType
import           Bot.FileOps
import           Data.Foldable
import           Turtle          hiding (fold)
--- DATA --------------------------------------------------------------------------------------

-- Lists the type of webpages that are logged
cmdWbPg :: [T.Text]
cmdWbPg = ["http", "ftp"]

cmdPic :: [T.Text]
cmdPic = ["jpg", "png", "jpeg", "gif"]

cmdVid :: [T.Text]
cmdVid = ["webm", "mp4", "flv", "ogv", "wmv", "gifv"]

cmdMus :: [T.Text]
cmdMus = ["flac", "mp3", "tta", "ogg", "wmv"]

cmdMisc :: [T.Text]
cmdMisc = ["pdf"]

cmdAll :: [T.Text]
cmdAll = fold [cmdPic, cmdVid, cmdMus, cmdMisc]

--- FUNCTIONS ---------------------------------------------------------------------------------

-- takes an IRC message and generates the correct response
respond :: T.Text -> T.Text -> IO (Maybe (T.Text, T.Text))
respond msg info
  | "PING"   `T.isPrefixOf` msg = return $ Just ("PONG", T.drop 5 msg)
  | "PRIVMSG" `T.isInfixOf` msg = foldr (\c -> ((>>) . c <*>))
                                        runCmd [cmdFldr, cmdLog, cmdLogFile] $ toMessage msg info
  | otherwise                   = return Nothing

--- LOGGING -----------------------------------------------------------------------------------

-- Logs any links posted and appends them to the users .log file
cmdLog :: Message -> IO ()
cmdLog = traverse_ . appendLog <*> linLn 
  where linLn m = (<> "\n") <$> allLinks m


-- Downloads any file and saves it to the user folder
cmdLogFile :: Message -> IO ()
cmdLogFile = traverse_ . dwnUsrFile <*> allImg
  where allImg m = cmdAll >>= myF (allLinks m)
        myF x y  = filter (y `T.isSuffixOf`) x

cmdFldr :: Message -> IO ()
cmdFldr = createUsrFldr

--- HELPER ------------------------------------------------------------------------------------

-- creates a list of all links
allLinks :: Message -> [T.Text]
allLinks msg = cmdWbPg >>= specWord msg
