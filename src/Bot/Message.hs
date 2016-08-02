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
cmdPic = ["jpg", "png", "jpeg", "webm", "gif", "mp4", "flv", "ogv", "wmv", "gifv"]

--- FUNCTIONS ---------------------------------------------------------------------------------


-- takes an IRC message and generates the correct response
respond :: T.Text -> IO (Maybe (T.Text, T.Text))
respond msg
  | "PING"   `T.isPrefixOf` msg = return $ Just ("PONG", T.drop 5 msg)
  | "PRIVMSG" `T.isInfixOf` msg = cmdLog con >> cmdLogPic con >> runCmd con
  | otherwise                   = return Nothing
  where con = toMessage msg

--- LOGGING -----------------------------------------------------------------------------------

-- Logs any links posted and appends them to the users .log file
cmdLog :: Message -> IO ()
cmdLog msg = createUsrFldr msg >> appLogs >> return ()
  where linksLn  = (<> "\n") <$> allLinks msg
        appLogs  = traverse_ (appendLog msg) linksLn


cmdLogPic :: MonadIO io => Message -> io [ExitCode]
cmdLogPic msg = sequenceA $ dwnUsrFile msg <$> allImg
  where allImg  = cmdPic >>= myF (allLinks msg)
        myF x y = filter (y `T.isSuffixOf`) x


--- HELPER ------------------------------------------------------------------------------------

-- creates a list of all links
allLinks :: Message -> [T.Text]
allLinks msg = cmdWbPg >>= specWord msg
