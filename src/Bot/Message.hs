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
cmdPic = ["jpg", "png", "jpeg"]

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
  where allLinks =  cmdWbPg >>= specWord msg           -- creates a [T.Text] list with all links
        linksLn  = (<> "\n") <$> allLinks              -- Appends a \n to links
        appLogs  = traverse_ (appendLog msg) linksLn


cmdLogPic :: MonadIO io => Message -> io [ExitCode]
cmdLogPic msg = sequenceA $ dwnUsrFile msg <$> allImg
  where allLinks = cmdWbPg >>= specWord msg
        allImg   = cmdPic >>= myF allLinks
        myF x y  = filter (y `T.isSuffixOf`) x
