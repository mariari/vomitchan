{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.Message (
  respond
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text        as T
import           Data.Monoid

import           Bot.Commands
import           Bot.MessageType
import           Bot.FileOps
--- DATA --------------------------------------------------------------------------------------

-- Lists the type of webpages that are logged
cmdWbPg :: [T.Text]
cmdWbPg = ["http", "ftp"]

--- FUNCTIONS ---------------------------------------------------------------------------------


-- takes an IRC message and generates the correct response
respond :: T.Text -> IO (Maybe (T.Text, T.Text))
respond msg
  | "PING"   `T.isPrefixOf` msg = return $ Just ("PONG", T.drop 5 msg)
  | "PRIVMSG" `T.isInfixOf` msg = cmdLog con >> runCmd con
  | otherwise                   = return Nothing
  where con = toMessage msg

--- LOGGING -----------------------------------------------------------------------------------

-- Logs any links posted and appends them to the users .log file
cmdLog :: Message -> IO ()
cmdLog msg = createUsrFldr msg >> appLogs >> return ()
  where allLinks = cmdWbPg >>= specWord msg           -- creates a [T.Text] list with all links
        linksLn = (<> "\n") <$> allLinks              -- Appends a \n to links
        appLogs  = mapM_ (appendLog msg) linksLn
