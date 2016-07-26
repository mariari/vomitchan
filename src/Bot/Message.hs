{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION ---
module Bot.Message (
  respond
) where


--- IMPORTS ---
import qualified Data.Text        as T

import           Bot.Commands
import           Bot.MessageType
import           Data.Monoid
import           Bot.FileOps
--- DATA ---

-- Lists the type of webpages that are logged
cmdWbPg :: [T.Text]
cmdWbPg = ["http", "ftp"]

--- FUNCTIONS ---


-- takes an IRC message and generates the correct response
respond :: T.Text -> IO (Maybe (T.Text, T.Text))
respond msg
  | "PING"   `T.isPrefixOf` msg = return $ Just ("PONG", T.drop 5 msg)
  | "PRIVMSG" `T.isInfixOf` msg = cmdLog con >> return (runCmd con)
  | otherwise                   = return Nothing
  where con = toMessage msg

-- Logs any links posted and appends them to the users .log file
cmdLog :: Message -> IO ()
cmdLog msg = createUsrFldr msg >> appLogs >> return ()
  where allLinks = concat [xs | xs <- flip (drpMsgRec msg) " " <$> cmdWbPg, not $ T.null (head xs)] -- creates a [T.Text]
        linksLn = (<> "\n") <$> allLinks
        appLogs  = mapM_ (appendLog msg) linksLn
