{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

--- MODULE DEFINITION ---
module Bot.Commands (
  runCmd,
) where


--- IMPORTS ---
import qualified Data.Text        as T
import qualified Data.Text.Format as T
import qualified Data.Text.IO     as T

import           Bot.MessageType


--- TYPES ---

-- type of all command functions
type CmdFunc = Message -> IO (Maybe (T.Text, T.Text))


--- DATA ---

-- list of admins allowed to use certain commands
-- TODO: Load this from config file
admins :: [T.Text]
admins = ["MrDetonia", "loli"]

-- list of prefixes and corresponding command functions
-- TODO: if the cmdList has over 50~ commands, put it into a hash table instead
cmdList :: [ (T.Text, CmdFunc)]
cmdList =  [ (".bots", cmdBots)
           , (".quit", cmdQuit)
           , (".lewd ", cmdLewd)]


-- FUNCTIONS ---

-- returns a corresponding command function from a message
runCmd :: CmdFunc
runCmd msg = foldr testFunc (return Nothing) cmdList
  where
    testFunc (p, cmd) k
      | p `T.isPrefixOf` msgContent msg = cmd msg
      | otherwise                       = k

--- COMMAND FUNCTIONS ---

-- print bot info
cmdBots :: CmdFunc
cmdBots = composeMsg " :I am a queasy bot written in Haskell | https://gitla.in/MrDetonia/vomitchan"

-- quit
cmdQuit :: CmdFunc
cmdQuit msg
  | msgUser msg `elem` admins = return $ Just ("QUIT", ":Exiting")
  | otherwise                 = return Nothing

-- lewd someone (rip halpybot)
cmdLewd :: CmdFunc
cmdLewd msg = composeMsg (" :\01ACTION lewds " `T.append` T.drop 6 (msgContent msg) `T.append` "\01") msg

-- TODO: add a *vomits* function that grabs random images/links from the channel that it's from and produces rainbow text before and after

-- TODO: add a *cheek pinch* function that puts the bot into reality mode

-- TODO: make reality mode make vomitchan only speak in nods

-- TODO: Reality/*dame* that posts quotes of not moving on and staying locked up
-- TODO: Slumber/*dame* that posts quotes of escapeism

-- TODO: add a *zzz* function that causes the bot go into slumber mode

--- HELPER FUNCTIONS ---

-- figures out where to send a response to
msgDest :: Message -> T.Text
msgDest msg
  | "#" `T.isPrefixOf` msgChan msg = msgChan msg
  | otherwise                      = msgNick msg

-- composes the format that the final send message will be
composeMsg :: T.Text -> CmdFunc
composeMsg str msg = return $ Just ("PRIVMSG", msgDest msg `T.append` str)
