{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

--- MODULE DEFINITION ---
module Bot.Commands (
  getCmd,
) where


--- IMPORTS ---
import qualified Data.Text        as T
import qualified Data.Text.Format as T
import qualified Data.Text.IO     as T

import           Bot.MessageType


--- TYPES ---

-- type of all command functions
type CmdFunc = Message -> Maybe (T.Text, T.Text)


--- DATA ---

-- list of admins allowed to use certain commands
-- TODO: Load this from config file
admins :: [T.Text]
admins = ["MrDetonia", "loli"]

-- list of prefixes and corresponding command functions
cmdList :: [(T.Text, CmdFunc)]
cmdList = [ (".bots", cmdBots)
          , (".quit", cmdQuit)
          ]


--- FUNCTIONS ---

-- print bot info
cmdBots :: CmdFunc
cmdBots msg = Just ("PRIVMSG", msgDest msg `T.append` " :I am a queasy bot written in Haskell | https://gitla.in/MrDetonia/vomitchan")

-- quit
cmdQuit :: CmdFunc
cmdQuit msg
    | msgUser msg `elem` admins = Just ("QUIT", ":Exiting")
    | otherwise                 = Nothing

-- returns a corresponding command function from a message
getCmd :: Message -> Maybe CmdFunc
getCmd msg = foldr testFunc Nothing cmdList

    where
        testFunc (p, cmd) _
            | p `T.isPrefixOf` msgContent msg = Just cmd
            | otherwise                       = Nothing


--- HELPER FUNCTIONS ---

-- figures out where to send a response to
msgDest :: Message -> T.Text
msgDest msg
    | "#" `T.isPrefixOf` msgChan msg = msgChan msg
    | otherwise                      = msgNick msg
