{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

--- MODULE DEFINITION ---
module Bot.Commands (
  runCmd,
) where


--- IMPORTS ---
import qualified Data.Text        as T
import           Data.Monoid
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
cmdLewd msg = (composeMsg . actionMe) ("lewds " <> target) msg
  where target = drpMsg " " msg

-- TODO: add a *vomits* function that grabs random images/links from the channel that it's from and produces rainbow text before and after

-- TODO: add a *cheek pinch* function that puts the bot into reality mode

-- TODO: make reality mode make vomitchan only speak in nods

-- TODO: Reality/*dame* that posts quotes of not moving on and staying locked up
-- TODO: Slumber/*dame* that posts quotes of escapism

-- TODO: add a *zzz* function that causes the bot go into slumber mode

--- HELPER FUNCTIONS ---

-- figures out where to send a response to
msgDest :: Message -> T.Text
msgDest msg
  | "#" `T.isPrefixOf` msgChan msg = msgChan msg
  | otherwise                      = msgNick msg

-- composes the format that the final send message will be
composeMsg :: T.Text -> CmdFunc
composeMsg str msg = return $ Just ("PRIVMSG", msgDest msg <> str)

-- Drops the command message [.lewd *vomits*] sent to vomitchan... the extra (T.drop 1) is there for * * commands
drpMsg :: T.Text -> Message -> T.Text
drpMsg bk = T.drop 1 . snd . T.breakOn bk . T.drop 1 . msgContent

-- Used for /me commands 
actionMe :: T.Text -> T.Text
actionMe txt = " :\0001ACTION " <> txt <> "\0001"
