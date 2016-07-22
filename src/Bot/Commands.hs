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
import           Bot.FileOps


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
cmdList :: [ (CmdFunc, [T.Text])]
cmdList =  [ (cmdBots, [".bots"])
           , (cmdQuit, [".quit"])
           , (cmdLewd, [".lewd "])
           , (cmdLog,  ["http","ftp"])]


-- FUNCTIONS ---

-- returns a corresponding command function from a message
runCmd :: CmdFunc
runCmd msg = foldr testFunc (return Nothing) cmdList
  where
    testFunc (cmd, p) k
      | or (flip T.isPrefixOf (msgContent msg) <$> p) 
      || or (flip T.isInfixOf (msgContent msg) <$> p) = cmd msg
      | otherwise                                     = k

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
  where target = T.tail $ drpMsg msg " "

cmdLog :: CmdFunc
cmdLog msg =  createUsrFldr msg >> mapM_ (appendLog msg) newLinks >> return Nothing
  where links = (fst . T.breakOn " " . drpMsg msg) <$> ["http", "ftp"]
        newLinks = (<>) <$> links <*> ["\n"]
        


-- >> (createUsrFldr msg) 

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

-- Drops the command message [.lewd *vomits*] sent to vomitchan... T.drop 1 is removed... send it via T.tail msg
drpMsg :: Message -> T.Text -> T.Text
drpMsg msg bk = (snd . T.breakOn bk . msgContent) msg

-- Used for /me commands 
actionMe :: T.Text -> T.Text
actionMe txt = " :\0001ACTION " <> txt <> "\0001"
