{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.Commands (
  runCmd,
  createUsrFldr,
  specWord,
) where
--- IMPORTS -----------------------------------------------------------------------------------
import           Data.Monoid
import qualified Data.Text       as T

import           Bot.FileOps
import           Bot.MessageType
import           System.Random
import           Data.Char
import           Data.Foldable
--- TYPES -------------------------------------------------------------------------------------

-- type of all command functions
type CmdFunc    = Message -> Maybe (T.Text, T.Text)
type CmdFuncImp = Message -> IO (Maybe (T.Text, T.Text))

-- Used for telling if the command is infix or not
type Infix = Bool

type CmdAlias = [T.Text]

--- DATA --------------------------------------------------------------------------------------

-- list of admins allowed to use certain commands
-- TODO: Load this from config file
admins :: [T.Text]
admins = ["MrDetonia", "loli"]

-- list of all Pure functions
-- TODO: if the cmdList has over 50~ commands, put it into a hash table instead
cmdList :: [ (CmdFunc, Infix,  CmdAlias)]
cmdList =  [ (cmdBots, False, [".bots", ".bot vomitchan"])
           , (cmdSrc,  False, [".source vomitchan"])
           , (cmdHelp, False, [".help vomitchan"])
           , (cmdQuit, False, [".quit"])
           , (cmdLewd, False, [".lewd "])]

-- List of all Impure functions
cmdListImp :: [ (CmdFuncImp, Infix,  CmdAlias)]
cmdListImp = [ (cmdVomit,    False, ["*vomits*"])]

-- The List of all functions pure <> impure
cmdTotList :: [(CmdFuncImp, Infix,  CmdAlias)]
cmdTotList = cmdList2 <> cmdListImp
  where cmdList2 = fmap (\x -> (return . f3 x, s3 x, t3 x)) cmdList

-- FUNCTIONS ----------------------------------------------------------------------------------

-- returns a corresponding command function from a message
runCmd :: CmdFuncImp
runCmd msg = foldr testFunc (return Nothing) cmdTotList
  where
    testFunc (cmd, inf, p) k
      | check T.isPrefixOf || (inf && check T.isInfixOf) = cmd msg
      | otherwise                                        = k
      where
        check f = or (fmap (`f` msgContent msg) p)

--- COMMAND FUNCTIONS -------------------------------------------------------------------------

-- print bot info
cmdBots :: CmdFunc
cmdBots = composeMsg "NOTICE" " :I am a queasy bot written in Haskell by MrDetonia and loli"

-- print source link
cmdSrc :: CmdFunc
cmdSrc = composeMsg "NOTICE" " :[Haskell] https://gitla.in/MrDetonia/vomitchan"

-- prints help information
-- TODO: Store command info in cmdList and generate this text on the fly
cmdHelp :: CmdFunc
cmdHelp = composeMsg "NOTICE" " :Commands: .lewd <someone>, *vomits* [nick]"

-- quit
cmdQuit :: CmdFunc
cmdQuit msg
  | msgUser msg `elem` admins = Just ("QUIT", ":Exiting")
  | otherwise                 = Nothing

-- lewd someone (rip halpybot)
cmdLewd :: CmdFunc
cmdLewd msg = (composeMsg "PRIVMSG" . actionMe) ("lewds " <> target) msg
  where target = T.tail $ drpMsg msg " "

cmdVomit :: CmdFuncImp
cmdVomit msg = (\y -> (composeMsg "PRIVMSG" . (<>) " :" . T.pack) y msg) <$> randMessage
  where
    randVom numT numR   = chr <$> (take numT . randomRs (75, 41) . mkStdGen) numR
    newUsr              = changeNickFstArg msg

    randRang x y        = fst . randomR (x,y) . mkStdGen
    randLin             = listUsrFldrNoLog newUsr >>= \y -> fmap (linCheck y) (randomRIO (0, length y -1))
                                                  >>= \z -> T.unpack <$> (upUsrFile . T.pack) (getUsrFldr newUsr <> z)
    linCheck lis ele
      | length lis == 0 = ""
      | otherwise       = lis !! ele

    randEff txt num     = action (["\x2","\x1D","\x1F","\x16", "", " "] !! randRang 0 5 num) txt
    randCol num txt     = actionCol txt ([0..15] !! randRang 0 15 num)
    randColEff txt      = randCol <*> (randEff . T.pack) [txt]
    randApply numT numR = foldrM (\chr str -> ((\num -> (T.unpack $ randColEff chr num) <> str) <$> randomIO)) "" (randVom numT numR)

    randMessage         = randomRIO (8,23) >>= \x -> randomIO >>= \z -> randomIO
                                           >>= \y -> randApply x z <> return " " <> randLin <> return " "  <> randApply x y


-- TODO's -------------------------------------------------------------------------------------
--
-- TODO: add a *cheek pinch* function that puts the bot into reality mode
--
-- TODO: make reality mode make vomitchan only speak in nods
--
-- TODO: Reality/*dame* that posts quotes of not moving on and staying locked up
-- TODO: Slumber/*dame* that posts quotes of escapism
--
-- TODO: add a *zzz* function that causes the bot go into slumber mode
--
--- HELPER FUNCTIONS --------------------------------------------------------------------------

-- figures out where to send a response to
msgDest :: Message -> T.Text
msgDest msg
  | "#" `T.isPrefixOf` msgChan msg = msgChan msg
  | otherwise                      = msgNick msg

-- Generates a list of words that specify the search constraint
specWord :: Message -> T.Text -> [T.Text]
specWord msg search = (isElemList. T.words . msgContent) msg
  where isElemList = filter (search `T.isPrefixOf`)

-- Drops the command message [.lewd *vomits*]... send *command* messages via T.tail msg
drpMsg :: Message -> T.Text -> T.Text
drpMsg msg bk = (snd . T.breakOn bk . msgContent) msg

-- composes the format that the final send message will be
composeMsg :: T.Text -> T.Text -> CmdFunc
composeMsg method str msg = Just (method, msgDest msg <> str)

-- Used for /me commands
actionMe :: T.Text -> T.Text
actionMe txt = " :\0001ACTION " <> txt <> "\0001"

-- Used for color commnad... color can go all the way up to 15
actionCol :: (Num a, Show a) => T.Text -> a -> T.Text
actionCol txt num = "\0003" <> T.pack (show num) <> txt <> "\0003"

-- Used as a generic version for making bold, italic, underlined, and swap
action :: T.Text -> T.Text -> T.Text
action cmd txt = cmd <> txt <> cmd

-- Used for  grabbing elements out of a 3 element tuple
f3 :: (a,b,c) -> a
f3 (a,_,_) = a

s3 :: (a,b,c) -> b
s3 (_,b,_) = b

t3 :: (a,b,c) -> c
t3 (_,_,c) = c

-- Changes the nick of the msg
changeNick :: T.Text -> Message -> Message
changeNick nick msg = msg {msgNick = nick}

-- Changes the nick of the msg if the first argument specifies it
changeNickFstArg :: Message -> Message
changeNickFstArg msg
  | length wordMsg > 1 = changeNick (wordMsg !! 1) msg
  | otherwise           = msg
  where wordMsg = (T.words . msgContent) msg

-- UNUSED HELPER FUNCTIONS --------------------------------------------------------------------
-- Like drpMsg but does it recursively until the break can't be found anymore
drpMsgRec :: Message -> T.Text -> T.Text ->  [T.Text]
drpMsgRec msg bkL bkR = recurse [] drpMess
  where recurse acc (x,"") = x:acc
        recurse acc (x,xs) = (recurse (x:acc) . drpRight . snd . drpLeft) xs
        drpMess            = T.breakOn bkR (drpMsg msg bkL)
        drpLeft            = T.breakOn bkL
        drpRight           = T.breakOn bkR
