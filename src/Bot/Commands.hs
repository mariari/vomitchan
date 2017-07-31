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
import           Bot.State
import           Bot.StateType
import           System.Random
import           Data.Char
import           Data.Foldable
import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Vector as V
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
admins = ["loli"]

-- list of all Pure functions
-- TODO: if the cmdList has over 50~ commands, put it into a hash table instead
cmdList :: [(CmdFunc, Infix,  CmdAlias)]
cmdList =  [(cmdBots, False,  [".bots", ".bot vomitchan"])
           ,(cmdSrc,  False,  [".source vomitchan"])
           ,(cmdHelp, False,  [".help vomitchan"])
           ,(cmdQuit, False,  [".quit"])
           ,(cmdJoin, False,  [".join"])
           ,(cmdPart, False,  [".leave", ".part"])]

-- List of all Impure functions
cmdListImp :: [(CmdFuncImp, Infix,  CmdAlias)]
cmdListImp =  [(cmdVomit,   True,   ["*vomits*"])
              ,(cmdDream,   False,  ["*cheek pinch*"])
              ,(cmdLewds,   False,  [".lewd "])]

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
cmdSrc = composeMsg "NOTICE" " :[Haskell] https://gitla.in/nymphet/vomitchan"

-- prints help information
-- TODO: Store command info in cmdList and generate this text on the fly
cmdHelp :: CmdFunc
cmdHelp = composeMsg "NOTICE" " :Commands: (.lewd <someone>), (*vomits* [nick]), (*cheek pinch*)"

-- quit
cmdQuit :: CmdFunc
cmdQuit msg
  | msgUser msg `elem` admins = Just ("QUIT", ":Exiting")
  | otherwise                 = Nothing

cmdLewds :: CmdFuncImp
cmdLewds msg = getChanState msg >>= f
  where f state
          | dream state = return . cmdLewd $ msg
          | otherwise   = cmdVomit msg

-- lewd someone (rip halpybot)
cmdLewd :: CmdFunc
cmdLewd msg = (composeMsg "PRIVMSG" . actionMe) ("lewds " <> target) msg
  where target = T.tail $ drpMsg msg " "

-- Causes Vomitchan to sleep ∨ awaken
cmdDream :: CmdFuncImp
cmdDream msg = do
  state <- getChanState msg
  modifyChanState msg (toHashStorage . not . dream <*> mute $ state)
  return $ composeMsg "PRIVMSG" " :dame" msg


-- Vomits up a colorful rainbow if vomitchan is asleep else it just vomits up red with no link
cmdVomit :: CmdFuncImp
cmdVomit msg = do
  state <- getChanState msg
  let
     -- has to be string so foldrM doesn't get annoyed in randApply
      randVom :: Int -> Int -> String
      randVom numT        = fmap (randRange V.!) . take numT . randomRs (0, length randRange - 1) . mkStdGen

      newUsr              = changeNickFstArg msg
      randRang x y        = fst . randomR (x,y) . mkStdGen

      randLink :: IO T.Text
      randLink
          | dream state   = usrFldrNoLog >=> (\y -> linCheck y <$> randomRIO (0, length y -1))
                                         >=> upUsrFile . T.pack . (getUsrFldr newUsr <>) $ newUsr
          | otherwise     = return ""

      linCheck [] _ = ""
      linCheck xs y = xs !! y

      randEff :: String -> Int -> String
      randEff txt num
          | dream state   = f txt
          | otherwise     = f (actionS "\x16" txt)
          where f = actionS (["\x2","\x1D","\x1F","\x16", "", " "] !! randRang 0 5 num)

      randCol :: Int -> String -> String
      randCol num txt
          | dream state   = actionColS txt ([0..15] !! randRang 0 15 num)
          | otherwise     = actionColS txt 4

      randColEff :: Char -> Int -> String
      randColEff txt      = randCol <*> randEff [txt]

      -- consing to text is O(n), thus we deal with strings here
      randApply :: Int -> Int -> IO T.Text
      randApply numT numR = T.pack <$> foldrM (\chr str -> fmap ((<> str) . randColEff chr) randomIO)
                                      "" (randVom numT numR)

      randMessage :: IO T.Text
      randMessage         = randomRIO (8,23) >>= \x ->
                            randomIO         >>= \z ->
                            randomIO         >>= \y -> fold [randApply x z, return " ", randLink, return " ", randApply x y]
  flip (composeMsg "PRIVMSG" . (" :" <>)) msg <$> randMessage

-- Joins the first channel in the message if the user is an admin else do nothing
cmdJoin :: CmdFunc
cmdJoin msg
  | msgUser msg `elem` admins && length (wordMsg msg) > 1 = Just ("JOIN", wordMsg msg !! 1)
  | otherwise = Nothing

-- Leaves the first channel in the message if the user is an admin else do nothing
cmdPart :: CmdFunc
cmdPart msg
  | isAdmin && length (wordMsg msg) > 1       = Just ("PART", wordMsg msg !! 1)
  | isAdmin && "#" `T.isPrefixOf` msgChan msg = Just ("PART", msgChan msg)
  | otherwise                                = Nothing
  where isAdmin = msgUser msg `elem` admins

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

-- generates the randomRange for the cmdVomit command
randRange :: V.Vector Char
randRange = (chr <$>) . V.fromList $  [8704..8959]   -- Mathematical symbols
                                   <> [32..75]       -- parts of the normal alphabet and some misc symbols
                                   <> [10627,10628]  -- obscure braces
                                   <> [10631, 10632] -- obscure braces
                                   <> [945..969]     -- greek symbols

-- Figures out where to send a response to
msgDest :: Message -> T.Text
msgDest msg
  | "#" `T.isPrefixOf` msgChan msg = msgChan msg
  | otherwise                      = msgNick msg

-- Generates a list of words that specify the search constraint
specWord :: Message -> T.Text -> [T.Text]
specWord msg search = (isElemList . T.words . msgContent) msg
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
actionColS :: String -> Int -> String
actionColS txt num = "\0003" <> (show num) <> txt <> "\0003"

-- Used as a generic version for making bold, italic, underlined, and swap, for strings
actionS :: String -> String -> String
actionS cmd txt = cmd <> txt <> cmd

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
  | length (wordMsg msg) > 1 = changeNick (wordMsg msg !! 1) msg
  | otherwise                = msg

-- converts a message into a list containing a list of the contents based on words
wordMsg :: Message -> [T.Text]
wordMsg = T.words . msgContent


-- UNUSED HELPER FUNCTIONS --------------------------------------------------------------------
-- Like drpMsg but does it recursively until the break can't be found anymore
drpMsgRec :: Message -> T.Text -> T.Text ->  [T.Text]
drpMsgRec msg bkL bkR = recurse [] drpMess
  where recurse acc (x,"") = x:acc
        recurse acc (x,xs) = (recurse (x:acc) . drpRight . snd . drpLeft) xs
        drpMess            = T.breakOn bkR (drpMsg msg bkL)
        drpLeft            = T.breakOn bkL
        drpRight           = T.breakOn bkR

-- Used as a generic version for making bold, italic, underlined, and swap
action :: T.Text -> T.Text -> T.Text
action cmd txt = cmd <> txt <> cmd

-- Used for color commnad... color can go all the way up to 15
actionCol :: T.Text -> Int -> T.Text
actionCol txt num = "\0003" <> T.pack (show num) <> txt <> "\0003"
