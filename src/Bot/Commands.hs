{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Bot.Misc
import           Bot.EffType
import           System.Random
import           Data.Char
import           Data.Foldable
import           Control.Lens
import           Control.Applicative
import           Data.Maybe
import           Control.Monad.Reader
import qualified Data.Map    as M
import qualified Data.Vector as V
--- TYPES -------------------------------------------------------------------------------------

-- type of all command functions

type CmdFunc    = Message -> Response (T.Text, T.Text)
type CmdFuncImp = Message -> IO (Response (T.Text, T.Text))

type CmdAlias = [T.Text]

data Color = White | Black | Blue  | Green | Red  | Brown | Purple | Orange | Yellow | LGreen
           | Teal  | LCyan | LBlue | Pink  | Grey | LGrey
           deriving (Enum, Show)

ansiColor :: V.Vector Color
ansiColor = V.fromList [White .. LGrey]

numAnsiColor :: Int
numAnsiColor = V.length ansiColor

-- underline and strikeThrough are new
data Effects = Bold      | Italics   | Color
             | UnderLine | Reverse   | Hex
             | Reset     | MonoSpace | None
             | Strikethrough

instance Show Effects where
  show Bold          = "\x2"
  show Italics       = "\x1D"
  show UnderLine     = "\x1F"
  show Strikethrough = "\x1E"
  show MonoSpace     = "\x11"
  show Color         = "\x3"
  show Hex           = "\x4"
  show Reverse       = "\x16"
  show Reset         = "\xF"
  show None          = ""

effectListLink :: V.Vector String
effectListLink = V.fromList $ fmap show [Bold, Italics, UnderLine, Reverse]

effectList :: V.Vector String
effectList = V.fromList (" " : fmap show [MonoSpace, Strikethrough, None]) <> effectListLink
--- DATA --------------------------------------------------------------------------------------

-- list of admins allowed to use certain commands
-- TODO: Load this from config file
admins :: [T.Text]
admins = ["loli"]


-- list of all Pure functions
cmdList :: Cmd m => [(m Func, [T.Text])]
cmdList = [(cmdBots, [".bots", ".bot vomitchan"])
          ,(cmdSrc,   [".source vomitchan"])
          ,(cmdHelp,  [".help vomitchan"])
          ,(cmdQuit,  [".quit"])
          ,(cmdJoin,  [".join"])
          ,(cmdPart,  [".leave", ".part"])
          ,(cmdLotg,  [".lotg"])
          ,(cmdBane,  [".amysbane"])]

-- List of all Impure functions
cmdListImp :: CmdImp m => [(m Func, [T.Text])]
cmdListImp = [(cmdVomit,    ["*vomits*"])
             ,(cmdDream,    ["*cheek pinch*"])
             ,(cmdFleecy,   ["*step*"])
             ,(cmdLewds,    [".lewd"])
             ,(cmdEightBall,[".8ball"])]

-- The List of all functions pure <> impure
cmdTotList :: CmdImp m => [(m Func, CmdAlias)]
cmdTotList = cmdList <> cmdListImp

-- the Map of all functions that are pure and impure
cmdMapList :: CmdImp m => M.Map T.Text (m Func)
cmdMapList = M.fromList $ cmdTotList >>= f
  where
    f (cfn, aliasList) = (\x -> (x, cfn)) <$> aliasList
-- FUNCTIONS ----------------------------------------------------------------------------------

-- only 1 space is allowed in a command at this point
-- returns a corresponding command function from a message
runCmd :: CmdImp m => m Func
runCmd = do
  msg <- ask
  fromMaybe (return NoResponse) (lookup (split msg))
  where
    split  msg         = T.split (== ' ') (msgContent msg)
    lookup (x : y : _) = lookup [x] <|> lookup [(x <> " " <> y)]
    lookup [x]         = cmdMapList M.!? x
    lookup []          = Nothing

--- COMMAND FUNCTIONS -------------------------------------------------------------------------

-- print bot info
cmdBots :: Cmd m => m Func
cmdBots = composeMsg "NOTICE" " :I am a queasy bot written in Haskell by MrDetonia and loli"

-- print source link
cmdSrc :: Cmd m => m Func
cmdSrc = composeMsg "NOTICE" " :[Haskell] https://github.com/mariari/vomitchan"

-- prints help information
-- TODO: Store command info in cmdList and generate this text on the fly
cmdHelp :: Cmd m => m Func
cmdHelp = composeMsg "NOTICE" " :Commands: (.lewd <someone>), (*vomits* [nick]), (*cheek pinch*)"

-- quit
cmdQuit :: Cmd m => m Func
cmdQuit = shouldQuit <$> ask
  where
    shouldQuit msg
      | isAdmin msg = response
      | otherwise   = NoResponse
      where
        words        = wordMsg msg
        allOrCurrnet = words !! 1
        response
          | length words > 1 && T.toLower allOrCurrnet == "all" = Quit AllNetworks
          | otherwise                                           = Quit CurrentNetwork
{-
cldLewds :: Cmd m => m FuncImp
cmdLewds = f <$> getChanState msg
  where f state
          | dream state = return . cmdLewd $ msg
          | otherwise   = cmdVomit msg
-}

cmdLewds :: CmdImp m => m Func
cmdLewds = getChanStateM >>= f
  where f state
          | dream state = cmdLewd
          | otherwise   = cmdVomit

-- lewd someone (rip halpybot)
cmdLewd :: Cmd m => m Func
cmdLewd = do
  target <- T.tail <$> drpMsg " "
  composeMsg "PRIVMSG" $ actionMe ("lewds " <> target)


-- Causes Vomitchan to sleep ∨ awaken
cmdDream :: CmdImp m => m Func
cmdDream = modifyDreamState >> composeMsg "PRIVMSG" " :dame"

-- Causes vomit to go into fleecy mode (at the moment just prints <3's instead of actual text in cmdVomit)
cmdFleecy :: CmdImp m => m Func
cmdFleecy = modifyFleecyState >> composeMsg "PRIVMSG" " :dame"


-- Vomits up a colorful rainbow if vomitchan is asleep else it just vomits up red with no link
cmdVomit :: CmdImp m => m Func
cmdVomit = do
  msg <- ask
  state <- getChanStateM
  let
     -- has to be string so foldrM doesn't get annoyed in randApply
      randVom :: Int -> Int -> String
      randVom numT numG
        | fleecy state = replicate numT '♥'
        | otherwise    = fmap (randRange V.!) . take numT . randomRs (0, length randRange - 1) . mkStdGen $ numG

      newUsr          = changeNickFstArg msg
      randBetween x y = fst . randomR (x,y) . mkStdGen

      randLink :: IO T.Text
      randLink
          | dream state = usrFldrNoLog newUsr >>= \y -> (y ^?) . element <$> randomRIO (0, length y -1) >>= fileCheck
          | otherwise   = return ""

      -- checks if there is a file to upload!
      fileCheck :: Maybe String -> IO T.Text
      fileCheck = maybe (return "") $ upUsrFile . (getUsrFldrT newUsr <>) . T.pack

      randEff :: V.Vector String -> String -> Int -> String
      randEff effectList txt num
          | dream state = f txt
          | otherwise   = f (action (show Reverse) txt)
          where f = action (effectList V.! randBetween 0 (length effectList - 1) num)

      randCol :: Int -> String -> String
      randCol num txt
          | dream state = actionCol txt (ansiColor V.! randBetween 0 (numAnsiColor - 1) num)
          | otherwise   = actionCol txt Red

      randColEff :: V.Vector String -> Char -> Int -> String
      randColEff eff txt = randCol <*> randEff eff [txt]

      -- consing to text is O(n), thus we deal with strings here
      charApply :: V.Vector String -> Char -> IO String
      charApply eff chr = randColEff eff chr <$> randomIO

      randApply :: Int -> Int -> IO T.Text
      randApply numT numR = T.pack <$> bindM (charApply effectList) (randVom numT numR)

      randApplyLink :: String -> IO T.Text
      randApplyLink str = T.pack <$> bindM (charApply effectListLink) str

      -- checks if the URL has been marked as nsfw, and if so make a string nsfw in light blue
      nsfwStr txt
        | "nsfw" `T.isSuffixOf` txt = T.pack $ action (show Reverse) (actionCol (action (show Bold) "nsfw") LBlue) <> " "
        | otherwise                 = ""

      randMessage :: IO T.Text
      randMessage = do
        x    <- randomRIO (8,23)
        y    <- randomIO
        z    <- randomIO
        link <- randLink
        fold [ return (nsfwStr link)
             , randApply x y,                 return " "
             , randApplyLink (T.unpack link), return " "
             , randApply x z]
  toWrite <- liftIO randMessage
  composeMsg "PRIVMSG" (" :" <> toWrite)

-- Joins the first channel in the message if the user is an admin else do nothing
cmdJoin :: Cmd m => m Func
cmdJoin = join <$> ask
  where
    join msg
      | isAdmin msg && length (wordMsg msg) > 1 = Response ("JOIN", wordMsg msg !! 1)
      | otherwise                               = NoResponse

-- Leaves the first channel in the message if the user is an admin else do nothing
cmdPart :: Cmd m => m Func
cmdPart = part <$> ask
  where
    part msg
      | isAdmin msg && length (wordMsg msg) > 1       = Response ("PART", wordMsg msg !! 1)
      | isAdmin msg && "#" `T.isPrefixOf` msgChan msg = Response ("PART", msgChan msg)
      | otherwise                                      = NoResponse



-- SLEX COMMANDS--------------------------------------------------------------------------------

cmdLotg :: Cmd m => m Func
cmdLotg = do
  target <- T.tail <$> drpMsg " "
  composeMsg "PRIVMSG" (" :May the Luck of the Grasshopper be with you always, " <> target)


cmdEightBall :: CmdImp m => m Func
cmdEightBall = do
  loc <- liftIO (randomRIO (0,length respones - 1))
  composeMsg "PRIVMSG" (" :" <> respones V.! loc)

cmdBane :: Cmd m => m Func
cmdBane = do
  target <- T.tail <$> drpMsg " "
  composeMsg "PRIVMSG" (" :The elder priest tentacles to tentacle "
                         <> target
                         <> "! "
                         <> target
                         <> "'s cloak of magic resistance disintegrates!")
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

-- checks if the user is an admin
isAdmin :: Message -> Bool
isAdmin msg = msgUser msg `elem` admins

-- generates the randomRange for the cmdVomit command
randRange :: V.Vector Char
randRange = (chr <$>) . V.fromList $  [8704..8959]   -- Mathematical symbols
                                   <> [32..75]       -- parts of the normal alphabet and some misc symbols
                                   <> [10627,10628]  -- obscure braces
                                   <> [10631, 10632] -- obscure braces
                                   <> [945..969]     -- greek symbols

respones = V.fromList ["It is certain"
                      ,"It is decidedly so"
                      ,"Without a doubt"
                      ,"Yes definitely"
                      ,"You may rely on it"
                      ,"As I see it, yes"
                      ,"Most likely"
                      ,"Outlook good"
                      ,"Yes"
                      ,"Signs point to yes"
                      ,"Reply hazy try again"
                      ,"Ask again later"
                      ,"Better not tell you now"
                      ,"Cannot predict now"
                      ,"Concentrate and ask again"
                      ,"Don't count on it"
                      ,"My reply is no"
                      ]


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
drpMsg :: Cmd m => T.Text -> m T.Text
drpMsg bk = snd . T.breakOn bk . msgContent <$> ask

-- composes the format that the final send message will be
--composeMsg :: Cmd m => T.Text -> T.Text -> m (Response (T.Text, T.Text))
composeMsg :: Cmd m => T.Text -> T.Text -> m Func
composeMsg method str = do
  msg <- ask
  return $ Response (method, msgDest msg <> str)

-- Used for /me commands
actionMe :: T.Text -> T.Text
actionMe txt = " :\0001ACTION " <> txt <> "\0001"

-- Used as a generic version for making bold, italic, underlined, and swap, for strings
action :: Monoid m => m -> m -> m
action cmd txt = cmd <> txt <> cmd

-- Used for color commnad... color can go all the way up to 15
actionCol :: String -> Color -> String
actionCol txt col = action (show Color) (show (fromEnum col) <> txt)

actionColN :: String -> Int -> String
actionColN txt num = action (show Color) (show num <> txt)

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
drpMsgRec :: Message -> T.Text -> T.Text -> [T.Text]
drpMsgRec msg bkL bkR = recurse [] drpMess
  where recurse acc (x,"") = x:acc
        recurse acc (x,xs) = (recurse (x:acc) . drpRight . snd . drpLeft) xs
        drpMess            = T.breakOn bkR (drpMsg bkL msg)
        drpLeft            = T.breakOn bkL
        drpRight           = T.breakOn bkR

-- Used for color commnad... color can go all the way up to 15
actionColT :: T.Text -> Int -> T.Text
actionColT txt num = "\0003" <> T.pack (show num) <> txt <> "\0003"
