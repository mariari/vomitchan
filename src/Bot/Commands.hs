{-# LANGUAGE FlexibleContexts #-}
--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.Commands (
  runCmd,
  specWord,
) where
--- IMPORTS -----------------------------------------------------------------------------------
import           Bot.FileOps
import           Bot.MessageType
import           Bot.State
import           Bot.StateType
import           Bot.Misc
import           Bot.EffType
import           Bot.NetworkType

import Data.Char (chr)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

import qualified Data.Text as T
import           System.Random
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
--- TYPES -------------------------------------------------------------------------------------

type CmdAlias = [T.Text]

data Color = White | Black | Blue  | Green | Red  | Brown | Purple | Orange | Yellow | LGreen
           | Teal  | LCyan | LBlue | Pink  | Grey | LGrey
           deriving (Enum, Show)

ansiColor :: V.Vector Effects
ansiColor = V.fromList (Color <$> [White .. LGrey])

-- underline and strikeThrough are new
data Effects = Bold      | Italics   | Color Color
             | UnderLine | Reverse   | Hex
             | Reset     | MonoSpace | None
             | Strikethrough | Txt String -- Space is a hack!

instance Show Effects where
  show Bold          = "\x2"
  show Italics       = "\x1D"
  show UnderLine     = "\x1F"
  show Strikethrough = "\x1E"
  show MonoSpace     = "\x11"
  show (Color _)     = "\x3"
  show Hex           = "\x4"
  show Reverse       = "\x16"
  show Reset         = "\xF"
  show (Txt s)       = s
  show None          = ""

effectListLink :: V.Vector Effects
effectListLink = V.fromList [Bold, Italics, UnderLine, Reverse]

effectList :: V.Vector Effects
effectList = V.fromList [Txt " ", MonoSpace, Strikethrough, None] <> effectListLink
--- DATA --------------------------------------------------------------------------------------

-- list of admins allowed to use certain commands
-- TODO: Load this from config file
admins :: [T.Text]
admins = ["loli", "~loli"]

-- list of all Pure functions
cmdList :: Cmd m => [(m Func, [T.Text])]
cmdList = [(cmdBots, [".bots", ".bot vomitchan"])
          ,(cmdSrc,  [".source vomitchan"])
          ,(cmdHelp, [".help vomitchan"])
          ,(cmdQuit, [".quit"])
          ,(cmdJoin, [".join"])
          ,(cmdPart, [".leave", ".part"])]

-- List of all Impure functions
cmdListImp :: CmdImp m => [(m Func, [T.Text])]
cmdListImp = [(cmdVomit,     ["*vomits*"])
             ,(cmdDream,     ["*cheek pinch*"])
             ,(cmdFleecy,    ["*step*"])
             ,(cmdYuki,      ["*yuki*"])
             ,(cmdLewds,     [".lewd"])
             ,(cmdEightBall, [".8ball"])
             ,(cmdLotg,      [".lotg"])
             ,(cmdBane,      [".amysbane"])]

-- The List of all functions pure <> impure
cmdTotList :: CmdImp m => [(m Func, CmdAlias)]
cmdTotList = cmdList <> cmdListImp

-- the Map of all functions that are pure and impure
cmdMapList :: CmdImp m => M.HashMap T.Text (m Func)
cmdMapList = M.fromList $ cmdTotList >>= f
  where
    f (cfn, aliasList) = zip aliasList (repeat cfn)
-- FUNCTIONS ----------------------------------------------------------------------------------

-- only 1 space is allowed in a command at this point
-- returns a corresponding command function from a message
runCmd :: CmdImp m => m Func
runCmd = do
  msg <- message <$> ask
  fromMaybe (return NoResponse) (lookup (split msg))
  where
    split  msg         = T.split (== ' ') (msgContent msg)
    lookup (x : y : _) = lookup [x] <|> lookup [x <> " " <> y]
    lookup [x]         = M.lookup x cmdMapList
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
cmdQuit = shouldQuit . message <$> ask
  where
    shouldQuit msg
      | not (isAdmin msg)                 = NoResponse
      | isSnd words && allOrCurr == "all" = Quit AllNetworks
      | otherwise                         = Quit CurrentNetwork
      where
        words     = wordMsg msg
        allOrCurr = words !! 1

cmdLewds :: CmdImp m => m Func
cmdLewds = getChanStateM >>= f
  where f state
          | _dream state = cmdLewd
          | otherwise    = cmdVomit

-- lewd someone (rip halpybot)
cmdLewd :: CmdImp m => m Func
cmdLewd = do
  target <- T.tail <$> drpMsg " "
  state  <- getChanStateM
  txt    <- (effectText ("lewds " <> target))
  composeMsg "PRIVMSG" $ actionMe txt

-- | Causes Vomitchan to sleep ∨ awaken
cmdDream :: CmdImp m => m Func
cmdDream = modifyDreamState >> composeMsg "PRIVMSG" " :dame"

-- | Causes vomit to go into fleecy mode
-- (at the moment just prints <3's instead of actual text in cmdVomit)
cmdFleecy :: CmdImp m => m Func
cmdFleecy = modifyFleecyState >> composeMsg "PRIVMSG" " :dame"

-- | Causes vomit to go into Yuki ona mode
cmdYuki :: CmdImp m => m Func
cmdYuki = modifyYukiState
       >> formattedEffectText ("dame" `withT` [Color Blue]) >>= composeMsg "PRIVMSG"

-- | Vomits up a colorful rainbow if vomitchan is asleep else it just vomits up red with no link
cmdVomit :: CmdImp m => m Func
cmdVomit = do
  msg   <- message <$> ask
  state <- getChanStateM
  let
      randVom numT numG
        | _fleecy state = replicate numT '♥'
        | otherwise     = take numT (randElems randRange numG)

      newUsr = changeNickFstArg msg

      randLink
        | _dream state = do
            files <- usrFldrNoLog newUsr
            i     <- randomRIO (0, length files - 1)
            fileCheck (files ^? ix i)
        | otherwise = return ""

      -- checks if there is a file to upload!
      fileCheck :: Maybe String -> IO T.Text
      fileCheck = maybe (return "") (upUsrFile . (getUsrFldrT newUsr <>) . T.pack)

      randEff :: V.Vector Effects -> String -> Int -> Int -> String
      randEff effects txt seed1 seed2
        | state^.dream && state^.yuki = txt `with` [randElem effects seed1, Color Blue]
        | state^.dream = txt `with` [randElem effects seed1, randElem ansiColor seed2]
        | otherwise    = txt `with` [randElem effects seed1, Color Red, Reverse]

      -- consing to text is O(n), thus we deal with strings here
      applyEffects :: V.Vector Effects -> String -> Int -> Int -> String
      applyEffects vs xs s = join . zipWith3 (\x -> randEff vs [x]) xs (randGen s) . randGen
        where randGen = randoms . mkStdGen

      randApply numT = applyEffects effectList . randVom numT
      randApplyLink  = applyEffects effectListLink

      nsfwStr txt
        | "nsfw" `T.isSuffixOf` txt = "nsfw" `with` [Reverse, Color LBlue, Bold]
        | otherwise                 = ""

      randPrivMsg :: IO T.Text
      randPrivMsg = do
        x                 <- randomRIO (8,23)
        y:z:g:h:i:l:m:n:_ <- randoms <$> newStdGen :: IO [Int]
        link              <- randLink
        return $ T.pack $  nsfwStr link                      <> " "
                        <> randApply x y g l                 <> " "
                        <> randApplyLink (T.unpack link) h m <> " "
                        <> randApply x z i n
  toWrite <- liftIO randPrivMsg
  composeMsg "PRIVMSG" (" :" <> toWrite)

-- Joins the first channel in the message if the user is an admin else do nothing
cmdJoin :: Cmd m => m Func
cmdJoin = join . message <$> ask
  where
    join msg
      | isAdmin msg && isSnd (wordMsg msg) = Response ("JOIN", wordMsg msg !! 1)
      | otherwise                          = NoResponse

-- Leaves the first channel in the message if the user is an admin else do nothing
cmdPart :: Cmd m => m Func
cmdPart = part . message <$> ask
  where
    part msg
      | isAdmin msg && isSnd (wordMsg msg)            = Response ("PART", wordMsg msg !! 1)
      | isAdmin msg && "#" `T.isPrefixOf` msgChan msg = Response ("PART", msgChan msg)
      | otherwise                                     = NoResponse

-- SLEX COMMANDS--------------------------------------------------------------------------------

cmdEightBall :: CmdImp m => m Func
cmdEightBall = do
  res <- liftIO (randElem respones <$> randomIO)
  txt <- formattedEffectText res
  composeMsg "PRIVMSG" txt

cmdLotg :: CmdImp m => m Func
cmdLotg = do
  target <- T.tail <$> drpMsg " "
  txt    <- formattedEffectText ("May the Luck of the Grasshopper be with you always, " <> target)
  composeMsg "PRIVMSG" txt

cmdBane :: CmdImp m => m Func
cmdBane = do
  target <- T.tail <$> drpMsg " "
  txt <- formattedEffectText ("The elder priest tentacles to tentacle "
                           <> target
                           <> "! "
                           <> target
                           <> "'s cloak of magic resistance disintegrates!")
  composeMsg "PRIVMSG" txt
-- TODO's -------------------------------------------------------------------------------------
--
-- TODO: make reality mode make vomitchan only speak in nods
--
-- TODO: Reality/*dame* that posts quotes of not moving on and staying locked up
-- TODO: Slumber/*dame* that posts quotes of escapism
--
-- TODO: add a *zzz* function that causes the bot go into slumber mode
--

-- Effects ------------------------------------------------------------------------------------

-- | runs the continuations for all the effects
effectText :: CmdImp m => T.Text -> m T.Text
effectText text =
  ($ text) <$> yukiMode id

-- | like effectText, but formats it properly for compose message
formattedEffectText :: CmdImp m => T.Text -> m T.Text
formattedEffectText text = (" :" <>) <$> effectText text

-- | the continuation for the yukimode effect
yukiMode cont = do
  state <- getChanStateM
  if state^.yuki then
    return $ cont . (`withT` [Color Blue])
  else
    return cont

--- HELPER FUNCTIONS --------------------------------------------------------------------------

-- | checks if the user is an admin
isAdmin :: PrivMsg -> Bool
isAdmin = maybe False (`elem` admins) . msgUser

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
msgDest :: PrivMsg -> T.Text
msgDest msg
  | "#" `T.isPrefixOf` msgChan msg = msgChan msg
  | otherwise                      = msgNick msg

-- Generates a list of words that specify the search constraint
specWord :: PrivMsg -> T.Text -> [T.Text]
specWord msg search = filter (search `T.isPrefixOf`) (wordMsg msg)

-- Drops the command message [.lewd *vomits*]... send *command* messages via T.tail msg
drpMsg :: Cmd m => T.Text -> m T.Text
drpMsg bk = snd . T.breakOn bk . msgContent . message <$> ask

-- composes the format that the final send message will be
composeMsg :: Cmd m => T.Text -> T.Text -> m Func
composeMsg method str = do
  dest <- msgDest . message <$> ask
  return $ Response (method, dest <> str)

-- Used for /me commands
actionMe :: T.Text -> T.Text
actionMe txt = " :\0001ACTION " <> txt <> "\0001"

-- | Used as a generic version for making bold, italic, underlined, colors, and swap, for strings
action :: Effects -> String -> String
action eff txt = seff <> payload eff <> seff
  where
    seff              = show eff
    payload (Color c) = showColor (fromEnum c) <> txt
    payload _         = txt

-- | same as action, but takes text instead
actionT :: Effects -> T.Text -> T.Text
actionT eff txt = seff <> payload eff <> seff
  where
    seff              = T.pack $ show eff
    payload (Color c) = T.pack (showColor (fromEnum c)) <> txt
    payload _         = txt

-- | used to do multiple actions in a row
with :: Foldable t => String -> t Effects -> String
with = foldr action

withT ::  Foldable t => T.Text -> t Effects -> T.Text
withT = foldr actionT

-- Changes the nick of the msg
changeNick :: Nick -> PrivMsg -> PrivMsg
changeNick nick (PrivMsg usr chan cont) = PrivMsg (usr {usrNick = nick}) chan cont

-- Changes the nick of the msg if the first argument specifies it
changeNickFstArg :: PrivMsg -> PrivMsg
changeNickFstArg msg
  | isSnd (wordMsg msg) = changeNick (wordMsg msg !! 1) msg
  | otherwise           = msg

isSnd (x : y : _) = True
isSnd _           = False

-- converts a message into a list containing a list of the contents based on words
wordMsg :: PrivMsg -> [T.Text]
wordMsg = T.words . msgContent

-- in the IRC protocol if numbers don't have at least 2 digits, then changing the color before a
-- number would invalidate it
showColor :: (Ord a, Num a, Show a) => a -> String
showColor x
  | x < 10    = "0" <> show x
  | otherwise = show x

-- grabs an infinite list of random values inside some vector
randElems :: V.Vector a -> Int -> [a]
randElems xs = fmap (xs V.!)  . randomRs (0, length xs - 1) . mkStdGen

randElem :: V.Vector a -> Int -> a
randElem xs  = head . randElems xs
