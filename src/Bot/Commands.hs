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
import           Bot.Modifier

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

data Message = Action T.Text | Message T.Text

instance Show TextEffects where
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

effectListLink :: V.Vector TextEffects
effectListLink = V.fromList [Bold, Italics, UnderLine, Reverse]

effectList :: V.Vector TextEffects
effectList = V.fromList [Txt " ", MonoSpace, Strikethrough, None] <> effectListLink
--- DATA --------------------------------------------------------------------------------------

-- list of all Pure functions
cmdList :: (Cmd m, CmdImp m') => [(ContFuncPure m m', [T.Text], Effect m')]
cmdList = [(cmdBots, [".bots", ".bot vomitchan"], effectText)
          ,(cmdSrc,  [".source vomitchan"]      , effectText)
          ,(cmdHelp, [".help vomitchan"]        , effectText)
          ,(cmdQuit, [".quit"]                  , const pure)
          ,(cmdJoin, [".join"]                  , const pure)
          ,(cmdPart, [".leave", ".part"]        , const pure)
          ,(cmdLotg, [".lotg"]                  , effectText)
          ,(cmdBane, [".amysbane"]              , effectText)]

-- List of all Impure functions
cmdListImp :: CmdImp m => [(ContFunc m, [T.Text], Effect m)]
cmdListImp = [(cmdVomit,     ["*vomits*"]       , effectTextRandom)
             ,(cmdDream,     ["*cheek pinch*"]  , effectText)
             ,(cmdFleecy,    ["*step*"]         , effectText)
             ,(cmdYuki,      ["*yuki*"]         , effectText)
             ,(cmdLewds,     [".lewd"]          , effectTextRandom)
             ,(cmdEightBall, [".8ball"]         , effectText)]

-- The List of all functions pure <> impure
cmdTotList :: CmdImp m => [(m (Effect m -> m Func), [T.Text], Effect m)]
cmdTotList = cmdList <> cmdListImp

-- the Map of all functions that are pure and impure
cmdMapList :: CmdImp m => M.HashMap T.Text (m Func)
cmdMapList = M.fromList $ cmdTotList >>= f
  where
    f (cfn, aliasList, eff) = zip aliasList (repeat (cfn >>= ($ eff)))
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
cmdBots :: (Cmd m, Monad m') => ContFuncPure m m'
cmdBots = noticeMsg "I am a queasy bot written in Haskell by MrDetonia and loli"

-- print source link
cmdSrc :: (Cmd m, Monad m') => ContFuncPure m m'
cmdSrc = noticeMsg "[Haskell] https://github.com/mariari/vomitchan"

-- prints help information
-- TODO: Store command info in cmdList and generate this text on the fly
cmdHelp :: (Cmd m, Monad m') => ContFuncPure m m'
cmdHelp = noticeMsg "Commands: (.lewd <someone>), (*vomits* [nick]), (*cheek pinch*)"

-- quit
cmdQuit :: (Cmd m, Monad m') => ContFuncPure m m'
cmdQuit = asks shouldQuit
  where
    shouldQuit info
      | not (isAdmin info)                 = noResponse
      | isSnd words && allOrCurr == "all"  = quit AllNetworks
      | otherwise                          = quit CurrentNetwork
      where
        words     = wordMsg . message $ info
        allOrCurr = words !! 1

cmdLewds :: CmdImp m => ContFunc m
cmdLewds = getChanStateM >>= f
  where f state
          | _dream state = cmdLewd
          | otherwise    = cmdVomit

-- lewd someone (rip halpybot)
cmdLewd :: CmdImp m => ContFunc m
cmdLewd = do
  target <- T.tail <$> drpMsg " "
  actionMsg ("lewds " <> target)

-- | Causes Vomitchan to sleep ∨ awaken
cmdDream :: CmdImp m => ContFunc m
cmdDream = modifyDreamState >> privMsg "dame"

-- | Causes vomit to go into fleecy mode
-- (at the moment just prints <3's instead of actual text in cmdVomit)
cmdFleecy :: CmdImp m => ContFunc m
cmdFleecy = modifyFleecyState >> privMsg "dame"

-- | Causes vomit to go into Yuki ona mode
cmdYuki :: CmdImp m => ContFunc m
cmdYuki = modifyYukiState >> privMsg "dame"

-- | Vomits up a colorful rainbow if vomitchan is asleep else it just vomits up red with no link
cmdVomit :: CmdImp m => m (Effect m -> m Func)
cmdVomit = do
  msg   <- asks message
  state <- getChanStateM
  pure $ \contEffect -> do
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

        randApply numLength =
          contEffect (Extra {validEffects = effectList}) . T.pack . randVom numLength
        randApplyLink =
          contEffect (Extra {validEffects = effectListLink})

        nsfwStr txt
          | "nsfw" `T.isSuffixOf` txt = "nsfw" `with` [Reverse, Color LBlue, Bold]
          | otherwise                 = ""

        randPrivMsg = do
          x               <- liftIO $ randomRIO (8,23)
          randomList      <- liftIO (randoms <$> newStdGen :: IO [Int])
          link            <- liftIO randLink >>= randApplyLink
          startingVomText <- randApply x (head randomList)
          endingVomText   <- randApply x (randomList !! 1)
          return $ T.pack (nsfwStr link) <> " "
                 <> startingVomText      <> " "
                 <> link                 <> " "
                 <> endingVomText
    toWrite <- randPrivMsg
    privMsg toWrite >>= ($ const pure)

-- Joins the first channel in the message if the user is an admin else do nothing
cmdJoin :: (Cmd m, Monad m') => ContFuncPure m m'
cmdJoin = asks join
  where
    join info
      | isAdmin info && isSnd msg = response ("JOIN", msg !! 1)
      | otherwise                 = noResponse
      where
        msg = wordMsg . message $ info

-- Leaves the first channel in the message if the user is an admin else do nothing
cmdPart :: (Cmd m, Monad m') => ContFuncPure m m'
cmdPart = asks part
  where
    part info
      | isAdmin info && isSnd msg                                 = response ("PART", msg !! 1)
      | isAdmin info && "#" `T.isPrefixOf` msgChan (message info) = response ("PART", msgChan . message $ info)
      | otherwise                                                 = noResponse
      where
        msg = wordMsg . message $ info

-- SLEX COMMANDS--------------------------------------------------------------------------------

cmdEightBall :: CmdImp m => ContFunc m
cmdEightBall = do
  res <- liftIO (randElem respones <$> randomIO)
  privMsg res

cmdTarget f = do
  target' <- drpMsg " "
  if mempty == target' then
      return noResponse
    else do
      let target = T.tail target'
      privMsg (f target)

cmdLotg :: (Cmd m, Monad m') => ContFuncPure m m'
cmdLotg =
  cmdTarget (\target -> "May the Luck of the Grasshopper be with you always, " <> target)

cmdBane :: (Cmd m, Monad m') => ContFuncPure m m'
cmdBane = do
  cmdTarget (\target -> "The elder priest tentacles to tentacle "
                     <> target
                     <> "! "
                     <> target
                     <> "'s cloak of magic resistance disintegrates!")
-- TODO's -------------------------------------------------------------------------------------
--
-- TODO: make reality mode make vomitchan only speak in nods
--
-- TODO: Reality/*dame* that posts quotes of not moving on and staying locked up
-- TODO: Slumber/*dame* that posts quotes of escapism
--
-- TODO: add a *zzz* function that causes the bot go into slumber mode
--

-- TextEffects ------------------------------------------------------------------------------------

-- | runs the continuations for all the effects
effectText :: CmdImp m => Effect m
effectText _extraArg = yukiMode

effectTextRandom :: CmdImp m => Effect m
effectTextRandom Extra {validEffects = validEffects} =
   dryEffect <=< yukiMode <=< randomColorEffect <=< randomEffect validEffects

-- | the continuation for the yukimode effect
yukiMode :: (MonadReader InfoPriv f, MonadIO f) => T.Text -> f T.Text
yukiMode text = f <$> getChanStateM
  where
    f state | state^.yuki = text `withT` [Color Blue]
            | otherwise   = text

randomColorEffect
  :: (MonadReader InfoPriv m, MonadIO m)
  => T.Text -> m T.Text
randomColorEffect text =
  getChanStateM >>= f
  where
    f state
      | not (state^.yuki) && (state^.dream) = do
          let colorizeText char (random1 :_) =
                [char] `with` [randElem ansiColor random1]
          onAllCharsT colorizeText text
      | otherwise =
          pure text

randomEffect :: (MonadReader InfoPriv m, MonadIO m) =>
                 V.Vector TextEffects ->  T.Text -> m T.Text
randomEffect validEffects text =
  let colorizeText char (random1 :_) =
        [char] `with` [randElem validEffects random1]
  in onAllCharsT colorizeText text

dryEffect :: (MonadReader InfoPriv f, MonadIO f) => T.Text -> f T.Text
dryEffect text =
  f <$> getChanStateM
  where
    f state
      | not (state^.dream) = text `withT` [Color Red, Reverse]
      | otherwise        = text

onAllCharsT :: (MonadIO m, Random a) => (Char -> [a] -> [Char]) -> T.Text -> m T.Text
onAllCharsT f text = do
  let onAllElemetns char =
        f char . randoms <$> newStdGen
  nestedString <- liftIO $ traverse onAllElemetns (T.unpack text)
  pure (T.pack (join nestedString))

-- TODO ∷ need to make the effect not run on texts
heartify = undefined

--- HELPER FUNCTIONS --------------------------------------------------------------------------

-- | checks if the user is an admin
isAdmin :: InfoPriv -> Bool
isAdmin info = (msgNick . message $ info) `elem` (netAdmins . network $ info)

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
drpMsg bk = asks (snd . T.breakOn bk . msgContent . message)

-- composes the format that the final send message will be
composeMsg :: (MonadReader InfoPriv m1, Monad m2) =>
             a -> Extra -> Message -> m1 (Effect m2 -> m2 (Response (a, T.Text)))
composeMsg method effModifier str = do
  dest <- asks (msgDest . message)
  pure $ \eff -> do
    -- if we have a message append nothing to it
    -- actions need to append to the text to properly work
    let appendAction =
          case str of
            Message _ -> id
            Action  _ -> actionMe
    sentBack <- eff effModifier (unboxMessage str)
    return $ Response (method, T.concat [dest, " :", appendAction sentBack])

unboxMessage :: Message -> T.Text
unboxMessage (Action  t) = t
unboxMessage (Message t) = t

-- Some aliases for composeMsg

privMsg, actionMsg, noticeMsg
  :: (MonadReader InfoPriv m1, Monad m2)
  => T.Text
  -> m1 (Effect m2 -> m2 (Response (T.Text, T.Text)))
privMsg = composeMsg "PRIVMSG" (Extra V.empty) . Message

actionMsg = composeMsg "PRIVMSG" (Extra V.empty) . Action

noticeMsg = composeMsg "NOTICE" (Extra V.empty) . Message


-- Used for /me commands
actionMe :: T.Text -> T.Text
actionMe txt = "\0001ACTION " <> txt <> "\0001"

-- | Used as a generic version for making bold, italic, underlined, colors, and swap, for strings
action :: TextEffects -> String -> String
action eff txt = seff <> payload eff <> seff
  where
    seff              = show eff
    payload (Color c) = showColor (fromEnum c) <> txt
    payload _         = txt

-- | same as action, but takes text instead
actionT :: TextEffects -> T.Text -> T.Text
actionT eff txt = seff <> payload eff <> seff
  where
    seff              = T.pack $ show eff
    payload (Color c) = T.pack (showColor (fromEnum c)) <> txt
    payload _         = txt

-- | used to do multiple actions in a row
with :: Foldable t => String -> t TextEffects -> String
with = foldr action

withT ::  Foldable t => T.Text -> t TextEffects -> T.Text
withT = foldr actionT

-- Changes the nick of the msg
changeNick :: Nick -> PrivMsg -> PrivMsg
changeNick nick (PrivMsg usr chan cont) = PrivMsg (usr {usrNick = nick}) chan cont

-- Changes the nick of the msg if the first argument specifies it
changeNickFstArg :: PrivMsg -> PrivMsg
changeNickFstArg msg
  | isSnd (wordMsg msg) = changeNick (wordMsg msg !! 1) msg
  | otherwise           = msg

isSnd (_ : _ : _) = True
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
