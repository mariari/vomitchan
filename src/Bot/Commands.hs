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
import           Bot.Database
import qualified Bot.Modifier    as Modifier

import Data.Char           (chr)
import Control.Applicative ((<|>))
import Data.Maybe          (fromMaybe)

import qualified Data.Text            as T
import           Data.Foldable        (fold)
import           System.Random
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.HashMap.Strict  as M
import qualified Data.Vector          as V

--- TYPES -------------------------------------------------------------------------------------

data Message = Action Modifier.T | Message Modifier.T

effectListLink :: V.Vector Modifier.TextEffects
effectListLink =
  V.fromList [Modifier.Bold, Modifier.Italics, Modifier.UnderLine, Modifier.Reverse]

effectList :: V.Vector Modifier.TextEffects
effectList =
  V.fromList
    [Modifier.Txt " ", Modifier.MonoSpace, Modifier.Strikethrough, Modifier.None]
  <> effectListLink
--- DATA --------------------------------------------------------------------------------------

-- list of all Pure functions
cmdList :: (Cmd m, CmdImp m') => [(ContFuncPure m m', [T.Text], Effect m', Maybe T.Text)]
cmdList = [(cmdBots, [".bots", ".bot vomitchan"], effectText, Nothing)
          ,(cmdSrc,  [".source vomitchan"]      , effectText, Nothing)
          ,(cmdHelp, [".help vomitchan"]        , effectText, Nothing)
          ,(cmdQuit, [".quit"]                  , const pure, Nothing)
          ,(cmdJoin, [".join"]                  , const pure, Nothing)
          ,(cmdPart, [".leave", ".part"]        , const pure, Nothing)
          ,(cmdLotg, [".lotg"]                  , effectText, Just "<someone>")
          ,(cmdBane, [".amysbane"]              , effectText, Just "<someone>")]

-- List of all Impure functions
cmdListImp :: CmdImp m => [(ContFunc m, [T.Text], Effect m, Maybe T.Text)]
cmdListImp = [(cmdVomit,     ["*vomits*"]       , effectTextRandom, Just "<someone>")
             ,(cmdRoulette,  ["*roulette*"]     , effectTextRandom, Nothing)
             ,(cmdDream,     ["*cheek pinch*"]  , effectText      , Nothing)
             ,(cmdFleecy,    ["*step*"]         , effectText      , Nothing)
             ,(cmdYuki,      ["*yuki*"]         , effectText      , Nothing)
             ,(cmdLewds,     [".lewd"]          , effectTextRandom, Just "<someone>")
             ,(cmdEightBall, [".8ball"]         , effectText      , Nothing)
             ,(cmdNukeMD5,   ["*nuke*"]         , const pure      , Just "<md5>")
             ,(cmdCut,       ["*cut*"]          , const pure      , Just "<link> <user>")
             ,(cmdSetNSFW,   [".set-nsfw"]      , const pure      , Just "<link>")
             ,(cmdSetNSFL,   [".set-nsfl"]      , const pure      , Just "<link>")]

cmdListHelp :: [(T.Text, Maybe T.Text)]
cmdListHelp = [(".bots"            , Nothing)
              ,(".source vomitchan", Nothing)
              ,(".help vomitchan"  , Nothing)
              ,(".quit"            , Nothing)
              ,(".join"            , Nothing)
              ,(".leave"           , Nothing)
              ,(".8ball"           , Nothing)
              ,("*roulette*"       , Nothing)
              ,("*cheek pinch*"    , Nothing)
              ,("*step*"           , Nothing)
              ,("*yuki*"           , Nothing)
              ,("*roulette*"       , Nothing)
              ,(".lotg"            , Just "<someone>")
              ,(".amysbane"        , Just "<someone>")
              ,("*vomits*"         , Just "<someone>")
              ,(".lewd"            , Just "<someone>")
              ,("*cut*"            , Just "<link> <user>")
              ,(".set-nsfw"        , Just "<link>")
              ,(".set-nsfl"        , Just "<link>")]

-- The List of all functions pure <> impure
cmdTotList :: CmdImp m => [(m (Effect m -> m Func), [T.Text], Effect m, Maybe T.Text)]
cmdTotList = cmdList <> cmdListImp


-- the Map of all functions that are pure and impure
cmdMapList :: CmdImp m => M.HashMap T.Text (m Func)
cmdMapList = M.fromList $ cmdTotList >>= f
  where
    f (cfn, aliasList, eff, _) = zip aliasList (repeat (cfn >>= ($ eff)))
-- FUNCTIONS ----------------------------------------------------------------------------------

-- only 1 space is allowed in a command at this point
-- returns a corresponding command function from a message
runCmd :: CmdImp m => m Func
runCmd = do
  msg <- asks message
  fromMaybe (return NoResponse) (lookup (split msg))
  where
    split  msg         = T.split (== ' ') (msgContent msg)
    lookup (x : y : _) = lookup [x] <|> lookup [x <> " " <> y]
    lookup [x]         = M.lookup x cmdMapList
    lookup []          = Nothing

--- COMMAND FUNCTIONS -------------------------------------------------------------------------

-- print bot info
cmdBots :: (Cmd m, Monad m') => ContFuncPure m m'
cmdBots =
  noticeMsgPlain "I am a queasy bot written in Haskell by MrDetonia and mariari"

-- print source link
cmdSrc :: (Cmd m, Monad m') => ContFuncPure m m'
cmdSrc = noticeMsgPlain "[Haskell] https://github.com/mariari/vomitchan"

-- prints help information
-- TODO: Store command info in cmdList and generate this text on the fly
cmdHelpText :: T.Text
cmdHelpText = fold (createHelp (head cmdListHelp) : (fmap createHelps $ drop 1 cmdListHelp))
  where createHelps (cmd, Just help) = fold [", (", cmd, " ", help, ")"]
        createHelps (cmd, Nothing)   = fold [", (", cmd, ")"]

        createHelp (cmd, Just help) = fold ["(", cmd, " ", help, ")"]
        createHelp (cmd, Nothing)   = fold ["(", cmd, ")"]


cmdHelp :: (Cmd m, Monad m') => ContFuncPure m m'
cmdHelp = noticeMsgPlain ("Commands: " <> cmdHelpText)

cmdSetNSFW :: CmdImp m => m (Effect m -> m Func)
cmdSetNSFW = cmdSetMetadata "nsfw" updateNSFW

cmdSetNSFL :: CmdImp m => m (Effect m -> m Func)
cmdSetNSFL = cmdSetMetadata "nsfl" updateNSFL

-- | Set field for a specific link
cmdSetMetadata :: CmdImp m => String -> (String -> String -> String -> IO Int) -> m (Effect m -> m Func)
cmdSetMetadata field updater = do
  info <- ask
  shouldUpdate info
  where
    shouldUpdate info
      | isSnd (wordMsg . message $ info) = process info
      | otherwise = noticeMsgPlain (".set-" <> T.pack field <> " <link>")

      where
        md5 = (wordMsg . message $ info) !! 1
        user = T.unpack . msgNick . message $ info
        channel = T.unpack . msgChan . message $ info
        process inf = do
          num <- liftIO $ updater (T.unpack $ (wordMsg . message) info !! 1) user channel
          noticeMsgPlain $ T.pack ("Putted " <> field <> " to " <> show num <> " files")

cmdCut :: CmdImp m => m (Effect m -> m Func)
cmdCut = do
  info <- ask
  shouldDelete info
  where
    words = wordMsg . message
    shouldDelete info
      | not (isAdmin info) = noticeMsgPlain "Not Admin"
      | isThr (words info) = executeCut info
      | otherwise          = noticeMsgPlain "something went wrong"

    executeCut info
      | "http://" `T.isPrefixOf` link
      || "https://"`T.isPrefixOf` link = nukeHOF nukeVomitsLinkUserFromDb
      | otherwise                     = nukeHOF nukeVomitsMD5UserFromDb

      where
        user    = T.unpack $ (words info) !! 2
        link    = (words info) !! 1
        channel = (T.unpack . msgChan . message $ info)
        nukeHOF hof = do
          currentVoms <- liftIO $ getUserQuantityOfVomits user channel
          voms <- liftIO $ hof link user channel
          _ <- liftIO $ traverse shredFile voms
          _ <- liftIO $ updateUserQuantityOfVomits user channel (currentVoms - (length voms))
          noticeMsgPlain ("Deleted images (" <> T.pack (show . length $ voms) <> ")")

cmdNukeMD5 :: CmdImp m => m (Effect m -> m Func)
cmdNukeMD5 = do
  info <- ask
  shouldDelete info

  where
    shouldDelete info
      | not (isAdmin info) = noticeMsgPlain "Not admin"
      | isSnd words        = nukeMD5 (T.unpack md5)
      | otherwise          = noticeMsgPlain "something went wrong"
      where
        words = wordMsg . message $ info
        md5   = words !! 1

        nukeMD5 md5 = do
          files <- liftIO $ nukeVomitByMD5 md5
          _ <- liftIO $ traverse shredFile files
          noticeMsgPlain ("Deleted " <> (T.pack . show . length $ files) <> " files")

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
  actionMsgPlain ("lewds " <> target)

-- | Causes Vomitchan to sleep ∨ awaken
cmdDream :: CmdImp m => ContFunc m
cmdDream = modifyDreamState >> privMsgPlain "dame"

-- | Causes vomit to go into fleecy mode
-- (at the moment just prints <3's instead of actual text in cmdVomit)
cmdFleecy :: CmdImp m => ContFunc m
cmdFleecy = modifyFleecyState >> privMsgPlain "dame"

-- | Causes vomit to go into Yuki ona mode
cmdYuki :: CmdImp m => ContFunc m
cmdYuki = modifyYukiState >> privMsgPlain "dame"

publishLink :: CmdImp m => String -> m (Effect m -> m Func)
publishLink filepathNotStripped = do
  -- TODO :: Hack please fix properly
  let filepath = filter (/= '\\') filepathNotStripped
  msg <- asks message
  manager <- asks manager
  state <- getChanStateM
  pure $ \contEffect -> do
    let
        randVom numT numG
          | _fleecy state = replicate numT '♥'
          | otherwise     = take numT (randElems randRange numG)

        randLink
          | _dream state = fileCheck (Just filepath)
          | otherwise    = return ""

        -- checks if there is a file to upload!
        fileCheck :: Maybe String -> IO T.Text
        fileCheck = maybe (return "") (upUsrFile manager . T.pack)

        randApply numLength randSeed =
          withUnitM (Modifier.effText (T.pack (randVom numLength randSeed)))
                    (contEffect (Extra {validEffects = effectList}))
        randApplyLink link =
          withUnitM (Modifier.effLink link)
                    (contEffect (Extra {validEffects = effectListLink}))

        nsfwStr txt
          | checkFilenameMetadata "-nsfl" (T.pack filepath) =
            withUnit
              (Modifier.effText "nsfl")
              (`withSet`
                [Modifier.Reverse, Modifier.Color Modifier.LGreen, Modifier.Bold])
          | checkFilenameMetadata "-nsfw" (T.pack filepath) =
            withUnit
              (Modifier.effText "nsfw")
              (`withSet`
                [Modifier.Reverse, Modifier.Color Modifier.LBlue, Modifier.Bold])
          | otherwise = Modifier.effText ""

        randPrivMsg = do
          x               <- liftIO $ randomRIO (8,23)
          randomList      <- liftIO (randoms <$> newStdGen :: IO [Int])
          link            <- liftIO randLink >>= randApplyLink
          startingVomText <- randApply x (head randomList)
          endingVomText   <- randApply x (randomList !! 1)
          return
            $ nsfwStr link    : Modifier.effText " "
            : startingVomText : Modifier.effText " "
            : link            : Modifier.effText " "
            : [endingVomText]
    toWrite <- randPrivMsg
    privMsg toWrite >>= ($ const pure)

-- | Vomit but random
cmdRoulette :: CmdImp m => m (Effect m -> m Func)
cmdRoulette = do
  msg      <- asks message
  filepath <- liftIO $ getRouletteVomit (T.unpack $ msgChan msg)
  publishLink filepath

-- | Vomits up a colorful rainbow if vomitchan is asleep else it just vomits up red with no link
cmdVomit :: CmdImp m => m (Effect m -> m Func)
cmdVomit = do
  msg      <- asks message
  let newUsr = changeNickFstArg msg
  filepath <-
    liftIO $ getRandomVomitPath (T.unpack $ msgNick newUsr) (T.unpack $ msgChan msg)
  publishLink filepath

-- Joins the first channel in the message if the user is an admin else do nothing
cmdJoin :: (Cmd m, Monad m') => ContFuncPure m m'
cmdJoin = asks join
  where
    join info
      | isAdmin info && isSnd msg = response ("JOIN", [Modifier.effNonModifiable (msg !! 1)])
      | otherwise                 = noResponse
      where
        msg = wordMsg . message $ info

-- Leaves the first channel in the message if the user is an admin else do nothing
cmdPart :: (Cmd m, Monad m') => ContFuncPure m m'
cmdPart = asks part
  where
    part info
      | isAdmin info && isSnd msg =
        response ("PART", [Modifier.effNonModifiable (msg !! 1)])
      | isAdmin info && "#" `T.isPrefixOf` msgChan (message info) =
        response ("PART", [Modifier.effNonModifiable (msgChan . message $ info)])
      | otherwise =
        noResponse
      where
        msg = wordMsg . message $ info

-- SLEX COMMANDS--------------------------------------------------------------------------------

cmdEightBall :: CmdImp m => ContFunc m
cmdEightBall = do
  res <- liftIO (randElem respones <$> randomIO)
  privMsgPlain res

cmdTarget f = do
  target' <- drpMsg " "
  if mempty == target' then
      return noResponse
    else do
      let target = T.tail target'
      privMsgPlain (f target)

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
yukiMode :: (MonadReader InfoPriv f, MonadIO f) => Modifier.T -> f Modifier.T
yukiMode text = f <$> getChanStateM
  where
    f state | state^.yuki = text `withSet` [Modifier.Color Modifier.Blue]
            | otherwise   = text

randomColorEffect
  :: (MonadReader InfoPriv m, MonadIO m)
  => Modifier.T -> m Modifier.T
randomColorEffect text =
  f <$> getChanStateM
  where
    f state
      | not (state^.yuki) && (state^.dream) = do
          text `withIndividual` [Modifier.ansiColor]
      | otherwise =
          text

randomEffect
  :: (MonadReader InfoPriv m, MonadIO m)
  => Modifier.ChoiceEffects -> Modifier.T -> m Modifier.T
randomEffect validEffects text =
  pure $ text `withIndividual` [validEffects]

dryEffect :: (MonadReader InfoPriv f, MonadIO f) => Modifier.T -> f Modifier.T
dryEffect text =
  f <$> getChanStateM
  where
    f state
      | not (state^.dream) = text `withSet` [Modifier.Color Modifier.Red, Modifier.Reverse]
      | otherwise        = text

-- TODO ∷ need to make the effect not run on texts
heartify = undefined

--- HELPER FUNCTIONS --------------------------------------------------------------------------

-- | checks if the user is an admin
isAdmin :: InfoPriv -> Bool
isAdmin info = (nick <> "@" <> host) `elem` (netAdmins . network $ info)
  where
    host = maybe "" id (msgHost . message $ info)
    nick = msgNick . message $ info

-- generates the randomRange for the cmdVomit command
randRange :: V.Vector Char
randRange = (chr <$>) . V.fromList $  [8704..8959]   -- Mathematical symbols
                                   <> [32..75]       -- parts of the normal alphabet and some misc symbols
                                   <> [10627,10628]  -- obscure braces
                                   <> [10631, 10632] -- obscure braces
                                   <> [945..969]     -- greek symbols
                                   <> decimateBy 70 [19968..40959] -- Chinese range 1
                                   <> decimateBy 23 [13312..19893] -- Chinese range 2

decimateBy :: Int -> [a] -> [a]
decimateBy _ [] = []
decimateBy a (x:xs) =
  case drop a (x : xs) of
    x : xs -> x : decimateBy a xs
    [    ] -> [x]

respones = V.fromList [ "It is certain"
                      , "It is decidedly so"
                      , "Without a doubt"
                      , "Yes definitely"
                      , "You may rely on it"
                      , "As I see it, yes"
                      , "Most likely"
                      , "Outlook good"
                      , "Yes"
                      , "Signs point to yes"
                      , "Reply hazy try again"
                      , "Ask again later"
                      , "Better not tell you now"
                      , "Cannot predict now"
                      , "Concentrate and ask again"
                      , "Don't count on it"
                      , "My reply is no" ]

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
             T.Text -> Message -> m1 (Effect m2 -> m2 Func)
composeMsg method str = do
  dest <- asks (msgDest . message)
  pure $ \eff -> do
    -- if we have a message append nothing to it
    -- actions need to append to the text to properly work
    let appendAction =
          case str of
            Message _ -> id
            Action  _ -> actionMe
    sentBack <- eff (Extra V.empty) (unboxMessage str)
    return
      $ Response (method
                 , Modifier.effNonModifiable (dest <> " :") : appendAction sentBack)

unboxMessage :: Message -> Modifier.T
unboxMessage (Action  t) = t
unboxMessage (Message t) = t

-- Some aliases for composeMsg

-- | @privMsg@, @actionMsg@, and @noticeMsg@ refer to the kinds of
-- messages that we send. The default one is for ones where effects may
-- be applied to texts atomatically.
privMsg, actionMsg, noticeMsg
  :: (MonadReader InfoPriv m1, Monad m2)
  => Modifier.T
  -> m1 (Effect m2 -> m2 Func)
privMsg = composeMsg "PRIVMSG" . Message

actionMsg = composeMsg "PRIVMSG" . Action

noticeMsg = composeMsg "NOTICE" . Message

-- | the @Plain@ variants expect normal text with no effects applies to
-- them. This occurs quite often when the text just has the effect
-- handler applies to it.
privMsgPlain, actionMsgPlain, noticeMsgPlain
  :: (MonadReader InfoPriv m1, Monad m2)
  => T.Text
  -> m1 (Effect m2 -> m2 Func)
privMsgPlain = privMsg . return . Modifier.effText

actionMsgPlain = actionMsg . return . Modifier.effText

noticeMsgPlain = noticeMsg . return . Modifier.effText

-- Used for /me commands
actionMe :: Modifier.T -> Modifier.T
actionMe txt =
  Modifier.effNonModifiable "\0001ACTION " : txt <> [Modifier.effNonModifiable "\0001"]

-- | applies TextEffects to all modifiers in the stack
with :: Modifier.T -> [Modifier.Occurence] -> Modifier.T
with mod newEffs =
  fmap (`Modifier.addBlockEffs` newEffs) mod

-- | applies TextEffects to all modifiers in the stack with a set
-- effect instead of randomly picking one out of a list
withSet :: Modifier.T -> [Modifier.TextEffects] -> Modifier.T
withSet mod = with mod . fmap Modifier.Set

withUnitM ::
  Functor f => Modifier.Unit -> (Modifier.T -> f Modifier.T) -> f Modifier.Unit
withUnitM unit f =
  head <$> f (return unit)

withUnit :: Modifier.Unit -> (Modifier.T -> Modifier.T) -> Modifier.Unit
withUnit unit f =
  head $ f $ return unit

-- | @withIndividual@ applies the effect modifier for every modifier action
withIndividual :: Modifier.T -> [Modifier.ChoiceEffects] -> Modifier.T
withIndividual mod newEffs =
  fmap (`Modifier.addIndividualEffs` newEffs) mod

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

isThr (_ : _ : _ : _) = True
isThr _               = False

frt :: (a, b, c, d) -> d
frt (_, _, _, d) = d

-- converts a message into a list containing a list of the contents based on words
wordMsg :: PrivMsg -> [T.Text]
wordMsg = T.words . msgContent


