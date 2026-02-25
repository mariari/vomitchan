module Bot.Examples
  ( -- * Parser examples
    exPrivmsg, exNotice, exPing, exJoin, exQuit, exPart, exTopic
  , parseAll
    -- * Type constructors
  , sampleUser, sampleMsg, mkMsg, sampleNetwork
    -- * Modifier examples
  , exPlain, exBold, exItalics, exRainbow
  , exEmptyScope, exNoOverride, exFixOverride
  , exStartColor, exAnsiColor, exAlternating
    -- * Render helpers
  , render, renderDebug
    -- * State examples
  , stateDefault, stateMuted, stateAllOff, stateYuki
    -- * GlobalState helpers (IO-based, since STM map can't be shown purely)
  , mkGlobalState, dumpState, showState
    -- * Mock environment building blocks
  , sampleStateEntries, mkEnv, mockEnv
    -- * Command testing
  , testCmd, testCmdWith
    -- * Database examples
    -- $db-examples
  , withTestDb
  , exDbSetup, exDbAddVomit, exDbLinkVomit, exDbAddSecondUser
  , exDbFixCounts, exDbNukeMD5, exDbNukeUser
  , allUsers, allVomits
  , module Database.SQLite.Simple
  ) where

import qualified Data.ByteString         as BS
import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified StmContainers.Map       as SM
import qualified Network.HTTP.Client     as Client
import qualified Network.HTTP.Client.TLS as ClientT
import qualified ListT

import           Control.Concurrent.STM  (atomically)
import           Control.Monad.Reader    (runReaderT)
import           Data.Foldable           (traverse_)

import Database.SQLite.Simple

import Bot.MessageParser (parseMessage)
import Bot.MessageType
import Bot.NetworkType
import Bot.StateType
import Bot.Modifier
import Bot.Servers       (initAllServer)
import Bot.EffType       (Func)
import Bot.Commands      (runCmd)
import Bot.Database

--------------------------------------------------------------------------------
-- Parser examples
--------------------------------------------------------------------------------

exPrivmsg :: BS.ByteString
exPrivmsg = ":nick!user@host PRIVMSG #channel :hello world"

exNotice :: BS.ByteString
exNotice = ":nick!user@host NOTICE #channel :this is a notice"

-- Note: the ping parser does not strip the colon prefix
exPing :: BS.ByteString
exPing = "PING :irc.example.net"

exJoin :: BS.ByteString
exJoin = ":nick!user@host JOIN :#channel"

exQuit :: BS.ByteString
exQuit = ":nick!user@host QUIT :leaving"

exPart :: BS.ByteString
exPart = ":nick!user@host PART #channel :goodbye"

exTopic :: BS.ByteString
exTopic = ":nick!user@host TOPIC #channel :new topic"

parseAll :: [(String, Either String Command)]
parseAll =
  [ ("PRIVMSG", parseMessage exPrivmsg)
  , ("NOTICE",  parseMessage exNotice)
  , ("PING",    parseMessage exPing)
  , ("JOIN",    parseMessage exJoin)
  , ("QUIT",    parseMessage exQuit)
  , ("PART",    parseMessage exPart)
  , ("TOPIC",   parseMessage exTopic)
  ]

--------------------------------------------------------------------------------
-- Type constructors
--------------------------------------------------------------------------------

sampleUser :: UserI
sampleUser = UserI "nick" (Just "user") (Just "host")

sampleMsg :: PrivMsg
sampleMsg = PrivMsg sampleUser "#test" "hello world"

mkMsg :: T.Text -> PrivMsg
mkMsg = PrivMsg sampleUser "#test"

sampleNetwork :: IRCNetwork
sampleNetwork = IRCNetwork
  { netServer = "irc.example.net"
  , netPort   = 6697
  , netSSL    = True
  , netNick   = "vomitchan"
  , netPass   = ""
  , netAdmins = ["nick@host"]
  , netIgnore = []
  , netBans   = []
  , netChans  = [("#test", Options Nothing)]
  , netState  = StateConfig [("#test", defaultChanState)]
  , netUpload = Nothing
  }

--------------------------------------------------------------------------------
-- Modifier examples (migrated from test/Modifier.hs)
--------------------------------------------------------------------------------

exPlain :: Unit
exPlain = Unit (Scope [] []) (Text "Hello My Name is Bob")

exBold :: Unit
exBold = Unit (Scope [Set Bold] []) (Text "Hello My Name is Bob")

exItalics :: Unit
exItalics = Unit (Scope [Set Italics] []) (Text "Hello My Name is Bob")

exRainbow :: Unit
exRainbow = Unit (Scope [] [ansiColor]) (Text "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZz")

exEmptyScope :: Unit
exEmptyScope = Unit (Scope [] []) (Text "Hello My Name is Bob")

exNoOverride :: Unit
exNoOverride =
  Unit (Scope { block      = [Set Italics, Set (Color White)]
              , individual = [V.fromList [Color White]]
              })
       (Text "Hello My Name is Bob")

exFixOverride :: Unit
exFixOverride =
  Unit (Scope { block      = [Set Italics, Set (Color White)]
              , individual = [V.fromList [Color Black]]
              })
       (Text "Hello My Name is Bob")

exStartColor :: Unit
exStartColor =
  Unit (Scope { block = [], individual = [V.fromList [Color Black]] })
       (Text "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZz")

exAnsiColor :: Unit
exAnsiColor =
  Unit (Scope { block = [], individual = [ansiColor] })
       (Text "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZz")

exAlternating :: Unit
exAlternating =
  Unit (Scope { block      = []
              , individual = [V.fromList [Italics, Bold], V.fromList [Bold]]
              })
       (Text "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZz")

--------------------------------------------------------------------------------
-- Render helpers
--------------------------------------------------------------------------------

render :: Unit -> IO T.Text
render = applyEffects

renderDebug :: Unit -> IO String
renderDebug unit = show <$> applyEffects unit

--------------------------------------------------------------------------------
-- State examples
--------------------------------------------------------------------------------

stateDefault :: HashStorage
stateDefault = defaultChanState

stateMuted :: HashStorage
stateMuted = HashStorage True True False False

stateAllOff :: HashStorage
stateAllOff = HashStorage False False False False

stateYuki :: HashStorage
stateYuki = HashStorage True False False True

--------------------------------------------------------------------------------
-- GlobalState helpers
--------------------------------------------------------------------------------

-- | Build a 'GlobalState' from a list of @(key, HashStorage)@ pairs.
--
-- >>> gs <- mkGlobalState [("net#chan", defaultChanState)]
-- >>> dumpState gs
-- [("net#chan",HashStorage {_dream = True, ...})]
mkGlobalState :: [(T.Text, HashStorage)] -> IO GlobalState
mkGlobalState entries = do
  m <- SM.newIO
  atomically $ traverse_ (\(k, v) -> SM.insert v k m) entries
  pure $ toGlobalState m

-- | Materialise a 'GlobalState' into an inspectable list.
-- This is the IO escape hatch for the STM map that 'Show' can't provide.
dumpState :: GlobalState -> IO [(T.Text, HashStorage)]
dumpState gs = atomically $ ListT.toList $ SM.listT (hash gs)

-- | Pretty-print a 'GlobalState' — handy in GHCi.
--
-- >>> gs <- mkGlobalState sampleStateEntries
-- >>> showState gs >>= putStrLn
-- GlobalState [("irc.example.net#test",HashStorage {_dream = True, ...})]
showState :: GlobalState -> IO String
showState gs = do
  pairs <- dumpState gs
  pure $ "GlobalState " ++ show pairs

--------------------------------------------------------------------------------
-- Mock environment building blocks
--------------------------------------------------------------------------------

-- | The default channel entries for 'mockEnv'.
-- Key format is @server <> channel@ (see 'Bot.State.getHashText').
sampleStateEntries :: [(T.Text, HashStorage)]
sampleStateEntries = [("irc.example.net#test", defaultChanState)]

-- | Build a mock environment from custom state entries.
-- Lets you test with different channel configurations:
--
-- >>> env <- mkEnv [("irc.example.net#test", stateMuted)]
-- >>> -- commands in #test now see mute = True
mkEnv :: [(T.Text, HashStorage)] -> IO (PrivMsg -> InfoPriv)
mkEnv entries = do
  state   <- mkGlobalState entries
  servers <- initAllServer
  mgr     <- Client.newManager ClientT.tlsManagerSettings
  pure $ \msg -> Info msg "irc.example.net" state mgr sampleNetwork servers

-- | 'mkEnv' with 'sampleStateEntries' — the quickest way to get going.
mockEnv :: IO (PrivMsg -> InfoPriv)
mockEnv = mkEnv sampleStateEntries

--------------------------------------------------------------------------------
-- Command testing
--------------------------------------------------------------------------------

-- | Run a command through the full dispatch pipeline using the default
-- mock environment.
testCmd :: PrivMsg -> IO Func
testCmd = testCmdWith sampleStateEntries

-- | Run a command with custom state entries.
--
-- >>> testCmdWith [("irc.example.net#test", stateYuki)] (mkMsg ".8ball")
testCmdWith :: [(T.Text, HashStorage)] -> PrivMsg -> IO Func
testCmdWith entries msg = do
  env <- mkEnv entries
  runReaderT runCmd (env msg)

--------------------------------------------------------------------------------
-- Database examples
--------------------------------------------------------------------------------

-- $db-examples
--
-- Each example is standalone — it sets up its own prerequisites internally.
-- Run any of them in GHCi with 'withTestDb':
--
-- >>> withTestDb $ \c -> exDbAddVomit c >> allVomits c
-- [DBVomit {vomitId = 1, vomitPath = "./data/logs/#test/nick/file.jpg", ...}]
--
-- >>> withTestDb $ \c -> exDbAddSecondUser c >> allUsers c
-- [DBUser {..., userName = "nick", ...}, DBUser {..., userName = "alice", ...}]

-- | Run an action with a fresh in-memory SQLite DB (schema already created).
withTestDb :: (Connection -> IO a) -> IO a
withTestDb action = withConnection ":memory:" $ \conn -> do
  createSchema conn
  action conn

-- | Create a channel and a user.
--
-- >>> withTestDb $ \c -> exDbSetup c >> getUserQuantityOfVomitsConn c "nick" "#test"
-- 0
exDbSetup :: Connection -> IO ()
exDbSetup conn = do
  addChannelConn conn "#test"
  addUserConn conn "nick" "#test"

-- | Download a file — add a vomit record and bump the count.
--
-- >>> withTestDb $ \c -> exDbAddVomit c >> allVomits c
-- [DBVomit {vomitId = 1, vomitPath = "./data/logs/#test/nick/file.jpg", vomitMD5 = "abc123", ...}]
exDbAddVomit :: Connection -> IO ()
exDbAddVomit conn = do
  exDbSetup conn
  addVomitConn conn "nick" "#test" "abc123" "./data/logs/#test/nick/file.jpg"
  succUserQuantityOfVomitsConn conn "nick" "#test"

-- | Cache an upload link on a vomit.
--
-- >>> withTestDb $ \c -> exDbLinkVomit c >> getLinkConn c "./data/logs/#test/nick/file.jpg"
-- Just "https://example.com/file.jpg"
exDbLinkVomit :: Connection -> IO ()
exDbLinkVomit conn = do
  exDbAddVomit conn
  updateLinkConn conn "./data/logs/#test/nick/file.jpg" "https://example.com/file.jpg"

-- | A second user joins and posts a file.
--
-- >>> withTestDb $ \c -> exDbAddSecondUser c >> allUsers c
-- [DBUser {..., userName = "nick", ...}, DBUser {..., userName = "alice", ...}]
exDbAddSecondUser :: Connection -> IO ()
exDbAddSecondUser conn = do
  exDbAddVomit conn
  addUserConn conn "alice" "#test"
  addVomitConn conn "alice" "#test" "xyz789" "./data/logs/#test/alice/pic.png"
  succUserQuantityOfVomitsConn conn "alice" "#test"

-- | Simulate a drifted count — add a vomit without bumping, then fix.
--
-- >>> withTestDb $ \c -> exDbFixCounts c >> getUserQuantityOfVomitsConn c "nick" "#test"
-- 2
exDbFixCounts :: Connection -> IO ()
exDbFixCounts conn = do
  exDbAddVomit conn
  addVomitConn conn "nick" "#test" "def456" "./data/logs/#test/nick/file2.png"
  -- count is now wrong (1, should be 2) — fix it
  fixQuantityOfVomitsConn conn

-- | Nuke a vomit by its md5 hash — returns the deleted file paths.
--
-- >>> withTestDb $ \c -> exDbNukeMD5 c
-- ["./data/logs/#test/nick/file.jpg"]
exDbNukeMD5 :: Connection -> IO [String]
exDbNukeMD5 conn = do
  exDbAddVomit conn
  nukeVomitByMD5FixConn conn "abc123"

-- | Remove a user entirely — vomits first, then the user record.
--
-- >>> withTestDb $ \c -> exDbNukeUser c >> allUsers c
-- []
exDbNukeUser :: Connection -> IO ()
exDbNukeUser conn = do
  exDbAddVomit conn
  nukeVomitsOfUserFromDbConn conn "nick" "#test"
  nukeUserFromDbConn conn "nick" "#test"

-- | Dump all users in the DB — handy for inspecting state between steps.
allUsers :: Connection -> IO [DBUser]
allUsers conn = query_ conn "SELECT * FROM user" :: IO [DBUser]

-- | Dump all vomits in the DB.
allVomits :: Connection -> IO [DBVomit]
allVomits conn = query_ conn "SELECT * FROM vomits" :: IO [DBVomit]
