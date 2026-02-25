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
    -- * Upload examples
  , exPomfSuccess, exPomfEmpty, exPomfFail, exPomfGarbage
  , exSecretParts, exUploaderParts
    -- * Database testing
  , withTestDb, seedTestDb
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
import qualified Network.HTTP.Client.MultipartFormData as MFD

import Bot.FileOps       (decodePomfUrl, secretPart, uploaderPart)
import Bot.Database      (createSchema, addChannelConn, addUserConn, addVomitConn)

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
-- Upload examples
--------------------------------------------------------------------------------

-- | Successful pomf response — extracts the URL.
--
-- >>> exPomfSuccess
-- Just "https://example.com/abc.png"
exPomfSuccess :: Maybe T.Text
exPomfSuccess = decodePomfUrl
  "{\"success\":true,\"files\":[{\"hash\":\"abc\",\"name\":\"abc.png\",\"url\":\"https://example.com/abc.png\",\"size\":1234}]}"

-- | Pomf response with empty files list.
--
-- >>> exPomfEmpty
-- Nothing
exPomfEmpty :: Maybe T.Text
exPomfEmpty = decodePomfUrl "{\"success\":true,\"files\":[]}"

-- | Pomf error response.
--
-- >>> exPomfFail
-- Nothing
exPomfFail :: Maybe T.Text
exPomfFail = decodePomfUrl
  "{\"success\":false,\"errorcode\":400,\"description\":\"bad request\"}"

-- | Garbage input.
--
-- >>> exPomfGarbage
-- Nothing
exPomfGarbage :: Maybe T.Text
exPomfGarbage = decodePomfUrl "not json"

-- | Secret part produces one form field when present, none when absent.
--
-- >>> length (exSecretParts Nothing)
-- 0
-- >>> length (exSecretParts (Just "my-secret"))
-- 1
exSecretParts :: Maybe T.Text -> [MFD.Part]
exSecretParts = secretPart

-- | Uploader part always produces one form field.
--
-- >>> length (exUploaderParts "nick")
-- 1
exUploaderParts :: T.Text -> [MFD.Part]
exUploaderParts = uploaderPart

--------------------------------------------------------------------------------
-- Database testing
--------------------------------------------------------------------------------

-- | Run a test action with an in-memory SQLite DB that has the schema ready.
--
-- >>> withTestDb $ \conn -> seedTestDb conn >> getUserQuantityOfVomitsConn conn "nick" "#test"
withTestDb :: (Connection -> IO a) -> IO a
withTestDb action = withConnection ":memory:" $ \conn -> do
  createSchema conn
  action conn

-- | Populate a test DB with sample data.
seedTestDb :: Connection -> IO ()
seedTestDb conn = do
  addChannelConn conn "#test"
  addUserConn conn "nick" "#test"
  addVomitConn conn "nick" "#test" "abc123" "/data/logs/#test/nick/file.jpg"
