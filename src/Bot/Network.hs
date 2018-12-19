module Bot.Network (
  IRCNetwork,
  readNetworks,
  saveNetworks,
  joinNetwork,
  startNetwork,
  reconnectNetwork,
  findNetwork,
  netServer,
  netState
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Aeson             as JSON
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Char8  as BC (putStrLn)
import qualified Data.Text              as T
import qualified Network.Connection     as C
import qualified Data.ByteString.Base64 as BS64
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Foldable          (traverse_)
import           Control.Monad          (when)
import           Control.Exception      (try, SomeException)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.MVar
import           Control.Concurrent(threadDelay)

import Bot.MessageType
import Bot.MessageParser
import Bot.Socket
import Bot.StateType
import Bot.FileOps
import Bot.NetworkType
import Bot.Servers

-- read IRC networks from file
readNetworks :: FilePath -> IO (Maybe [IRCNetwork])
readNetworks file = do
  jsonData <- JSON.eitherDecodeFileStrict' file
  case jsonData of
    Left  err  -> putStrLn err >> return Nothing
    Right nets -> return $ Just nets

-- save IRC networks to file
saveNetworks :: FilePath -> [IRCNetwork] -> IO ()
saveNetworks file nets = BL.writeFile file (JSON.encode nets)

-- joins a network and returns a handle
joinNetwork :: C.ConnectionContext -> IRCNetwork -> IO (Maybe C.Connection)
joinNetwork ctx net = do
  con <- try
       . C.connectTo ctx
       $ C.ConnectionParams { C.connectionHostname  = T.unpack $ netServer net
                            , C.connectionPort      = fromIntegral $ netPort net
                            , C.connectionUseSecure = Just $ C.TLSSettingsSimple False False True
                            , C.connectionUseSocks  = Nothing
                            } :: IO (Either SomeException C.Connection)
  case con of
    Left ex -> putStrLn (show ex) >> return Nothing
    Right con -> do
      passConnect con
      traverse_ (write con) (zip (repeat "JOIN") (netChans net))
      return (Just con)
  where
    passConnect con = do
      when (netSSL net)
           (writeBS con ("CAP", "LS 302"))
      write con ("NICK", netNick net)
      write con ("USER", netNick net <> " 0 * :connected")
      when (netPass net /= "" && netSSL net)
           (write con ("NICKSERV :IDENTIFY", netPass net))
      waitNext con

    waitNext h = do
      line <- C.connectionGetLine 10240 h
      BC.putStrLn line
      case parseMessage line of
        Left err  -> appendError err line >> print err
        Right msg -> case msg of
          PING (Ping s)          -> write h ("PONG", s) >> waitNext h
          OTHER "AUTHENTICATE" _ -> writeBS h ("AUTHENTICATE", encode saslPass) >> waitNext h
          NUMBERS (N904 _ _)     -> writeBS h ("CAP", "END") -- authentication failed
          NUMBERS (N903 _ _)     -> writeBS h ("CAP", "END") -- authentication succeeded
          NUMBERS (N376 _ _)     -> return ()                -- if we don't sasl we wait until we see the MOTD
          OTHER "CAP" (OtherServer _ content)
            | " * LS" `T.isPrefixOf` content -> writeBS h ("CAP", "REQ :sasl") >> waitNext h
            | otherwise                      -> writeBS h ("AUTHENTICATE", "PLAIN") >> waitNext h
          _                     -> waitNext h

    encode   = BS64.encode . encodeUtf8
    saslPass = netNick net <> "\0" <> netNick net <> "\0" <> netPass net

startNetwork :: AllServers -> C.ConnectionContext -> IRCNetwork -> IO (Maybe (C.Connection, MVar Quit))
startNetwork allS ctx network = do
  mjoined <- joinNetwork ctx network
  case mjoined of
    Just x -> do
      mvar <- newEmptyMVar
      atomically (addConnected allS mvar network)
      return (Just (x, mvar))
    Nothing -> do
      atomically (addDisconnected allS network)
      return Nothing

reconnectNetwork :: AllServers -> C.ConnectionContext -> IRCNetwork -> IO (C.Connection, MVar Quit)
reconnectNetwork allS ctx network = recurse 1000
  where
    recurse i = do
      mjoined <- joinNetwork ctx network
      case mjoined of
        Nothing -> do
          threadDelay 1000
          recurse (i * 2)
        Just x -> do
          mmvar <- atomically (previousMvar allS network)
          case mmvar of
            Nothing -> do
              mvar <- newEmptyMVar
              return (x,mvar)
            Just mvar -> return (x,mvar)

--- HELPER FUNCTIONS / UNUSED -----------------------------------------------------------------

-- finds a network by name and maybe returns it
findNetwork :: [IRCNetwork] -> Server -> Maybe IRCNetwork
findNetwork []       _ = Nothing
findNetwork (nt:nts) sv
  | netServer nt == sv = Just nt
  | null nts           = Nothing
  | otherwise          = findNetwork nts sv
