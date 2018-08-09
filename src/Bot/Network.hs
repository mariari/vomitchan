{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.Network (
  IRCNetwork,
  readNetworks,
  saveNetworks,
  joinNetwork,
  findNetwork,
  netServer,
  netState
) where
--- IMPORTS -----------------------------------------------------------------------------------
import           Control.Monad
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Lazy  as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC (putStrLn)
import qualified Data.Text             as T
import           GHC.Generics
import qualified Network.Connection    as C
import           Data.Monoid
import qualified Data.ByteString.Base64 as BS64
import qualified Data.Text.Encoding as TE

import           Control.Exception (try,SomeException)
import           Data.Foldable
import           Bot.MessageType
import           Bot.MessageParser
import           Bot.Socket
import           Bot.StateType
import           Bot.FileOps
--- DATA STRUCTURES ---------------------------------------------------------------------------

-- IRC network table
data IRCNetwork = IRCNetwork
             { netServer :: Server
             , netPort   :: Port
             , netSSL    :: Bool
             , netNick   :: Nick
             , netPass   :: Pass
             , netChans  :: [Chan]
             , netState  :: StateConfig
             } deriving (Show,Generic)

-- allow encoding to/from JSON
instance JSON.FromJSON IRCNetwork
instance JSON.ToJSON IRCNetwork


--- FUNCTIONS ---------------------------------------------------------------------------------

-- read IRC networks from file
readNetworks :: FilePath -> IO (Maybe [IRCNetwork])
readNetworks file = do
  jsonData <- JSON.eitherDecode <$> B.readFile file :: IO (Either String [IRCNetwork])
  case jsonData of
    Left  err  -> putStrLn err >> return Nothing
    Right nets -> return $ Just nets

-- save IRC networks to file
saveNetworks :: FilePath -> [IRCNetwork] -> IO ()
saveNetworks file nets = B.writeFile file (JSON.encode nets)

-- joins a network and returns a handle
joinNetwork :: IRCNetwork -> IO (Maybe C.Connection)
joinNetwork net = do
  ctx <- C.initConnectionContext
  con <- try $ C.connectTo ctx C.ConnectionParams { C.connectionHostname  = T.unpack $ netServer net
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
      if netSSL net
        then writeBS con ("CAP", "LS 302")
        else return ()
      write con ("NICK", netNick net)
      write con ("USER", netNick net <> " 0 * :connected")
      unless (netPass net == "" && not (netSSL net))
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

    encode   = BS64.encode . TE.encodeUtf8
    saslPass = netNick net <> "\0" <> netNick net <> "\0" <> netPass net
--- HELPER FUNCTIONS / UNUSED -----------------------------------------------------------------

-- finds a network by name and maybe returns it
findNetwork :: [IRCNetwork] -> Server -> Maybe IRCNetwork
findNetwork (nt:nts) sv
  | netServer nt == sv = Just nt
  | null nts           = Nothing
  | otherwise          = findNetwork nts sv
