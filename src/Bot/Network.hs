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


import           Data.Foldable
import           Bot.MessageType
import           Bot.Socket
import           Bot.StateType
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
joinNetwork :: IRCNetwork -> IO C.Connection
joinNetwork net = do
  ctx <- C.initConnectionContext
  con <- C.connectTo ctx C.ConnectionParams { C.connectionHostname  = T.unpack $ netServer net
                                            , C.connectionPort      = fromIntegral $ netPort net
                                            , C.connectionUseSecure = Just $ C.TLSSettingsSimple False False True
                                            , C.connectionUseSocks  = Nothing
                                            }
  passConnect con
  traverse_ (write con) (zip (repeat "JOIN") (netChans net))
  return con
  where
    waitForX str h = do
      line <- C.connectionGetLine 10240 h
      BC.putStrLn line
      unless (str `BS.isInfixOf` line) (waitForX str h)

    waitForAuth = waitForX ":You are now identified"
    waitForSASL = waitForX ":account-notify"
    waitForPlus = waitForX "AUTHENTICATE +"
    waitForJoin = waitForX "you have registered"
    waitForHost = waitForX ":*** Looking up your hostname..."

    passConnect con
      | not (netSSL net) = do
          write con ("NICK", netNick net)
          write con ("USER", netNick net <> " 0 * :connected")
          unless (netPass net == "")
                 (write con ("NICKSERV :IDENTIFY", netPass net) >> waitForAuth con)
      | otherwise = do
          write con ("CAP", "LS 302")
          write con ("CAP REQ", "sasl")
          write con ("NICK", netNick net)
          write con ("USER", netNick net <> " 0 * :connected")
          waitForSASL con
          write con ("AUTHENTICATE", "PLAIN")
          waitForPlus con
          write con ("AUTHENTICATE", netPass net)
          waitForJoin con
          write con ("CAP", "END")
--- HELPER FUNCTIONS / UNUSED -----------------------------------------------------------------

-- finds a network by name and maybe returns it
findNetwork :: [IRCNetwork] -> Server -> Maybe IRCNetwork
findNetwork (nt:nts) sv
  | netServer nt == sv = Just nt
  | null nts           = Nothing
  | otherwise          = findNetwork nts sv
