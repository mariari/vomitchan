{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}


--- MODULE DEFINITION ---
module Bot.Network (
  IRCNetwork,
  readNetworks,
  saveNetworks,
  joinNetwork,
  findNetwork
) where


--- IMPORTS ---
import           Control.Monad
import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.Format     as T
import qualified Data.Text.IO         as T
import           GHC.Generics
import           Network
import           System.IO

import           Bot.Message
import           Bot.Socket


--- DATA STRUCTURES ---

-- IRC network table
data IRCNetwork = IRCNetwork
             { netServer :: Server
             , netPort   :: Port
             , netNick   :: Nick
             , netPass   :: Pass
             , netChans  :: [Chan]
             } deriving (Show,Generic)


-- allow encoding to/from JSON
instance JSON.FromJSON IRCNetwork
instance JSON.ToJSON IRCNetwork


--- FUNCTIONS ---

-- read IRC networks from file
readNetworks ∷ FilePath → IO (Maybe [IRCNetwork])
readNetworks file = do
    jsonData <- (JSON.eitherDecode <$> B.readFile file) :: IO (Either String [IRCNetwork])

    case jsonData of
         Left err -> do
             putStrLn err
             return Nothing
         Right nets -> return $ Just nets


-- save IRC networks to file
saveNetworks ∷ FilePath → [IRCNetwork] → IO ()
saveNetworks file nets = B.writeFile file (JSON.encode nets)


-- joins a network and returns a handle
joinNetwork ∷ IRCNetwork → IO Handle
joinNetwork net = do
  h <- connectTo (T.unpack $ netServer net) (PortNumber (fromIntegral (netPort net)))
  hSetBuffering h NoBuffering

  write h ("NICK", netNick net)
  write h ("USER", netNick net `T.append` " 0 * :connected")
  unless (netPass net == "") (write h ("NICKSERV :IDENTIFY", netPass net) >> waitForAuth h)
  mapM_ (write h) (zip (repeat "JOIN") (netChans net))

  return h

  where
    waitForAuth h = do
      line <- T.hGetLine h
      T.putStrLn line
      unless (":You are now identified" `T.isInfixOf` line) (waitForAuth h)


--- HELPER FUNCTIONS / UNUSED ---

-- finds a network by name and maybe returns it
findNetwork ∷ [IRCNetwork] → Server → Maybe IRCNetwork
findNetwork (nt:nts) sv
    | netServer nt == sv = Just nt
    | null nts           = Nothing
    | otherwise          = findNetwork nts sv
