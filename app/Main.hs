{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

-- TODO: remove TODOs
-- TODO: make error messages more useful

-- IMPORTS ------------------------------------------------------------------------------------

import           Control.Monad
import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.Format     as T
import qualified Data.Text.IO         as T
import           GHC.Generics
import           Network
import           System.Exit
import           System.IO


-- TYPES --------------------------------------------------------------------------------------

-- types for IRC data
type Server = String
type Port   = Integer
type Nick   = String
type User   = String
type Host   = String
type Pass   = String
type Chan   = String

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

-- IRC message structure
data Message = Message
             { msgNick    :: Nick
             , msgUser    :: User
             , msgHost    :: Host
             , msgChan    :: Chan
             , msgContent :: T.Text
             } deriving (Show)

-- converts a string to a Message
toMessage :: T.Text -> Message
toMessage str = Message nick user host chan content
    where
        (head:content:_) = T.splitOn ":" (T.drop 1 str)
        (nick:_:user:host:_:chan:_) = map T.unpack $ T.split delims head
        delims c = case c of
                        ' ' -> True
                        '!' -> True
                        '~' -> True
                        '@' -> True
                        _   -> False


-- IRC NETWORK TABLE --------------------------------------------------------------------------

-- read IRC networks from file
readNetworks ∷ FilePath → IO (Maybe [IRCNetwork])
readNetworks file = do
  d <- (JSON.eitherDecode <$> B.readFile file) :: IO (Either String [IRCNetwork])
  case d of
       Left err -> do
         putStrLn err
         return Nothing
       Right ht -> return $ Just ht

-- save IRC networks to file
saveNetworks ∷ FilePath → [IRCNetwork] → IO ()
saveNetworks file nts = B.writeFile file (JSON.encode nts)


-- SOCKET OPERATIONS --------------------------------------------------------------------------

-- takes a String, sends to socket
write ∷ Handle → (String, String) → IO ()
write h (act,args) = do
  T.hprint h "{} {}\r\n" [act, args]
  T.print "> {} {}\n" [act, args]

-- handles privmsgs and creates responses
handlePM ∷ Message → Maybe String
handlePM s
  | prefix ".quit" && msgUser s == "MrDetonia" = Just "QUIT"
  | prefix ".bots" = Just $ msgChan s ++ " :I am a bot written by MrDetonia in Haskell"
  | otherwise = Nothing
  where
    prefix p = p `T.isPrefixOf` msgContent s

-- takes an IRC message and generates the correct response
respond ∷ T.Text → Maybe (String, String)
respond s
  | "PING: " `T.isPrefixOf` s = Just ("PONG", ':' : drop 6 (T.unpack s))
  | "PRIVMSG" `T.isInfixOf` s = do
    let res = handlePM $ toMessage s
    case res of
         Just msg -> Just ("PRIVMSG", msg)
         Nothing  -> Nothing
  | otherwise = Nothing

-- simply listens to a socket forever
listen ∷ Handle → IO ()
listen h = forever $ do
  s <- T.hGetLine h
  T.putStrLn s
  let res = respond s
  case res of
       Just x  -> if snd x == "QUIT"
                     then write h ("QUIT",":Exiting") >> exitSuccess
                     else write h x
       Nothing -> return ()

-- finds a network by name and maybe returns it
findNetwork ∷ [IRCNetwork] → Server → Maybe IRCNetwork
findNetwork (nt:nts) sv
  | netServer nt == sv = Just nt
  | null nts           = Nothing
  | otherwise          = findNetwork nts sv

-- joins a network by name and maybe returns a handle
joinNetwork ∷ [IRCNetwork] → Server → IO (Maybe Handle)
joinNetwork nts sv = do
  let nt = findNetwork nts sv
  case nt of
       Nothing -> return Nothing
       Just nt -> do
         h <- connectTo (netServer nt) (PortNumber (fromIntegral (netPort nt)))
         hSetBuffering h NoBuffering
         write h ("NICK", netNick nt)
         write h ("USER", netNick nt ++ " 0 * :connected")
         write h ("NICKSERV :IDENTIFY", netPass nt)
         waitForAuth h
         mapM_ (write h) (zip (repeat "JOIN") (netChans nt))
         return $ Just h
  where
    waitForAuth h = do
      s <- T.hGetLine h
      T.putStrLn s
      unless (":You are now identified" `T.isInfixOf` s) (waitForAuth h)


-- ENTRY POINT --------------------------------------------------------------------------------

main ∷ IO ()
main = do
  nts <- readNetworks "data/networks.json"
  case nts of
       Nothing  -> putStrLn "ERROR loading servers from JSON"
       Just nts -> do
         h <- joinNetwork nts "irc.freenode.net"
         case h of
              Nothing -> putStrLn "ERROR: could not find server"
              Just h  -> listen h
