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
type Server = T.Text
type Port   = Integer
type Nick   = T.Text
type User   = T.Text
type Host   = T.Text
type Pass   = T.Text
type Chan   = T.Text

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
        (head,body)                 = T.breakOn ":" (T.drop 1 str)
        content                     = T.drop 1 body
        (nick:_:user:host:_:chan:_) = T.split delims head
        delims c                    = case c of
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
write ∷ Handle → (T.Text, T.Text) → IO ()
write h (act,args) = do
  T.hprint h "{} {}\r\n" [act, args]
  T.print "> {} {}\n" [act, args]

-- handles privmsgs and creates responses
handlePM ∷ Message → Maybe (T.Text, T.Text)
handlePM s
  | prefix ".quit" && msgUser s == "MrDetonia" = Just ("QUIT","")
  | prefix ".bots" = Just ("PRIVMSG", dest `T.append` " :I am a bot written by MrDetonia in Haskell | https://gitla.in/MrDetonia/detoniabot")
  | otherwise = Nothing
  where
    prefix p = p `T.isPrefixOf` msgContent s
    dest = if "#" `T.isPrefixOf` msgChan s then msgChan s else msgNick s

-- takes an IRC message and generates the correct response
respond ∷ T.Text → Maybe (T.Text, T.Text)
respond s
  | "PING" `T.isPrefixOf` s = Just ("PONG", ':' `T.cons` T.drop 6 s)
  | "PRIVMSG" `T.isInfixOf` s = do
    let res = handlePM $ toMessage s
    case res of
         Just msg -> Just msg
         Nothing  -> Nothing
  | otherwise = Nothing

-- simply listens to a socket forever
listen ∷ Handle → IO ()
listen h = forever $ do
  s <- T.hGetLine h
  T.putStrLn s
  let res = respond s
  case res of
       Just x  -> if fst x == "QUIT"
                     then write h ("QUIT",":Exiting") >> hClose h >> exitSuccess
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
         h <- connectTo (T.unpack $ netServer nt) (PortNumber (fromIntegral (netPort nt)))
         hSetBuffering h NoBuffering
         write h ("NICK", netNick nt)
         write h ("USER", netNick nt `T.append` " 0 * :connected")
         unless (T.null $ netPass nt) (write h ("NICKSERV :IDENTIFY", netPass nt) >> waitForAuth h)
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
