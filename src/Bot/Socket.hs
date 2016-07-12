{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION ---
module Bot.Socket (
  write,
  listen
) where


--- IMPORTS ---
import qualified Control.Concurrent as C
import           Control.Monad
import qualified Data.Text          as T
import qualified Data.Text.Format   as T
import qualified Data.Text.IO       as T
import           Network
import           System.IO

import           Bot.Message


--- FUNCTIONS ---

-- takes a Handle and an (Action, Args) tuple and sends to socket
write :: Handle -> (T.Text, T.Text) -> IO ()
write h (act,args) = do
  T.hprint h "{} {}\r\n" [act, args]
  T.print "{} {}\n" [act,args]


-- simply listens to a socket forever
listen :: Handle -> IO ()
-- TODO: use untilM
listen h = forever $ do
    s <- T.hGetLine h
    T.putStrLn s

    let res = respond s
    case res of
      Just x  -> do
        write h x
        when (fst x == "QUIT") $ do
          hClose h
          myid <- C.myThreadId
          C.killThread myid

      Nothing -> return ()
