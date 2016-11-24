{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.Socket (
  write,
  listen
) where
--- IMPORTS ------------------------------------------------------------------------------ ----
import           Control.Monad
import           Control.Monad.Loops
import           Control.Concurrent as C
import qualified Data.Text          as T
import qualified Data.Text.Format   as T
import qualified Data.Text.IO       as T
import           Network
import           System.IO

import           Bot.Message
--- FUNCTIONS ------------------------------------------------------------------------------ --

-- takes a Handle and an (Action, Args) tuple and sends to socket
write :: Handle -> (T.Text, T.Text) -> IO ()
write h (act,args) = T.hprint h "{} {}\r\n" [act, args]
                  >> T.print "{} {}\n" [act,args]


-- simply listens to a socket forever
listen :: Handle -> IO ()
listen h = iterateUntil (== Just ()) resLoop >> hClose h
  where
    resLoop = do
      s   <- T.hGetLine h
      l   <- hShow h
      let x = T.pack l
      T.putStrLn s
      print x

      quit <- C.newEmptyMVar

      _ <- forkIO(inout s x quit)

      C.tryTakeMVar quit

    inout s x quit = do
      res <- respond s x
      case res of
        Just x -> write h x >> when (fst x == T.pack "QUIT") (C.putMVar quit ())
        Nothing -> return ()
