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
listen :: Handle -> T.Text -> IO ()
listen h net = iterateUntil (== Just ()) resLoop >> hClose h
  where
    resLoop = do
      s   <- T.hGetLine h
      T.putStrLn s
      print net

      quit <- C.newEmptyMVar

      _ <- forkIO(inout s net quit)

      C.tryTakeMVar quit

    inout s net quit = do
      res <- respond s net
      case res of
        Just x -> write h x >> when (fst x == T.pack "QUIT") (C.putMVar quit ())
        Nothing -> return ()
