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
import           Control.Monad.IO.Class
import           Control.Concurrent.STM

import           Bot.Message
import           Bot.StateType
--- FUNCTIONS ------------------------------------------------------------------------------ --

-- takes a Handle and an (Action, Args) tuple and sends to socket
write :: Handle -> (T.Text, T.Text) -> IO ()
write h (act,args) = T.hprint h "{} {}\r\n" [act, args]
                  >> T.print "{} {}\n" [act,args]


-- simply listens to a socket forever
listen :: Handle -> T.Text -> TVar GlobalState -> IO ()
listen h net state = iterateUntil (== Just ()) resLoop >> (liftIO . hClose) h
  where
    resLoop = do
        s    <- T.hGetLine h
        T.putStrLn s
        print net
        quit <- C.newEmptyMVar
        _    <- forkIO(inout s net quit state)

        C.tryTakeMVar quit

    inout s net quit state = do
      res <- respond s net state
      case res of
        Just x -> write h x >> when (fst x == T.pack "QUIT") (C.putMVar quit ())
        Nothing -> return ()
