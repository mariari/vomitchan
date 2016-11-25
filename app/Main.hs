{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Main where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Control.Concurrent as C

import           Bot.Network
import           Bot.Socket
import           Bot.State
--- FUNCTIONS ---------------------------------------------------------------------------------

-- creates a thread and adds its thread ID to an MVar list, kills all
-- listed threads when finished  DICKS
forkWithKill :: C.MVar[C.ThreadId] -> IO () -> IO (C.MVar())
forkWithKill tids act = do
  handle <- C.newEmptyMVar
  C.forkFinally spawn (\_ -> kill >> C.putMVar handle ())
  return handle

  where
    spawn = C.myThreadId >>= (\tid -> C.modifyMVar_ tids (return . (tid:))) >> act

    kill = do
      threads <- C.readMVar tids
      mytid <- C.myThreadId
      mapM_ C.killThread [t | t <- threads, t /= mytid ]

 --- ENTRY POINT ------------------------------------------------------------------------------

main :: IO ()
main = do
  nets <- readNetworks "data/networks.json"

  case nets of
       Nothing       -> putStrLn "ERROR loading servers from JSON"
       Just networks -> do
         tids    <- C.newMVar []
         handles <- mapM (forkWithKill tids . connect) networks
         mapM_ C.takeMVar handles

  where connect n = joinNetwork n >>= \x -> listen x (netServer n)
