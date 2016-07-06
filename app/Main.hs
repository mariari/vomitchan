{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION ---
module Main where


--- IMPORTS ---
import qualified Control.Concurrent as C
import           Control.Monad

import           Bot.Network
import           Bot.Socket


--- FUNCTIONS ---
-- creates a thread and adds its thread ID to an MVar list, kills all
-- listed threads when finished
forkWithKill :: C.MVar[C.ThreadId] -> IO () -> IO (C.MVar())
forkWithKill tids act = do
  handle <- C.newEmptyMVar
  C.forkFinally spawn (\_ -> kill >> C.putMVar handle ())
  return handle

  where
    spawn = do
      tid <- C.myThreadId
      C.modifyMVar_ tids (\x -> return $ tid:x)
      act

    kill = do
      threads <- C.readMVar tids
      mytid <- C.myThreadId
      mapM_ C.killThread [t | t <- threads, t /= mytid ]


 --- ENTRY POINT ---
main :: IO ()
main = do
  nets <- readNetworks "data/networks.json"

  case nets of
       Nothing       -> putStrLn "ERROR loading servers from JSON"

       Just networks -> do
         tids    <- C.newMVar []
         handles <- mapM (forkWithKill tids . connect) networks
         mapM_ C.takeMVar handles

  where connect n = do
          h <- joinNetwork n
          listen h
