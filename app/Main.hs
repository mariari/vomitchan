{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}


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
forkWithKill ∷ C.MVar[C.ThreadId] → IO () → IO ()
forkWithKill tids act = void $ C.forkFinally spawn (\_ -> mapM_ C.killThread =<< C.readMVar tids)

  where spawn = do
          tid <- C.myThreadId
          C.modifyMVar_ tids (\x -> return $ tid:x)
          act


 --- ENTRY POINT ---
main ∷ IO ()
main = do
  nets <- readNetworks "data/networks.json"

  case nets of
       Nothing  -> putStrLn "ERROR loading servers from JSON"

       Just networks -> do
         tids <- C.newMVar []
         mapM_ (forkWithKill tids . connect) networks

  where connect n = do
          h <- joinNetwork n
          listen h
