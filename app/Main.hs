{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Main where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Control.Concurrent             as C
import qualified Data.HashTable.IO              as H
import           Data.Monoid
import qualified Data.Text                      as T
import           Control.Monad.IO.Class
import           Control.Concurrent.STM

import           Bot.Network
import           Bot.Socket
import           Bot.StateType
--- FUNCTIONS ---------------------------------------------------------------------------------

-- creates a thread and adds its thread ID to an MVar list, kills all
-- listed threads when finished
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
  nets   <- readNetworks "data/networks.json"
  stateT <- H.new >>= newTVarIO . toGlobalState
  state  <- readTVarIO stateT
  case nets of
       Nothing       -> putStrLn "ERROR loading servers from JSON"
       Just networks -> do
         initHash networks (hash state)
         tids    <- C.newMVar []
         handles <- mapM (forkWithKill tids . connect stateT) networks
         mapM_ C.takeMVar handles

  where connect s n = joinNetwork n >>= \x -> listen x (netServer n) s
        -- poorly composed :( Maybe use lenses to fix eventually
        initHash net ht = mapM_ (\x -> mapM_ (\y -> mapM_ (\z -> hashadd (netServer x) y z ht)
                                                    (muteMode . netState $ x))
                                       (dreamMode . netState $ x))
                          net
        hashadd serv (ch1, b1) (ch2, b2) ht
          | ch1 == ch2 = H.insert ht (serv <> ch1) (toHashStorage b1 b2)
          | otherwise = return()
