{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Main where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Control.Concurrent             as C
import           Control.Monad.Trans.State
import qualified Data.HashTable.IO              as H
import           Data.Monoid
import qualified Data.Text                      as T
import           Control.Monad.IO.Class

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

mainloop :: MonadIO m => StateT GlobalState m ()
mainloop =
  get >>= \ht ->
  liftIO $ readNetworks "data/networks.json" >>= \nets ->
  case nets of
       Nothing       -> putStrLn "ERROR loading servers from JSON"
       Just networks -> do
         initHash networks (hash ht)
         tids    <- C.newMVar []
         handles <- mapM (forkWithKill tids . connect) networks
         mapM_ C.takeMVar handles

  where connect n = joinNetwork n >>= \x -> listen x (netServer n)
        -- poorly composed :( Maybe use lenses to fix eventually
        initHash net ht = mapM_ (\x -> mapM_ (\y -> mapM_ (\z -> hashadd (netServer x) y z ht)
                                                    (muteMode . netState $ x))
                                       (dreamMode . netState $ x))
                          net
        hashadd serv (ch1, b1) (ch2, b2) ht
          | ch1 == ch2 = H.insert ht (serv <> ch1) (toHashStorage b1 b2)
          | otherwise = return()

 --- ENTRY POINT ------------------------------------------------------------------------------

main :: IO ()
main = (H.new :: IO (H.BasicHashTable T.Text HashStorage)) >>= \x -> runStateT mainloop (toGlobalState x) >> return () 
