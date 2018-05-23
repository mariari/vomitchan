{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Main where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Control.Concurrent             as C
import qualified Data.HashTable.IO              as H
import           Data.Monoid
import qualified Data.Text                      as T
import           Data.Foldable
import           Control.Monad.IO.Class
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import           Bot.Network
import           Bot.Socket
import           Bot.StateType
--- FUNCTIONS ---------------------------------------------------------------------------------

-- creates a thread and adds its thread ID to an MVar list, kills all
-- listed threads when finished
forkWithKill :: C.MVar[C.ThreadId] -> IO () -> IO (C.MVar ())
forkWithKill tids act = do
  handle <- C.newEmptyMVar
  _ <- C.forkFinally spawn (\_ -> kill >> C.putMVar handle ())
  return handle

  where
    spawn = C.myThreadId >>= (\tid -> C.modifyMVar_ tids (return . (tid :))) >> act

    kill = do
      threads <- C.readMVar tids
      mytid <- C.myThreadId
      traverse_ C.killThread [t | t <- threads, t /= mytid ]

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
        initHash :: [IRCNetwork] -> H.BasicHashTable T.Text HashStorage -> IO ()
        initHash net ht = sequence_ $ do
          x  <- net
          d  <- dreamMode  . netState $ x
          m  <- muteMode   . netState $ x
          f  <- fleecyMode . netState $ x
          let chanSettings = (fst f, (snd d, snd m, snd f))
          guard  $ all ((== fst f) . fst) [d, m]         -- check if the y and z are talking
          return $ hashadd (netServer x) chanSettings ht -- about the same channel
        hashadd :: T.Text -> (T.Text, (Bool, Bool, Bool)) -> H.BasicHashTable T.Text HashStorage -> IO ()
        hashadd serv (chan, (d, m, f)) ht = H.insert ht (serv <> chan) $ toHashStorage d m f
