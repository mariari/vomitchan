{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Main where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Control.Concurrent      as C
import qualified STMContainers.Map       as M

import           Data.Monoid
import qualified Data.Text               as T
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
forkWithKill :: C.MVar[C.ThreadId] -> IO Quit -> IO (C.MVar ())
forkWithKill tids act = do
  handle <- C.newEmptyMVar
  let f (Right AllNetworks)    = kill >> C.putMVar handle ()
      f (Right CurrentNetwork) = C.putMVar handle ()
      f (Left e)               = print (show e)
  C.forkFinally spawn f
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
  nets  <- readNetworks "data/networks.json"
  state <- toGlobalState <$> M.newIO
  case nets of
       Nothing       -> putStrLn "ERROR loading servers from JSON"
       Just networks -> do
         initHash networks (hash state)
         tids    <- C.newMVar []
         handles <- traverse (forkWithKill tids . connect state) networks
         traverse_ C.takeMVar handles

  where connect s n = do
         x <- joinNetwork n
         case x of
           Nothing -> return CurrentNetwork
           Just cx -> listen cx (netServer n) s
        initHash :: [IRCNetwork] -> M.Map T.Text HashStorage -> IO ()
        initHash net ht = atomically . sequence_ $ do
          x             <- net
          (chan, modes) <- fromStateConfig (netState x)
          let serv      = netServer x
          return $ M.insert modes (serv <> chan) ht
