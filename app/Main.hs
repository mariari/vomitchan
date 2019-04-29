{-# LANGUAGE ScopedTypeVariables #-}
module Main where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Control.Concurrent      as C
import qualified StmContainers.Map       as M
import qualified Data.List               as L
import qualified Data.Text               as T
import           Network.Connection(initConnectionContext, Connection(..), HostNotResolved)
import           Control.Exception
import           Control.Exception.Base(AsyncException(..))
import           Data.Foldable (traverse_)
import           Data.Maybe    (catMaybes)
import           Control.Monad (zipWithM)
import           Control.Concurrent.STM (atomically)

import Bot.Network
import Bot.Socket
import Bot.StateType
import Bot.Servers
import GHC.Conc (numCapabilities)
--- FUNCTIONS ---------------------------------------------------------------------------------

-- creates a thread and adds its thread ID to an MVar list, kills all
-- listed threads when finished
forkWithKill :: C.MVar [C.ThreadId] -> IO Quit -> IO (C.MVar ())
forkWithKill tids act = do
  handle <- C.newEmptyMVar
  let f (Right AllNetworks)    = kill >> C.putMVar handle ()
      f (Right CurrentNetwork) = print "quitting current network " >> C.putMVar handle ()
      f (Left e)               = print (show e <> " in forkWithKill") >> C.putMVar handle ()
  C.forkFinally spawn f
  return handle
  where
    spawn = withThread act tids
    kill  = do
      threads <- C.readMVar tids
      mytid   <- C.myThreadId
      traverse_ C.killThread (filter (/= mytid) threads)

withThread :: IO a -> C.MVar [C.ThreadId] -> IO a
withThread act tids = do
  tid <- C.myThreadId
  modifyVar (:) tid *> act <* modifyVar L.delete tid
  where
    modifyVar f tid = C.modifyMVar_ tids (return . f tid)

handleSelf :: IO Quit -> IO Quit -> IO Quit
handleSelf f g =
  f `catches` [ Handler (\ (e :: SomeAsyncException) ->
                           print ("AsyncException: " <> show e) >> return CurrentNetwork)
              , Handler (\ (e :: HostNotResolved) ->
                           print "retrying connection" >> g)
              , Handler (\ (e :: SomeException) ->
                           print ("Unknown Exception" <> show e) >> g)]

 --- ENTRY POINT ------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn $ "number of cores: " ++ show numCapabilities
  nets  <- readNetworks "data/networks.json"
  state <- toGlobalState <$> M.newIO
  case nets of
       Nothing       -> putStrLn "ERROR loading servers from JSON"
       Just networks -> do
         initHash networks (hash state)
         servMap <- initAllServer
         tids    <- C.newMVar []
         ctx     <- initConnectionContext
         let listenTry x n =
               handleSelf (listen x servMap (netServer n) state) (listenRetry n)
             listenRetry n = do
               handleSelf (do x <- reconnectNetwork servMap ctx n
                              listen x servMap (netServer n) state)
                          (listenRetry n)
         handles <- do
           mConnVar    <- traverse (startNetwork servMap ctx) networks
           let connVar = catMaybes mConnVar
           zipWithM (\x -> forkWithKill tids . listenTry x) connVar networks
         traverse_ C.takeMVar handles
  where
    initHash :: [IRCNetwork] -> M.Map T.Text HashStorage -> IO ()
    initHash net ht = atomically . sequence_ $ do
      x             <- net
      (chan, modes) <- fromStateConfig (netState x)
      return $ M.insert modes (netServer x <> chan) ht
