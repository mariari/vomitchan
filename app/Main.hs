{-# LANGUAGE ScopedTypeVariables #-}
module Main where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Control.Concurrent  as C
import qualified StmContainers.Map   as M
import qualified StmContainers.Set   as S
import qualified Data.List           as L
import qualified Data.Text           as T
import qualified Data.Hashable       as Hashable
import qualified ListT
import qualified System.IO.Error     as Error
import qualified Network.Connection  as Connection
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as ClientT

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
-- Int is the identifier for the current process
forkWithKill :: C.MVar [C.ThreadId] -> S.Set ConnectionEq -> (IO Quit, Int) -> IO (C.MVar ())
forkWithKill tids connections (act,identifier) = do
  handle <- C.newEmptyMVar
  let f (Right AllNetworks)    = kill >> C.putMVar handle ()
      f (Right CurrentNetwork) = print "quitting current network " >> C.putMVar handle ()
      f (Left e)               = print (show e <> " in forkWithKill") >> C.putMVar handle ()
  C.forkFinally spawn f
  return handle
  where
    spawn = withThread act tids
    kill  = do
      -- peacefully quit from the network!
      conns <- atomically $ ListT.toList $ S.listT connections
      -- filter out the current network, to prevent an error from an empty socket
      let connectionsNotCurrent = filter (\(ConnectionEq _ i) -> i /= identifier) conns
      traverse (\(ConnectionEq con _) -> quitNetwork con) connectionsNotCurrent
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
              -- Triggered by the other thread telling the network to quit
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
         servMap     <- initAllServer
         tids        <- C.newMVar []
         connections <- S.newIO
         ctx         <- initConnectionContext
         manager     <- manager ctx
         handles <-
           let listenTry net identifier x@(con,_) = do
                 let conEq = ConnectionEq con identifier
                 let leave = atomically (S.delete conEq connections)
                 atomically (S.insert conEq connections)
                 handleSelf (listen x servMap (netServer net) state manager <* leave)
                            (leave >> listenRetry net identifier)
               listenRetry n ident = do
                 x <- reconnectNetwork servMap ctx n
                 listenTry n ident x
               -- save the ident so we can send it into forkWithKill, to filter out current process
               listenMTry n ident = fmap (\x -> (listenTry n ident x, ident))
               listened           = catMaybes . zipWith3 listenMTry networks [1..]
           in do
             mConnVar <- traverse (startNetwork servMap ctx) networks
             traverse (forkWithKill tids connections) (listened mConnVar)
         traverse_ C.takeMVar handles
  where
    initHash :: [IRCNetwork] -> M.Map T.Text HashStorage -> IO ()
    initHash net ht = atomically . sequence_ $ do
      x             <- net
      (chan, modes) <- fromStateConfig (netState x)
      return $ M.insert modes (netServer x <> chan) ht


manager :: Connection.ConnectionContext -> IO Client.Manager
manager ctx =
  let settings =
        ClientT.mkManagerSettingsContext
          (Just ctx)
          (Connection.TLSSettingsSimple False False False)
          Nothing
  in Client.newManager settings

-- TODO replace manual instances with deriving via
-- Give connection an EQ instance, by supplying an Int
data ConnectionEq = ConnectionEq Connection !Int

instance Eq ConnectionEq where
  (ConnectionEq _ i) == (ConnectionEq _ j) = i == j

instance Hashable.Hashable ConnectionEq where
  hashWithSalt x (ConnectionEq _ i) = Hashable.hashWithSalt x i
