module Bot.Servers where

import qualified Data.Map.Strict as M
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM

import Bot.MessageType
import Bot.NetworkType
import Bot.StateType

initAllServer :: IO AllServers
initAllServer = S <$> newTVarIO mempty
                  <*> newTVarIO mempty
                  <*> newTVarIO mempty
                  <*> newTVarIO mempty

addConnected :: AllServers -> MVar Quit -> IRCNetwork -> STM ()
addConnected (S {_numToConnect = tNum, _servToNumConn = tCon}) mvar network = do
  numMap <- readTVar tNum
  let i = case M.lookupMax numMap of
            Just (n,_) -> n + 1
            Nothing    -> 1
  writeTVar   tNum (M.insert i (C mvar network) numMap)
  modifyTVar' tCon (M.insert (netServer network) i)

addDisconnected :: AllServers -> IRCNetwork -> STM ()
addDisconnected (S {_numToDisconnect = tNum, _servToNumDisconn = tCon}) network = do
  numMap <- readTVar tNum
  let i = case M.lookupMax numMap of
            Just (n,_) -> n + 1
            Nothing    -> 1
  writeTVar   tNum (M.insert i network numMap)
  modifyTVar' tCon (M.insert (netServer network) i)


previousMvar :: AllServers -> IRCNetwork -> STM (Maybe (MVar Quit))
previousMvar (S {_servToNumConn = tCon, _numToConnect = tNum}) network = do
  numMap <- readTVar tNum
  conMap <- readTVar tCon
  return $ M.lookup (netServer network) conMap >>= (M.!?) numMap >>= return . _connection

-- | currently unused, however it removes the network from the maps,
-- and keeps the maps without a hole in the number
disconnectServer :: AllServers -> IRCNetwork -> STM ()
disconnectServer all@(S { _numToConnect     = tNum
                        , _servToNumConn    = tCon
                        , _numToDisconnect  = tNumD
                        , _servToNumDisconn = tConD
                        }) network = do
  numMap <- readTVar tNum
  conMap <- readTVar tCon
  let disconnect =  do
        writeTVar tCon (M.delete (netServer network) conMap)
        addDisconnected all network

  case M.lookupMax numMap of
    Just (iMax,net) -> do
      case M.lookup (netServer network) conMap of
        Just iNet
          | iNet == iMax -> do
              writeTVar tNum (M.delete iNet numMap)
              disconnect
          | otherwise -> do
              let newMap = M.insert iNet net $ M.delete iMax numMap
              writeTVar tNum newMap
              disconnect
              modifyTVar' tCon (M.insert (netServer (_networkInfo net)) iNet)
        Nothing -> disconnect
    Nothing     -> disconnect
