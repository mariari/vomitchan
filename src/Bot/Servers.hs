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
