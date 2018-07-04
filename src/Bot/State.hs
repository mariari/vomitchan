{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot.State (
  getChanState,
  modifyChanState,
  modifyDreamState,
  modifyMuteState,
  modifyFleecyState,
  ) where

--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text         as T
import qualified Data.HashTable.IO as H
import           Control.Concurrent.STM
import           Data.Monoid

import Bot.StateType
import Bot.MessageType
-- Functions ----------------------------------------------------------------------------------

-- Returns the hash-table state for a message
getChanState :: Message -> IO HashStorage
getChanState msg = do
  ht    <- hash <$> (readTVarIO . msgState) msg
  maybs <- H.lookup ht (getHashText msg)
  case maybs of
    Nothing -> return $ defaultChanState
    Just x -> return x

-- modifies the hash-table state for a message
modifyChanState :: Message -> HashStorage -> IO ()
modifyChanState msg hStore = do
  ht <- hash <$> (readTVarIO . msgState) msg
  H.insert ht (getHashText msg) hStore

updateState :: (HashStorage -> HashStorage) -> Message -> IO ()
updateState f msg = getChanState msg >>= modifyChanState msg . f

modifyDreamState :: Message -> IO ()
modifyDreamState = updateState (\s -> s {dream = not (dream s)})

modifyMuteState :: Message -> IO ()
modifyMuteState = updateState (\s -> s {mute = not (mute s)})

modifyFleecyState :: Message -> IO ()
modifyFleecyState = updateState (\s -> s {fleecy = not (fleecy s)})


-- Checks if the message is a pm
isPM :: Message -> Bool
isPM = not . ("#" `T.isPrefixOf`) . msgChan

-- gives back the hashname for the message
getHashText :: Message -> T.Text
getHashText msg
  | isPM msg  = createText msgNick
  | otherwise = createText msgChan
  where createText f = msgServer msg <> f msg
