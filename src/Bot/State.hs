{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot.State (
  getChanState,
  modifyChanState
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
    Nothing -> return $ toHashStorage True False
    Just x -> return x

-- modifies the hash-table state for a message
modifyChanState :: Message -> HashStorage -> IO ()
modifyChanState msg hStore = do
  ht    <- hash <$> (readTVarIO . msgState) msg
  H.insert ht (getHashText msg) hStore


isPM :: Message -> Bool
isPM msg = msgChan msg == "vomitchan"


getHashText :: Message -> T.Text
getHashText msg
  | isPM msg  = createText msgNick
  | otherwise = createText msgChan
  where createText f = msgServer msg <> f msg
