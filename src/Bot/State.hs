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
    Nothing -> return $ toHashStorage True False False
    Just x -> return x

-- modifies the hash-table state for a message
modifyChanState :: Message -> HashStorage -> IO ()
modifyChanState msg hStore = do
  ht <- hash <$> (readTVarIO . msgState) msg
  H.insert ht (getHashText msg) hStore

-- applys not or identity to all the different states to modify state
flipChanState :: Message -> (Bool -> Bool) -> (Bool -> Bool) -> (Bool -> Bool) -> IO ()
flipChanState msg d m f = do
  state <- getChanState msg
  modifyChanState msg (toHashStorage . d . dream <*> m . mute <*> f . fleecy $ state)

modifyDreamState :: Message -> IO ()
modifyDreamState msg = flipChanState msg not id id

modifyMuteState :: Message -> IO ()
modifyMuteState msg = flipChanState msg id not id

modifyFleecyState :: Message -> IO ()
modifyFleecyState msg = flipChanState msg id id not


-- Checks if the message is a pm
isPM :: Message -> Bool
isPM = not . ("#" `T.isPrefixOf`) . msgChan

-- gives back the hashname for the message
getHashText :: Message -> T.Text
getHashText msg
  | isPM msg  = createText msgNick
  | otherwise = createText msgChan
  where createText f = msgServer msg <> f msg
