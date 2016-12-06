{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot.State (
  getChanState
  ) where

--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text         as T
import qualified Data.HashTable.IO as H
import           Control.Concurrent.STM
import           Data.Monoid

import Bot.StateType
import Bot.MessageType
-- Functions ----------------------------------------------------------------------------------

getChanState :: Message -> IO HashStorage
getChanState msg = do
  ht    <- hash <$> (readTVarIO . msgState) msg
  maybs <- H.lookup ht (msgServer msg <> msgChan msg)
  case maybs of
    Nothing -> return $ toHashStorage False False
    Just x -> return x
