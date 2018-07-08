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
import           Control.Concurrent.STM
import           Data.Monoid
import qualified STMContainers.Map as M

import Bot.StateType
import Bot.MessageType
-- Functions ----------------------------------------------------------------------------------

-- Returns the hash-table state for a message
getChanState :: Message -> IO HashStorage
getChanState msg = atomically (f <$> M.lookup (getHashText msg) ht)
  where
    ht = hash . msgState $ msg
    f Nothing  = defaultChanState
    f (Just x) = x

-- modifies the hash-table state for a message
modifyChanState :: Message -> HashStorage -> IO ()
modifyChanState msg hStore = atomically (M.insert hStore (getHashText msg) ht)
  where
    ht = hash . msgState $ msg


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
