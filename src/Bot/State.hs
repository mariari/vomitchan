{-# LANGUAGE FlexibleContexts #-}
module Bot.State (
  getChanState,
  getChanStateM,
  modifyChanState,
  modifyDreamState,
  modifyMuteState,
  modifyFleecyState,
  modifyYukiState
  ) where

--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text  as T
import           Data.Maybe (fromMaybe)

import qualified StmContainers.Map as M

import           Control.Concurrent.STM (atomically, STM(..))
import           Control.Lens

import Bot.StateType
import Bot.MessageType
import Bot.EffType
-- Functions ----------------------------------------------------------------------------------

-- Returns the hash-table state for a message
getChanState :: InfoPriv -> STM HashStorage
getChanState msg = fromMaybe defaultChanState <$> M.lookup (getHashText msg) ht
  where
    ht = hash . vomState $ msg

getChanStateM :: CmdImp m => m HashStorage
getChanStateM = toReaderImp (atomically . getChanState)

-- modifies the hash-table state for a message
modifyChanState :: InfoPriv -> HashStorage -> STM ()
modifyChanState msg hStore = M.insert hStore (getHashText msg) ht
  where
    ht = hash . vomState $ msg

updateState :: (HashStorage -> HashStorage) -> InfoPriv -> IO ()
updateState f msg = atomically (getChanState msg >>= modifyChanState msg . f)

updateStateR f = toReaderImp (updateState f)

modifyDreamState
  , modifyMuteState
  , modifyFleecyState
  , modifyYukiState :: CmdImp m => m ()
modifyDreamState  = updateStateR (over dream not)
modifyMuteState   = updateStateR (over mute not)
modifyFleecyState = updateStateR (over fleecy not)
modifyYukiState   = updateStateR (over yuki not)


-- Checks if the message is a pm
isPM :: InfoPriv -> Bool
isPM = not . ("#" `T.isPrefixOf`) . infoChan

-- gives back the hashname for the message
getHashText :: InfoPriv -> T.Text
getHashText msg
  | isPM msg  = createText infoNick
  | otherwise = createText infoChan
  where createText f = server msg <> f msg
