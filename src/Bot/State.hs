{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Bot.State (
  getChanState,
  getChanStateM,
  modifyChanState,
  modifyDreamState,
  modifyMuteState,
  modifyFleecyState,
  ) where

--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text         as T
import           Control.Concurrent.STM
import           Data.Monoid
import           Data.Maybe
import           Control.Monad.Reader
import qualified STMContainers.Map as M

import Bot.StateType
import Bot.MessageType
import Bot.EffType
-- Functions ----------------------------------------------------------------------------------

-- Returns the hash-table state for a message
getChanState :: InfoPriv -> IO HashStorage
getChanState msg = atomically (fromMaybe defaultChanState <$> M.lookup (getHashText msg) ht)
  where
    ht = hash . vomState $ msg

getChanStateM :: CmdImp m => m HashStorage
getChanStateM = toReaderImp getChanState

-- modifies the hash-table state for a message
modifyChanState :: InfoPriv -> HashStorage -> IO ()
modifyChanState msg hStore = atomically (M.insert hStore (getHashText msg) ht)
  where
    ht = hash . vomState $ msg

updateState :: (HashStorage -> HashStorage) -> InfoPriv -> IO ()
updateState f msg = getChanState msg >>= modifyChanState msg . f

updateStateR f = toReaderImp (updateState f)

modifyDreamState :: CmdImp m => m ()
modifyDreamState = updateStateR (\s -> s {dream = not (dream s)})

modifyMuteState :: CmdImp m => m ()
modifyMuteState = updateStateR (\s -> s {mute = not (mute s)})

modifyFleecyState :: CmdImp m => m ()
modifyFleecyState = updateStateR (\s -> s {fleecy = not (fleecy s)})


-- Checks if the message is a pm
isPM :: InfoPriv -> Bool
isPM = not . ("#" `T.isPrefixOf`) . infoChan

-- gives back the hashname for the message
getHashText :: InfoPriv -> T.Text
getHashText msg
  | isPM msg  = createText infoNick
  | otherwise = createText infoChan
  where createText f = server msg <> f msg
