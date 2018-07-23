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
getChanState :: Message -> IO HashStorage
getChanState msg = atomically (fromMaybe defaultChanState <$> M.lookup (getHashText msg) ht)
  where
    ht = hash . msgState $ msg

getChanStateM :: CmdImp m => m HashStorage
getChanStateM = reader getChanState >>= liftIO

-- modifies the hash-table state for a message
modifyChanState :: Message -> HashStorage -> IO ()
modifyChanState msg hStore = atomically (M.insert hStore (getHashText msg) ht)
  where
    ht = hash . msgState $ msg


updateState :: (HashStorage -> HashStorage) -> Message -> IO ()
updateState f msg = getChanState msg >>= modifyChanState msg . f

updateStateR f = reader (updateState f) >>= liftIO

modifyDreamState :: CmdImp m => m ()
modifyDreamState = updateStateR (\s -> s {dream = not (dream s)})

modifyMuteState :: CmdImp m => m ()
modifyMuteState = updateStateR (\s -> s {mute = not (mute s)})

modifyFleecyState :: CmdImp m => m ()
modifyFleecyState = updateStateR (\s -> s {fleecy = not (fleecy s)})


-- Checks if the message is a pm
isPM :: Message -> Bool
isPM = not . ("#" `T.isPrefixOf`) . msgChan

-- gives back the hashname for the message
getHashText :: Message -> T.Text
getHashText msg
  | isPM msg  = createText msgNick
  | otherwise = createText msgChan
  where createText f = msgServer msg <> f msg
