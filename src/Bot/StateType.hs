{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.StateType (
  StateConfig,
  GlobalState,
  HashStorage,
  VomState,
  toHashStorage,
  toGlobalState,
  fleecy,
  dream,
  mute,
  hash,
  fromStateConfig,
  defaultChanState
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text         as T
import qualified Data.Aeson        as JSON
import           GHC.Generics
import qualified Data.HashTable.IO as H
import           Control.Concurrent.STM
--- TYPES -------------------------------------------------------------------------------------
type Chan   = T.Text
--- DATA STRUCTURES ---------------------------------------------------------------------------

-- IRC State information
data StateConfig = StateConfig [(Chan, HashStorage)] deriving (Show, Generic)

fromStateConfig (StateConfig xs) = xs

instance JSON.FromJSON StateConfig
instance JSON.ToJSON   StateConfig

-- Stores the Hash Information per channel
data HashStorage = HashStorage
                 { dream  :: Bool
                 , mute   :: Bool
                 , fleecy :: Bool
                 } deriving (Show, Generic)

defaultChanState :: HashStorage
defaultChanState = toHashStorage True False False

instance JSON.FromJSON HashStorage
instance JSON.ToJSON   HashStorage


-- Generates HashStorage
toHashStorage = HashStorage

--  All the Global Variables that make up State
newtype GlobalState = GlobalState
                 {hash :: H.BasicHashTable T.Text HashStorage}

-- Generates GlobalState
toGlobalState :: H.BasicHashTable T.Text HashStorage -> GlobalState
toGlobalState = GlobalState

-- Our globalState type
type VomState = TVar GlobalState
