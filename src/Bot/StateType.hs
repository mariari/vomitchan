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
  dreamMode,
  muteMode,
  fleecyMode,
  fleecy,
  dream,
  mute,
  hash
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
data StateConfig = StateConfig
                 { dreamMode   :: [(Chan, Bool)]
                 , muteMode    :: [(Chan, Bool)]
                 , fleecyMode  :: [(Chan, Bool)]
                 } deriving (Show, Generic)


instance JSON.FromJSON StateConfig
instance JSON.ToJSON   StateConfig


-- set to fix HashSTorage not being a functor issue
-- data HashStorage a = HashStorage { dream :: a, mute :: a } deriving(Functor)
--data Pt_ a = Pt { pX :: a, pY :: a } deriving(Functor)
--type Pt = Pt_ Integer


-- Stores the Hash Information per channel
data HashStorage = HashStorage
                 { dream  :: Bool
                 , mute   :: Bool
                 , fleecy :: Bool
                 }

-- Generates HashStorage
toHashStorage :: Bool -> Bool -> Bool -> HashStorage
toHashStorage = HashStorage

--  All the Global Variables that make up State
newtype GlobalState = GlobalState
                 {hash :: H.BasicHashTable T.Text HashStorage}

-- Generates GlobalState
toGlobalState :: H.BasicHashTable T.Text HashStorage -> GlobalState
toGlobalState = GlobalState

-- Our globalState type
type VomState = TVar GlobalState
