{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.State (
  StateConfig,
  GlobalState,
  HashStorage,
  toHashStorage,
  toGlobalState,
  dreamMode,
  muteMode,
  dream,
  mute,
  hash
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text       as T
import           Text.Regex.TDFA
import qualified Data.Aeson           as JSON
import           GHC.Generics
import           Bot.MessageType
import qualified Data.HashTable.IO as H
--- TYPES -------------------------------------------------------------------------------------
--- DATA STRUCTURES ---------------------------------------------------------------------------

-- IRC State information
data StateConfig = StateConfig
                 { dreamMode   :: [(Chan, Bool)]
                 , muteMode    :: [(Chan, Bool)]
                 } deriving (Show, Generic)


instance JSON.FromJSON StateConfig
instance JSON.ToJSON   StateConfig

data HashStorage = HashStorage
                 { dream :: Bool
                 , mute  :: Bool
                 }

toHashStorage :: Bool -> Bool -> HashStorage
toHashStorage = HashStorage

data GlobalState = GlobalState
                 {hash  :: H.BasicHashTable T.Text HashStorage}

toGlobalState :: H.BasicHashTable T.Text HashStorage -> GlobalState
toGlobalState = GlobalState
