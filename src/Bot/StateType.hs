{-# LANGUAGE TemplateHaskell #-}
module Bot.StateType (
  StateConfig(..),
  GlobalState(..),
  HashStorage(..),
  VomState,
  Quit(..),
  Response(..),
  response,
  noResponse,
  quit,
  toHashStorage,
  toGlobalState,
  fromStateConfig,
  defaultChanState,
  dream, fleecy, yuki, mute
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text  as T
import qualified Data.Aeson as JSON

import qualified StmContainers.Map as M

import           Control.Lens

import           GHC.Generics
--- TYPES -------------------------------------------------------------------------------------
type Chan = T.Text
--- DATA STRUCTURES ---------------------------------------------------------------------------

-- IRC State information
newtype StateConfig = StateConfig [(Chan, HashStorage)] deriving (Show, Generic)

fromStateConfig (StateConfig xs) = xs

instance JSON.FromJSON StateConfig
instance JSON.ToJSON   StateConfig

-- Stores the Hash Information per channel
data HashStorage = HashStorage
                 { _dream  :: !Bool
                 , _mute   :: !Bool
                 , _fleecy :: !Bool
                 , _yuki   :: !Bool
                 } deriving (Show, Generic)

-- Generates HashStorage
toHashStorage = HashStorage

defaultChanState :: HashStorage
defaultChanState = toHashStorage True False False False

makeLenses ''HashStorage

-- used to generate default aeson instance with lens in tact
-- wish we had ocaml package open here!
instance JSON.ToJSON HashStorage where
  toJSON = JSON.genericToJSON JSON.defaultOptions
           { JSON.fieldLabelModifier = drop 1 }

instance JSON.FromJSON HashStorage where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions
              { JSON.fieldLabelModifier = drop 1 }

--  All the Global Variables that make up State
newtype GlobalState = GlobalState {hash :: M.Map T.Text HashStorage}

instance Show GlobalState where
  show _ = "VomState"

-- Generates GlobalState
toGlobalState :: M.Map T.Text HashStorage -> GlobalState
toGlobalState = GlobalState

-- Our globalState type
type VomState = GlobalState

-- Exit codes for smart exiting
data Quit = AllNetworks
          | CurrentNetwork
          deriving (Show, Eq)

-- Basically the Maybe Monad but with exit codes
data Response a = Response a
                | NoResponse
                | Quit !Quit
                deriving (Show, Functor)

-- LIFTING CONSTRUCTORS------------------------------------------------------------

noResponse :: Applicative a => b -> a (Response c)
noResponse = const $ pure NoResponse

quit :: Applicative a => Quit -> b -> a (Response c)
quit = const . pure . Quit

response :: Applicative a => c -> b -> a (Response c)
response = const . pure . Response
