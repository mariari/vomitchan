module Bot.StateType (
  StateConfig(..),
  GlobalState(..),
  HashStorage(..),
  VomState,
  Quit(..),
  Response(..),
  toHashStorage,
  toGlobalState,
  fromStateConfig,
  defaultChanState
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text         as T
import qualified Data.Aeson        as JSON
import qualified StmContainers.Map as M
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
                 { dream  :: !Bool
                 , mute   :: !Bool
                 , fleecy :: !Bool
                 } deriving (Show, Generic)

defaultChanState :: HashStorage
defaultChanState = toHashStorage True False False

instance JSON.FromJSON HashStorage
instance JSON.ToJSON   HashStorage

-- Generates HashStorage
toHashStorage = HashStorage

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
