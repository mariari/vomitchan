module Bot.NetworkType where

import qualified Data.Aeson as JSON
import qualified Data.Text  as T

import           GHC.Generics

import Bot.StateType

-- types for IRC data
type Server  = T.Text
type Port    = Int
type Nick    = T.Text
type User    = T.Text
type Host    = T.Text
type Pass    = T.Text
type Target  = T.Text
type Content = T.Text
type MD5     = T.Text
type Chan    = (T.Text, ChanOptions)

data ChanOptions =
  Options {chanKey :: Maybe T.Text }
  deriving (Show, Generic)
data IRCNetwork = IRCNetwork
  { netServer :: Server
  , netPort   :: Port
  , netSSL    :: Bool
  , netNick   :: Nick
  , netPass   :: Pass
  , netAdmins :: [Nick]
  , netIgnore :: [Nick]
  , netBans   :: [MD5]
  , netChans  :: [Chan]
  , netState  :: StateConfig
  } deriving (Show, Generic)

-- allow encoding to/from JSON
instance JSON.FromJSON ChanOptions
instance JSON.ToJSON ChanOptions

instance JSON.FromJSON IRCNetwork
instance JSON.ToJSON IRCNetwork
