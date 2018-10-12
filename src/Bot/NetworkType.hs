{-# LANGUAGE DeriveGeneric #-}

module Bot.NetworkType where

import qualified Data.Aeson as JSON
import qualified Data.Text  as T
import           GHC.Generics

import Bot.StateType

-- types for IRC data
type Server = T.Text
type Port   = Int
type Nick   = T.Text
type User   = T.Text
type Host   = T.Text
type Pass   = T.Text
type Chan   = T.Text
type Target = T.Text
type Content = T.Text


data IRCNetwork = IRCNetwork
             { netServer :: Server
             , netPort   :: Port
             , netSSL    :: Bool
             , netNick   :: Nick
             , netPass   :: Pass
             , netChans  :: [Chan]
             , netState  :: StateConfig
             } deriving (Show, Generic)

-- allow encoding to/from JSON
instance JSON.FromJSON IRCNetwork
instance JSON.ToJSON IRCNetwork
