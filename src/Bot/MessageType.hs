{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.MessageType where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text          as T
import qualified Data.Map.Strict    as M
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM
import           Text.Regex.TDFA
import           Bot.StateType
import           Data.Foldable
import           Control.Lens

import Bot.NetworkType
--- TYPES -------------------------------------------------------------------------------------

--- DATA STRUCTURES ---------------------------------------------------------------------------

data Command = PRIVMSG PrivMsg
             | JOIN    Join
             | CQUIT   CQuit
             | NUMBERS Numbers
             | PART    Part
             | PING    Ping
             | OTHER   !T.Text Other -- the T.Text is the unidentified command
             | ERROR
             deriving Show

data InfoPriv = Info
  { message  :: !PrivMsg
  , server   :: !T.Text
  , vomState :: VomState
  , _servers :: AllServers
  }

data AllServers = S
  { _servToNumConn    :: TVar (M.Map Server Int)
  , _servToNumDisconn :: TVar (M.Map Server Int)
  , _numToConnect     :: TVar (M.Map Int ConnectionInfo)
  , _numToDisconnect  :: TVar (M.Map Int IRCNetwork)
  }

initAllServer :: IO AllServers
initAllServer = S <$> newTVarIO mempty
                  <*> newTVarIO mempty
                  <*> newTVarIO mempty
                  <*> newTVarIO mempty

addConnected :: AllServers -> MVar Quit -> IRCNetwork -> STM ()
addConnected (S {_numToConnect = tNum, _servToNumConn = tCon}) mvar network = do
  numMap <- readTVar tNum
  let i = case M.lookupMax numMap of
            Just (n,_) -> n + 1
            Nothing    -> 1
  writeTVar   tNum (M.insert i (C mvar network) numMap)
  modifyTVar' tCon (M.insert (netServer network) i)

addDisconnected :: AllServers -> IRCNetwork -> STM ()
addDisconnected (S {_numToDisconnect = tNum, _servToNumDisconn = tCon}) network = do
  numMap <- readTVar tNum
  let i = case M.lookupMax numMap of
            Just (n,_) -> n + 1
            Nothing    -> 1
  writeTVar   tNum (M.insert i network numMap)
  modifyTVar' tCon (M.insert (netServer network) i)

data ConnectionInfo = C
  { _connection  :: MVar Quit -- the quit var for the thread each listen tries to take
  , _networkInfo :: IRCNetwork
  }

-- taking only user prefixes
data PrivMsg = PrivMsg { user       :: !UserI
                       , msgChan    :: !Target
                       , msgContent :: !Content
                       } deriving Show


data Part  = Part  !UserI !Target !Content deriving Show
data Join  = Join  !UserI !Target         deriving Show
data CQuit = CQuit !UserI !Content        deriving Show

-- taking no prefixes
data Ping = Ping Content deriving Show

data UserI = UserI { usrNick :: !Nick
                   , usrUser :: !(Maybe User)
                   , usrHost :: !(Maybe Host)
                   } deriving Show

data Numbers = N354 !Server !Content
             | N904 !Server !Content
             | N903 !Server !Content
             | N376 !Server !Content
             | NOther !Int Other
             deriving Show


data Other = OtherUser   !UserI !Content
           | OtherServer !Server !Content
           | OtherNoInfo !Content
           deriving Show


msgNick :: PrivMsg -> Nick
msgNick = usrNick . user
msgUser = usrUser . user
msgHost = usrHost . user

infoNick    = msgNick . message
infoUser    = msgUser . message
infoHost    = msgHost . message
infoChan    = msgChan . message
infoContent = msgContent . message

class GetUser f where
  userI :: f -> UserI

instance GetUser (PrivMsg) where
  userI (PrivMsg u _ _) = u

makeLenses ''InfoPriv
makeLenses ''AllServers
makeLenses ''ConnectionInfo
