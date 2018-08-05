{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.MessageType where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text as T
import           Text.Regex.TDFA
import           Bot.StateType
import           Data.Foldable
--- TYPES -------------------------------------------------------------------------------------

-- types for IRC data
type Server = T.Text
type Port   = Integer
type Nick   = T.Text
type User   = T.Text
type Host   = T.Text
type Pass   = T.Text
type Chan   = T.Text
type Target = T.Text
type Content = T.Text
--- DATA STRUCTURES ---------------------------------------------------------------------------

data Command = PRIVMSG PrivMsg
             | JOIN    Join
             | CQUIT   CQuit
             | NUMBERS Numbers
             | PART    Part
             | PING    Ping
             | OTHER   T.Text Other -- the T.Text is the unidentified command
             | ERROR
             deriving Show

data InfoPriv = Info { message  :: PrivMsg
                     , server   :: T.Text
                     , vomState :: VomState
                     } deriving Show

-- taking only user prefixes
data PrivMsg = PrivMsg {user       :: UserI
                       ,msgChan    :: Target
                       ,msgContent :: Content
                       } deriving Show

data Part    = Part    UserI Target Content deriving Show
data Join    = Join    UserI Target         deriving Show
data CQuit   = CQuit   UserI Content        deriving Show

-- taking no prefixes
data Ping    = Ping Content deriving Show

data UserI = UserI { usrNick :: Nick
                   , usrUser :: Maybe User
                   , usrHost :: Maybe Host
                   } deriving Show

data Numbers = N354 Server Content
             | N904 Server Content
             | N903 Server Content
             | N376 Server Content
             | NOther Int Other
             deriving Show


data Other = OtherUser   UserI Content
           | OtherServer Server Content
           | OtherNoInfo Content
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
