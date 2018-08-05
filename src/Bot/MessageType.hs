{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.MessageType (
  Server,
  Port,
  Nick,
  User,
  Host,
  Pass,
  Chan,
  Info(..),
  Message(..),
  Command(..),
  PrivMsg(..),
  Join(..),
  CQuit(..),
  UserI(..),
  Ping(..),
  Numbers(..),
  Part(..),
  Other(..),
  infoNick,
  infoUser,
  infoHost,
  infoChan,
  infoContent,
  toMessage,
  toInfo
) where
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

-- taking only user prefixes
data PrivMsg = PrivMsg UserI Target Content deriving Show
data Part    = Part    UserI Target Content deriving Show
data Join    = Join    UserI Target         deriving Show
data CQuit   = CQuit   UserI Content        deriving Show

-- taking no prefixes
data Ping    = Ping Content deriving Show

data UserI = UserI Nick (Maybe User) (Maybe Host) deriving Show

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

data Info = Info { message  :: Message
                 , server   :: T.Text
                 , vomState :: VomState
                 } deriving Show

-- IRC message structure
data Message = Message
             { msgNick    :: Nick
             , msgUser    :: User
             , msgHost    :: Host
             , msgChan    :: Chan
             , msgContent :: T.Text
             } deriving Show

infoNick    = msgNick . message
infoUser    = msgUser . message
infoHost    = msgHost . message
infoChan    = msgChan . message
infoContent = msgContent . message

--- FUNCTIONS ---------------------------------------------------------------------------------
toInfo :: T.Text -> T.Text -> VomState -> Info
toInfo str = Info (toMessage str)

-- converts a string to a Message
toMessage :: T.Text -> Message
toMessage str = Message nick user host chan content
  where
    nick    = T.tail            $ regex str  ":[^!]*"
    user    = T.init            $ regex str  "[^!~]*@"
    host    = T.tail            $ regex str  "@[^ ]*"
    chan    = T.strip . T.init  $ regex str  "[^ ]* :"
    content = T.tail  . T.strip $ regex str  " :.*$"

-- performs a regex on a T.Text
regex :: T.Text -> String -> T.Text
regex str pat = T.pack (T.unpack str =~ pat)
