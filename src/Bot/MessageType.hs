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
  Info,
  Message,
  message,
  msgNick,
  msgUser,
  msgHost,
  msgChan,
  msgContent,
  toMessage,
  server,
  vomState,
  infoNick,
  infoUser,
  infoHost,
  infoChan,
  infoContent,
  toInfo
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text       as T
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

--- DATA STRUCTURES ---------------------------------------------------------------------------

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

infoNick    = msgNick    . message
infoUser    = msgUser    . message
infoHost    = msgHost    . message
infoChan    = msgChan    . message
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
