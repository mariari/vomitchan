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
  Message,
  msgNick,
  msgUser,
  msgHost,
  msgChan,
  msgContent,
  toMessage
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text as T
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

-- IRC message structure
data Message = Message
             { msgNick    :: Nick
             , msgUser    :: User
             , msgHost    :: Host
             , msgChan    :: Chan
             , msgContent :: T.Text
             } deriving (Show)


--- FUNCTIONS ---------------------------------------------------------------------------------
-- converts a string to a Message
toMessage :: T.Text -> Message
toMessage str = Message nick user host chan content
  where
    (head,body)               = T.breakOn ":" (T.tail str)
    content                   = T.strip $ T.drop 1 body
    (nick:user:host:_:chan:_) = [x | x <- T.split delims head, not $ T.null x]
    delims c                  = case c of
                                    ' ' -> True
                                    '!' -> True
                                    '~' -> True
                                    '@' -> True
                                    _   -> False
