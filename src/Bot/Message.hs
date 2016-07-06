{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION ---
module Bot.Message (
  Server,
  Port,
  Nick,
  User,
  Host,
  Pass,
  Chan,
  Message,
  toMessage,
  handlePM,
  respond
) where


--- IMPORTS ---
import qualified Data.Text        as T
import qualified Data.Text.Format as T
import qualified Data.Text.IO     as T


--- TYPES ---

-- types for IRC data
type Server = T.Text
type Port   = Integer
type Nick   = T.Text
type User   = T.Text
type Host   = T.Text
type Pass   = T.Text
type Chan   = T.Text


--- DATA STRUCTURES ---

-- IRC message structure
data Message = Message
             { msgNick    :: Nick
             , msgUser    :: User
             , msgHost    :: Host
             , msgChan    :: Chan
             , msgContent :: T.Text
             } deriving (Show)


--- FUNCTIONS ---

-- converts a string to a Message
toMessage :: T.Text -> Message
toMessage str = Message nick user host chan content
    where
        (head,body)                 = T.breakOn ":" (T.drop 1 str)
        content                     = T.drop 1 body
        (nick:user:host:_:chan:_)   = [x | x <- T.split delims head, not $ T.null x]
        delims c                    = case c of
                                           ' ' -> True
                                           '!' -> True
                                           '~' -> True
                                           '@' -> True
                                           _   -> False


-- handles privmsgs and creates responses
handlePM :: Message -> Maybe (T.Text, T.Text)
handlePM msg
  | prefix ".quit" && msgUser msg == "MrDetonia" = Just ("QUIT",":Exiting")
  | prefix ".bots"                               = Just ("PRIVMSG", dest `T.append` " :I am a bot written by MrDetonia in Haskell | https://gitla.in/MrDetonia/detoniabot")
  | otherwise                                    = Nothing

  where
    prefix p = p `T.isPrefixOf` msgContent msg
    dest     = if "#" `T.isPrefixOf` msgChan msg then msgChan msg else msgNick msg


-- takes an IRC message and generates the correct response
respond :: T.Text -> Maybe (T.Text, T.Text)
respond msg
    | "PING" `T.isPrefixOf` msg   = Just ("PONG", ':' `T.cons` T.drop 6 msg)
    | "PRIVMSG" `T.isInfixOf` msg = handlePM $ toMessage msg
    | otherwise                   = Nothing
