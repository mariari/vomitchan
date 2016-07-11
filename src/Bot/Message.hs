{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION ---
module Bot.Message (
  toMessage,
  handlePM,
  respond
) where


--- IMPORTS ---
import qualified Data.Text        as T
import qualified Data.Text.Format as T
import qualified Data.Text.IO     as T

import           Bot.Commands
import           Bot.MessageType


--- FUNCTIONS ---

-- handles privmsgs and creates responses
handlePM :: Message -> Maybe (T.Text, T.Text)
handlePM msg = getCmd msg >>= \cmd -> cmd msg

-- takes an IRC message and generates the correct response
respond :: T.Text -> Maybe (T.Text, T.Text)
respond msg
    | "PING" `T.isPrefixOf` msg   = Just ("PONG", ':' `T.cons` T.drop 6 msg)
    | "PRIVMSG" `T.isInfixOf` msg = handlePM $ toMessage msg
    | otherwise                   = Nothing
