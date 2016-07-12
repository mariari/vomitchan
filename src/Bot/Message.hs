{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION ---
module Bot.Message (
  respond
) where


--- IMPORTS ---
import qualified Data.Text        as T
import qualified Data.Text.Format as T
import qualified Data.Text.IO     as T

import           Bot.Commands
import           Bot.MessageType


--- FUNCTIONS ---

-- takes an IRC message and generates the correct response
respond :: T.Text -> IO (Maybe (T.Text, T.Text))
respond msg
  | "PING"   `T.isPrefixOf` msg = return $ Just ("PONG", T.drop 5 msg)
  | "PRIVMSG" `T.isInfixOf` msg = runCmd $ toMessage msg
  | otherwise                   = return Nothing
