{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.EffType (Cmd, CmdImp, Func) where

import qualified Data.Text as T
import           Control.Monad.Reader
import           Bot.MessageType
import           Bot.StateType

-- type of all command functions
type Cmd m    = MonadReader Message m
type CmdImp m = (MonadReader Message m, MonadIO m)
type Func    = Response (T.Text, T.Text)
