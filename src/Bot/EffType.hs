{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.EffType (Cmd, CmdImp, Func, toReaderImp) where

import qualified Data.Text as T
import           Control.Monad.Reader
import           Bot.MessageType
import           Bot.StateType

-- type of all command functions
type Cmd m    = MonadReader Info m
type CmdImp m = (Cmd m, MonadIO m)
type Func     = Response (T.Text, T.Text)


toReaderImp :: (MonadIO m, MonadReader r m) => (r -> IO b) -> m b
toReaderImp = (liftIO =<<) . reader


