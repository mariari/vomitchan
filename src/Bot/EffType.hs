{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.EffType where

import qualified Data.Text as T
import           Control.Monad.Reader

import Bot.MessageType
import Bot.StateType

-- type of all command functions
type Cmd m    = MonadReader InfoPriv m
type CmdImp m = (Cmd m, MonadIO m)
type Func     = Response (T.Text, T.Text)

-- | Expresses the various effects *chink pinch*
type Effect m = T.Text -> m T.Text

-- | Continuation type for Func that encodes sending an effect in
type ContFunc m = m (Effect m -> m Func)

type ContFuncPure m m' = m (Effect m' -> m' Func)

toReaderImp :: (MonadIO m, MonadReader r m) => (r -> IO b) -> m b
toReaderImp = liftIO <=< reader
