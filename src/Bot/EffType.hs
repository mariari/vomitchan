{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.EffType where

import qualified Data.Text as T
import           Control.Monad.Reader
import qualified Data.Vector as V

import Bot.MessageType
import Bot.StateType
import Bot.Modifier as Modifier

-- type of all command functions
type Cmd m    = MonadReader InfoPriv m
type CmdImp m = (Cmd m, MonadIO m)
type Func     = Response (Action, Modifier.T)

-- | @Action@ is the Action of the Text
type Action = T.Text

-- | Extra Paramater to send into effect functions
newtype Extra =
  Extra { validEffects :: V.Vector TextEffects }

-- | Expresses the various effects *chink pinch*
type Effect m = Extra -> Modifier.T -> m Modifier.T

-- | Continuation type for Func that encodes sending an effect in
type ContFunc m = m (Effect m -> m Func)

type ContFuncPure m m' = m (Effect m' -> m' Func)

toReaderImp :: (MonadIO m, MonadReader r m) => (r -> IO b) -> m b
toReaderImp = liftIO <=< reader
