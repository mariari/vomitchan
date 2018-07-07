module Bot.Misc (bindM) where

import Control.Monad (join)

bindM :: (Applicative f, Monad m, Traversable m) => (a1 -> f (m a2)) -> m a1 -> f (m a2)
bindM f a = join <$> traverse f a
