module Bot.Misc (bindM, randElems, randElem) where

import           Control.Monad (join)
import qualified System.Random as Random
import qualified Data.Vector   as V

bindM :: (Applicative f, Monad m, Traversable m) => (a1 -> f (m a2)) -> m a1 -> f (m a2)
bindM f a = join <$> traverse f a



-- grabs an infinite list of random values inside some vector
randElems :: V.Vector a -> Int -> [a]
randElems xs =
  fmap (xs V.!)  . Random.randomRs (0, length xs - 1) . Random.mkStdGen

randElem :: V.Vector a -> Int -> a
randElem xs  = head . randElems xs
