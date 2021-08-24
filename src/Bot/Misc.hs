module Bot.Misc (bindM, randElems, randElem, randElemList) where

import           Control.Monad (join)
import qualified System.Random as Random
import qualified Data.Vector   as V

bindM :: (Applicative f, Monad m, Traversable m) => (a1 -> f (m a2)) -> m a1 -> f (m a2)
bindM f a = join <$> traverse f a


--I know there is an abstraction to be made here but w/e
randElemsList :: [a] -> Int -> [a]
randElemsList xs = fmap (xs !!)  . Random.randomRs (0, length xs - 1) . Random.mkStdGen

randElemList :: [a] -> Int -> a
randElemList xs = head . randElemsList xs


-- grabs an infinite list of random values inside some vector
randElems :: V.Vector a -> Int -> [a]
randElems xs =
  fmap (xs V.!)  . Random.randomRs (0, length xs - 1) . Random.mkStdGen

randElem :: V.Vector a -> Int -> a
randElem xs  = head . randElems xs
