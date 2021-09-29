{-# LANGUAGE NamedFieldPuns #-}
module Bot.Modifier where

import qualified Data.Maybe  as Maybe
import qualified Data.List   as List
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Data.Set    as Set

import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Applicative    ((<|>))

import qualified System.Random as Random

import qualified Bot.Misc      as Misc

data Color = White | Black | Blue  | Green | Red  | Brown | Purple | Orange | Yellow | LGreen
           | Teal  | LCyan | LBlue | Pink  | Grey | LGrey
           deriving (Enum, Show, Eq)

ansiColor :: V.Vector TextEffects
ansiColor = V.fromList (Color <$> [White .. LGrey])

-- underline and strikeThrough are new
data TextEffects = Bold      | Italics   | Color Color
                 | UnderLine | Reverse   | Hex
                 | Reset     | MonoSpace | None
                 | Strikethrough | Txt String -- Space is a hack!


instance Show TextEffects where
  show Bold          = "\x2"
  show Italics       = "\x1D"
  show UnderLine     = "\x1F"
  show Strikethrough = "\x1E"
  show MonoSpace     = "\x11"
  show (Color _)     = "\x3"
  show Hex           = "\x4"
  show Reverse       = "\x16"
  show Reset         = "\xF"
  show (Txt s)       = s
  show None          = ""

-- | @TextUnit@ represents the Text unit of a document, this can be
-- files, IRC specifies, or just normal text
data TextUnit
  = Text T.Text
  | Link T.Text
  | NonModifiable T.Text
  deriving (Show)

-- | @Occurence@ represents effects which may be picked out of a hat,
-- or are set in stone
data Occurence
  -- | @Pick@ represents the choice to select an arbitrary effect
  = Pick ChoiceEffects
  -- | @Set@ represents a set effect over the entire block
  | Set TextEffects
  deriving (Show)

-- | @Scope@ talks about effects that change the effects of the text
data Scope =
  Scope
    { -- | @Block@ represents a block change over the entire Unit
      block :: [Occurence],
      -- | @Individual@ represents a change over individual characters,
      -- these are always a pick of individual effects
      individual :: [ChoiceEffects]
    }
    deriving (Show)

-- | @Unit@ is the Effect over any particular representation of text,
-- we break this up into scoped effects over text
data Unit = Unit Scope TextUnit
          deriving (Show)

-- | @ChoiceEffects@ is the effects we chose from when picking an
-- effect.
type ChoiceEffects = V.Vector TextEffects

type Representation = [Unit]

type T = Representation

effText :: T.Text -> Unit
effText = Unit (Scope [] []) . Text

effLink :: T.Text -> Unit
effLink = Unit (Scope [] []) . Link

effNonModifiable :: T.Text -> Unit
effNonModifiable = Unit (Scope [] []) . NonModifiable

addIndividualEffs :: Unit -> [V.Vector TextEffects] -> Unit
addIndividualEffs (Unit Scope {block, individual} text) effs =
  Unit (Scope {block, individual = individual <> effs})
       text

addBlockEffs :: Unit -> [Occurence] -> Unit
addBlockEffs (Unit Scope {block, individual} text) effs =
  Unit (Scope {block = block <> effs, individual})
       text

unitToText :: Unit -> T.Text
unitToText (Unit _ (Text t)) = t
unitToText (Unit _ (Link t)) = t
unitToText (Unit _ (NonModifiable t)) = t

--------------------------------------------------------------------------------
-- Modifier Reduction system
--------------------------------------------------------------------------------

-- Naive boundary logic, ideally we can be a bit smarter about this
toText :: T -> IO T.Text
toText = fmap T.concat . traverse applyEffects

-- for applyEffects we must chose a specific effect for the ones that
-- vary for every effect
applyEffects :: Unit -> IO T.Text
applyEffects (Unit Scope {block, individual} texi) =
  case texi of
    Text t -> textLinkBody t
    Link l -> textLinkBody l
    NonModifiable text -> pure text
  where
    blockEffs = fmap Maybe.catMaybes (traverse occurenceToEffect block)

    effectsOutOfScopeStr nextClosure closure =
      concat $ Set.toList (effectsOutOfScope nextClosure closure)
    effectsInScopeStr nextClosure closure =
      concat $ Set.toList (effectsInScope nextClosure closure)

    textLinkBody text = do
      blockEffs <- blockEffs
      let startingClosure = newClosure blockEffs
          initialEffs     = effectsInScopeStr startingClosure emptyClosure
          textStr         = T.unpack text
      (lastClosure, resultingStr) <-
        foldM (\ (closure, textStr) char -> do
                effects <- fmap Maybe.catMaybes (traverse choiceEffectToEffect individual)
                -- to get the starting effects see the differences
                -- between what enters scope, by doing
                -- effectsOutOfScope in reverse.
                let nextClosure = computeNextEffects closure effects
                    endingEffs  = effectsOutOfScopeStr nextClosure closure
                    startingEff = effectsInScopeStr nextClosure closure
                pure (nextClosure, textStr <> (endingEffs <> startingEff <> [char])))
              (startingClosure, "")
              textStr
      let lastEffectEnding = effectsOutOfScopeStr emptyClosure lastClosure
      pure (T.pack (initialEffs <> resultingStr <> lastEffectEnding))

-- | @occurenceToEffect@ turns an @Occurence@ into a @TextEffect@,
-- chosing an arbitrary effect in the pick case
occurenceToEffect :: Occurence -> IO (Maybe TextEffects)
occurenceToEffect (Set eff)     = pure (Just eff)
occurenceToEffect (Pick choice) = choiceEffectToEffect choice

choiceEffectToEffect :: ChoiceEffects -> IO (Maybe TextEffects)
choiceEffectToEffect choice
  | V.null choice = pure Nothing
  | otherwise     = Just . Misc.randElem choice <$> Random.randomIO

------------------------------------------------------------
-- Effect Closure Operations
------------------------------------------------------------
-- The effect closure operations maintain a closure of what effects are
-- currently in scope.

-- We will use this to determine what effects need to be ended at what
-- point


data Closure
  = Clos
    { -- blockEffs don't get cleared, but instead are included in the
      -- algorithm in every step
      blockEffs  :: Set.Set String
      -- The Current effect set
    , current    :: Set.Set String
      -- This represents the color of the current character
    , color      :: Maybe Color
      -- This represents the block color which can be temporarily
      -- overwritten
    , blockColor :: Maybe Color
    }
    deriving (Show, Eq)


newClosure :: [TextEffects] -> Closure
newClosure effs =
  Clos { current = blockEffs
       , blockEffs
       , blockColor
       , color = Nothing
       }
  where
    (removeColor, blockColor) = computeColorFromEffList effs
    blockEffs = Set.fromList (fmap show removeColor)

emptyClosure :: Closure
emptyClosure = newClosure []

-- | @effectsInScope@ we compute the effects that are entering scope
-- for the next iteration
effectsInScope :: Closure -> Closure -> Set.Set String
effectsInScope nextClosure currentClosure =
  Set.insert
    (startColor (colorFromClosure currentClosure) (colorFromClosure nextClosure))
    (Set.difference (current nextClosure) (current currentClosure))

-- | @effectsOutOfScope@ we compute the effects that are leaving scope
-- for the next iteration
effectsOutOfScope :: Closure -> Closure -> Set.Set String
effectsOutOfScope nextClosure currentClosure =
  Set.insert
    (endColor (colorFromClosure currentClosure) (colorFromClosure nextClosure))
    (Set.difference (current currentClosure) (current nextClosure))

colorFromClosure closure =
  color closure <|> blockColor closure

startColor :: Maybe Color -> Maybe Color -> String
startColor old new =
  case (old, new) of
    (Nothing, Just c1) -> show (Color c1) <> showColor (fromEnum c1)
    (Just c1, Nothing) -> ""
    (Nothing, Nothing) -> ""
    (Just c1, Just c2)
      | c1 == c2 -> ""
      | otherwise -> show (Color c2) <> showColor (fromEnum c2)

endColor :: Maybe Color -> Maybe Color -> String
endColor old new =
  case (old, new) of
    (Nothing, Just c1) -> ""
    (Just c1, Nothing) -> show (Color c1)
    (Nothing, Nothing) -> ""
    (Just c1, Just c2) -> ""



-- | @computeNextEffects@ we compute the next Closure based on a list
-- of effects to add.
computeNextEffects :: Closure -> [TextEffects] -> Closure
computeNextEffects Clos {current, blockEffs, color, blockColor} effs =
  Clos { blockEffs
       , current = foldr Set.insert blockEffs (fmap show removeColor)
       , color   = newColor
       , blockColor
       }
  where
    (removeColor, newColor) = computeColorFromEffList effs

computeColorFromEffList :: [TextEffects] -> ([TextEffects], Maybe Color)
computeColorFromEffList effs = (removeColor, fmap unboxColorErr color)
  where
    removeColor = filter (not . isColor) effs

    color = List.find isColor effs

    isColor (Color _) = True
    isColor _         = False

    unboxColorErr (Color c) = c
    unboxColorErr _ = error "does not happen"


-- in the IRC protocol if numbers don't have at least 2 digits, then
-- changing the color before a number would invalidate it
showColor :: (Ord a, Num a, Show a) => a -> String
showColor x
  | x < 10    = "0" <> show x
  | otherwise = show x
