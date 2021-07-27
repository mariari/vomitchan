module Bot.Modifier where

import qualified Data.Vector as V
import qualified Data.Text   as T
import Control.Monad.IO.Class

data Color = White | Black | Blue  | Green | Red  | Brown | Purple | Orange | Yellow | LGreen
           | Teal  | LCyan | LBlue | Pink  | Grey | LGrey
           deriving (Enum, Show)

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

-- | @Occurence@ represents effects which may be picked out of a hat,
-- or are set in stone
data Occurence
  -- | @Pick@ represents the choice to select an arbitrary effect
  = Pick ChoiceEffects
  -- | @Set@ represents a set effect over the entire block
  | Set TextEffects

-- | @Scope@ talks about effects that change the effects of the text
data Scope =
  Scope
    { -- | @Block@ represents a block change over the entire Unit
      block :: [Occurence],
      -- | @Individual@ represents a change over individual characters,
      -- these are always a pick of individual effects
      individual :: [ChoiceEffects]
    }

-- | @Unit@ is the Effect over any particular representation of text,
-- we break this up into scoped effects over text
data Unit
  = Unit Scope TextUnit

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
addIndividualEffs (Unit (Scope block indiv) text) effs  =
  Unit (Scope block (indiv <> effs)) text

addBlockEffs :: Unit -> [Occurence] -> Unit
addBlockEffs (Unit (Scope block indiv) text) effs  =
  Unit (Scope (block <> effs) indiv) text

unitToText :: Unit -> T.Text
unitToText (Unit _ (Text t)) = t
unitToText (Unit _ (Link t)) = t
unitToText (Unit _ (NonModifiable t)) = t

--------------------------------------------------------------------------------
-- Modifier Reduction system
--------------------------------------------------------------------------------


-- -- | Used as a generic version for making bold, italic, underlined, colors, and swap, for strings
-- action :: TextEffects -> String -> String
-- action eff txt = seff <> payload eff <> seff
--   where
--     seff              = show eff
--     payload (Color c) = showColor (fromEnum c) <> txt
--     payload _         = txt

-- -- | same as action, but takes text instead
-- actionT :: TextEffects -> T.Text -> T.Text
-- actionT eff txt = seff <> payload eff <> seff
--   where
--     seff              = T.pack $ show eff
--     payload (Color c) = T.pack (showColor (fromEnum c)) <> txt
--     payload _         = txt

-- for applyEffects we must chose a specific effect for the ones that
-- vary for every effect
applyEffects :: (MonadIO m) => Unit -> m T.Text
applyEffects = undefined
