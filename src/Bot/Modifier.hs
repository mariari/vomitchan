module Bot.Modifier where

import qualified Data.Vector as V
import qualified Data.Text   as T

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


-- | @TextUnit@ represents the Text unit of a document, this can be
-- strings or just normal text
data TextUnit
  = Text T.Text
  | Link T.Text

-- | @EffectUnit@ is the Effect over any particular representation of text
data EffectUnit
  = EffectUnit [TextEffects] TextUnit

type Representation = [EffectUnit]
