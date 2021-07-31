module Modifier where

import qualified Data.Vector as V

import Bot.Modifier
-- TODO ∷ add HUnit tests

emptyTest =
  applyEffects
    (Unit Scope {block = [], individual = []}
       (Text "Hello My Name is Bob"))

testNoOveride =
  applyEffects
    (Unit (Scope
             { block = [Set Italics, Set (Color White)]
             , individual = [V.fromList [Color White]]
             })
          (Text "Hello My Name is Bob"))

testFixOveride =
  applyEffects
    (Unit (Scope
             { block = [Set Italics, Set (Color White)]
             , individual = [V.fromList [Color Black]]
             })
          (Text "Hello My Name is Bob"))

testStartColor =
  applyEffects
    (Unit (Scope
             { block = []
             , individual = [V.fromList [Color Black]]
             })
          (Text "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZz"))

testAnsiColor =
  applyEffects
    (Unit (Scope
             { block = []
             , individual = [ansiColor]
             })
          (Text "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZz"))


testAlternatingWithOverlap =
  applyEffects
    (Unit (Scope
             { block = []
             , individual = [V.fromList [Italics, Bold], V.fromList [Bold]]
             })
          (Text "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZz"))
