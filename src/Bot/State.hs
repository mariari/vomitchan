{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.State (
  GlobalState
) where
--- IMPORTS -----------------------------------------------------------------------------------
import qualified Data.Text       as T
import           Text.Regex.TDFA
--- TYPES -------------------------------------------------------------------------------------
--- DATA STRUCTURES ---------------------------------------------------------------------------


-- IRC State information
data GlobalState = GlobalState
                 { dreamMode       :: Bool

                 }
