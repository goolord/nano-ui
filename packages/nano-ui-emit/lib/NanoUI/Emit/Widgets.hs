-- | The core "NanoUI" widgets, which emitting views use unchanged. Kept so
-- imports of this module still resolve; "NanoUI.Emit" re-exports them.
module NanoUI.Emit.Widgets
  {-# DEPRECATED "NanoUIE views use the core NanoUI widgets; import NanoUI.Emit or NanoUI" #-}
  ( module NanoUI
  )
where

import NanoUI
