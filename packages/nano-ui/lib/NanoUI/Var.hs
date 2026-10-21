-- | Mutable state that persists across frames.
--
-- A 'Var' is a mutable reference passed to stateful widgets: @textInput draft@
-- displays @draft@ and mutates it in place on user input. Read a 'Var' with
-- 'readVar' and update it with 'writeVar' or 'modifyVar'.
--
-- > counter :: NanoUI ()
-- > counter = row $ do
-- >   n <- useVar (0 :: Int)
-- >   whenM (button "-") (modifyVar n (subtract 1))
-- >   label . T.pack . show =<< readVar n
-- >   whenM (button "+") (modifyVar n (+ 1))
--
-- = Creating Vars
--
-- * 'useVar' allocates view-local mutable state keyed by its position among
--   the positional 'Var's of its scope (apart from widget ids), for 'Int',
--   'Double', 'Float', 'Bool', 'Text' and 'NanoUI.SortCol', and enumerations
--   via 'useEnumVar'.
-- * 'newVar' allocates a 'Var' of any type in 'IO' for application-owned
--   state.
-- * 'VarMap' stores one 'Var' per key for dynamic collections.
-- * @'constVar' x@ is a read-only 'Var' that always reads as @x@ and discards
--   writes, useful for inputs driven by derived state whose changes are handled
--   via the primed widget's 'NanoUI.Response'.
--
-- With @OverloadedRecordDot@, @v.field@ focuses a 'Var' on a single field of a
-- single-constructor 'Generic' record. 'forEachVar' projects a 'Var' of a list
-- into a 'Var' per element.
--
-- = Rerun semantics
--
-- A view normally runs once per frame. Mutating a 'Var' that was already read
-- earlier in the same pass invalidates the earlier read and triggers a second
-- pass before rendering. Reading a 'Var' after the widget that mutates it
-- avoids the extra pass. Record fields are tracked independently, so reading
-- @m.title@ before a widget mutates @m.name@ also runs in a single pass.
module NanoUI.Var
  ( -- * Vars
    Var
  , constVar
  , newVar
  , useVar
  , useEnumVar
  , VarSlot

    -- * Reading and writing
  , readVar
  , peekVar
  , writeVar
  , modifyVar
  , readVarIO
  , writeVarIO
  , modifyVarIO

    -- * Parts of a value
  , zoomVar
  , zoomVarL

    -- * Lists
  , forEachVar
  , withElemVars
  , VarMap
  , newVarMap
  , varAt
  , dropVar
  , retainVars

    -- * Plain values
  , withValue
  )
where

import NanoUI.Internal.Var
import NanoUI.Internal.Var.List
import NanoUI.Internal.Var.Map
import NanoUI.Internal.Var.Types
