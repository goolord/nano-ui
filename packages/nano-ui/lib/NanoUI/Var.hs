-- | State that outlives a frame.
--
-- A 'Var' holds a value the view shows and the user edits. Widgets take
-- Vars: @textInput draft@ shows @draft@ and stores the user's edits in it,
-- with no setter to call. Read a Var with 'readVar' where you show it, and
-- change it with 'writeVar' or 'modifyVar'.
--
-- > counter :: NanoUI ()
-- > counter = row $ do
-- >   n <- useVar (0 :: Int)
-- >   whenM (button "-") (modifyVar n (subtract 1))
-- >   label . T.pack . show =<< readVar n
-- >   whenM (button "+") (modifyVar n (+ 1))
--
-- = Where Vars come from
--
-- * 'useVar' keeps a Var at its position in the view, for small local state
--   of a few types: 'Int', 'Double', 'Float', 'Bool', 'Text', and an
--   enumeration through 'useEnumVar'.
-- * 'newVar' makes a Var of any type in IO, for state the application owns:
--   make it once, keep it in a record, and pass it to the view.
-- * A 'VarMap' holds one Var per key, for the rows of a list.
-- * @'constVar' x@ is a constant: the widget shows @x@ and an edit is gone
--   on the next frame, for an input showing a value derived from other
--   state, its change handled through the primed variant's response.
--
-- @v.field@ (with @OverloadedRecordDot@) is a Var for one field of the
-- record in @v@, for a record type with one constructor and a 'Generic'
-- instance. 'forEachVar' gives each element of a list in a Var its own Var.
--
-- = What a change costs
--
-- A view runs once per frame. When code writes a Var that something earlier
-- in the same run already read, what that reader drew is stale, so the view
-- runs a second time before anything is drawn. Read a Var after the widgets
-- that edit it and typing into a field costs one run. Fields count apart:
-- showing @m.title@ above a field that edits @m.name@ costs one run too.
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
