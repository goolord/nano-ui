-- | The multi-line text area widget and its state: the document buffer,
-- caret and selection, viewport, and commands run against it.
module NanoUI.Widgets.TextArea
  ( -- * Pure state
    TextAreaState (..)
  , initTextAreaState
  , setTextAreaViewport
  , setTextAreaSelection
  , runTextAreaCommand
    -- * Widget
  , textArea
  , textArea'
  , textAreaWith
  , textAreaWith'
  , textAreaDocument
  , textAreaDocument'
  , textAreaDocumentWith
  , textAreaDocumentWith'
  , textAreaLayout
  ) where

import NanoUI.Internal.Widgets.TextArea
