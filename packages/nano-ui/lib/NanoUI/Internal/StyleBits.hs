-- | The bit layout of a node's style index
-- ('NanoUI.Internal.Layout.Arena.getStyleIdx'): one 'Int' whose bits mean
-- different things per 'NanoUI.Internal.Layout.Arena.NodeType'. Every field
-- is a 'BitField' constant here, and every encoder and decoder reads its
-- shift and width from one of them. 'styleLayouts' lists the fields each
-- node type packs together; the @style-bits-spec@ suite checks that none of
-- them overlap and that each fits in 64 bits.
--
-- This module imports only @base@, so that suite compiles it on its own.
--
-- @
-- bits    field             node types
-- ------  ----------------  ----------------------------------------------
-- 0-3     textFace          text font: NodeText, NodeTextInput,
-- 4-7     stripe            NodeTextArea, and a button's or select's
-- 8-11    textWeight        labelFont (moved up 32 bits)
-- 12-13   textSlant
-- 14-15   textDecoration
-- 18-20   textTone          tone plus one, 0 for none
-- 26      inputSearch       NodeTextInput flags
-- 28      inputSelectable
-- 29      inputPassword
-- 30      inputNumeric
-- 0-23    buttonVisual      NodeButton: the bits below the type flags
-- 0-1     buttonVariant     close button (1 trailing, 2 tab), choice (1 radio)
-- 0-1     tabStyle          tab header ('buttonTab')
-- 2-3     tabOrient
-- 0-7     treeDepth         tree row ('buttonRow')
-- 8       treeHasKids
-- 9       treeExpanded
-- 10      treeOdd
-- 16-17   headerSortMark    table header ('buttonTable')
-- 20-22   buttonTone        plain toggle button: tone plus one
-- 24-31   button type flags row, choice, content, menu bar, menu, close,
--                           tab, table (one bit each, in that order)
-- 32-63   labelFont         NodeButton, NodeSelect: a text font packing
-- 0       containerInert    NodeContainer
-- 1       containerClip
-- 8-9     chromePart        tab strip chrome
-- 10-11   chromeStyle
-- 12-13   chromeOrient
-- 4-7     stripe            NodePanel: a table cell's row stripe
-- 0-1     scrollPolicyX     NodeScrollContainer
-- 2-3     scrollPolicyY
-- 4       scrollMarker      always set
-- 5       scrollBare
-- 0-1     pickerPart        NodeColorPicker
-- @
--
-- A @NodeBox@ keeps its fill colour's 32-bit word and a @NodeImage@ its look
-- in the whole index; the other node types keep nothing there.
module NanoUI.Internal.StyleBits
  ( BitField (..)
  , fieldEnd
  , fieldMask
  , getField
  , packField
  , placeField
  , testField
  -- * Text font
  , textFace
  , stripe
  , textWeight
  , textSlant
  , textDecoration
  , textTone
  , textFontFields
  , textFontKeyMask
  -- * Text field flags
  , inputSearch
  , inputSelectable
  , inputPassword
  , inputNumeric
  -- * Button
  , buttonVisual
  , buttonVariant
  , buttonTone
  , buttonRow
  , buttonChoice
  , buttonContent
  , buttonMenuBar
  , buttonMenu
  , buttonClose
  , buttonTab
  , buttonTable
  , buttonTypeFlags
  , tabStyle
  , tabOrient
  , treeDepth
  , treeHasKids
  , treeExpanded
  , treeOdd
  , headerSortMark
  , labelFont
  -- * Container
  , containerInert
  , containerClip
  , chromePart
  , chromeStyle
  , chromeOrient
  -- * Scroll container
  , scrollPolicyX
  , scrollPolicyY
  , scrollMarker
  , scrollBare
  -- * Colour picker
  , pickerPart
  -- * Layouts
  , styleLayouts
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)

-- | @bfWidth@ bits of a style index starting at bit @bfShift@. The fields
-- below are INLINE constants, so 'getField' and 'packField' on one of them
-- compile to the shift and mask literals they replace.
data BitField = BitField
  { bfShift :: !Int
  , bfWidth :: !Int
  }
  deriving (Eq, Show)

-- | The bit after a field's last one.
{-# INLINE fieldEnd #-}
fieldEnd :: BitField -> Int
fieldEnd f = bfShift f + bfWidth f

{-# INLINE lowMask #-}
lowMask :: BitField -> Int
lowMask f = (1 `shiftL` bfWidth f) - 1

-- | A field's bits in place.
{-# INLINE fieldMask #-}
fieldMask :: BitField -> Int
fieldMask f = lowMask f `shiftL` bfShift f

-- | The value a style index holds in a field.
{-# INLINE getField #-}
getField :: BitField -> Int -> Int
getField f si = (si `shiftR` bfShift f) .&. lowMask f

-- | @v@ cut to the field's width and moved into place.
{-# INLINE packField #-}
packField :: BitField -> Int -> Int
packField f v = (v .&. lowMask f) `shiftL` bfShift f

-- | @v@ moved into place, for a value already known to fit the field.
{-# INLINE placeField #-}
placeField :: BitField -> Int -> Int
placeField f v = v `shiftL` bfShift f

-- | Whether any bit of the field is set.
{-# INLINE testField #-}
testField :: BitField -> Int -> Bool
testField f si = si .&. fieldMask f /= 0

-- Text font ----------------------------------------------------------------

-- | The face of the layout's 'NanoUI.Internal.Style.FontVariant'.
{-# INLINE textFace #-}
textFace :: BitField
textFace = BitField 0 4

-- | A row stripe code (0 none, 1 even row, 2 odd row), on a text node or on
-- a table cell's panel.
{-# INLINE stripe #-}
stripe :: BitField
stripe = BitField 4 4

{-# INLINE textWeight #-}
textWeight :: BitField
textWeight = BitField 8 4

{-# INLINE textSlant #-}
textSlant :: BitField
textSlant = BitField 12 2

{-# INLINE textDecoration #-}
textDecoration :: BitField
textDecoration = BitField 14 2

-- | The 'NanoUI.Internal.Style.Tone' plus one, or 0 for none.
{-# INLINE textTone #-}
textTone :: BitField
textTone = BitField 18 3

-- | Every field of a text font packing, which 'labelFont' must hold.
textFontFields :: [(String, BitField)]
textFontFields =
  [ ("textFace", textFace)
  , ("stripe", stripe)
  , ("textWeight", textWeight)
  , ("textSlant", textSlant)
  , ("textDecoration", textDecoration)
  , ("textTone", textTone)
  ]

-- | The fields that pick the font a text node is measured in: face, weight
-- and slant.
{-# INLINE textFontKeyMask #-}
textFontKeyMask :: Int
textFontKeyMask = fieldMask textFace .|. fieldMask textWeight .|. fieldMask textSlant

-- Text field flags ---------------------------------------------------------

{-# INLINE inputSearch #-}
inputSearch :: BitField
inputSearch = BitField 26 1

{-# INLINE inputSelectable #-}
inputSelectable :: BitField
inputSelectable = BitField 28 1

{-# INLINE inputPassword #-}
inputPassword :: BitField
inputPassword = BitField 29 1

{-# INLINE inputNumeric #-}
inputNumeric :: BitField
inputNumeric = BitField 30 1

-- Button -------------------------------------------------------------------

-- | A button's own bits below its type flags. Each kind reads them its own
-- way ('buttonVariant', 'tabStyle', 'treeDepth', 'headerSortMark',
-- 'buttonTone').
{-# INLINE buttonVisual #-}
buttonVisual :: BitField
buttonVisual = BitField 0 24

-- | The variant of a close button (1 trailing, 2 tab header) or a choice (1
-- radio).
{-# INLINE buttonVariant #-}
buttonVariant :: BitField
buttonVariant = BitField 0 2

-- | The 'NanoUI.Internal.Style.Tone' plus one that a plain toggle button fills with, or 0 for the
-- accent.
{-# INLINE buttonTone #-}
buttonTone :: BitField
buttonTone = BitField 20 3

{-# INLINE buttonRow #-}
buttonRow :: BitField
buttonRow = BitField 24 1

{-# INLINE buttonChoice #-}
buttonChoice :: BitField
buttonChoice = BitField 25 1

{-# INLINE buttonContent #-}
buttonContent :: BitField
buttonContent = BitField 26 1

{-# INLINE buttonMenuBar #-}
buttonMenuBar :: BitField
buttonMenuBar = BitField 27 1

{-# INLINE buttonMenu #-}
buttonMenu :: BitField
buttonMenu = BitField 28 1

{-# INLINE buttonClose #-}
buttonClose :: BitField
buttonClose = BitField 29 1

{-# INLINE buttonTab #-}
buttonTab :: BitField
buttonTab = BitField 30 1

{-# INLINE buttonTable #-}
buttonTable :: BitField
buttonTable = BitField 31 1

-- | The one-bit type flags of a button, as 'styleLayouts' lists them.
buttonTypeFlags :: [(String, BitField)]
buttonTypeFlags =
  [ ("buttonRow", buttonRow)
  , ("buttonChoice", buttonChoice)
  , ("buttonContent", buttonContent)
  , ("buttonMenuBar", buttonMenuBar)
  , ("buttonMenu", buttonMenu)
  , ("buttonClose", buttonClose)
  , ("buttonTab", buttonTab)
  , ("buttonTable", buttonTable)
  ]

-- | A tab header's tab style (0-3).
{-# INLINE tabStyle #-}
tabStyle :: BitField
tabStyle = BitField 0 2

-- | A tab header's strip orientation (0-3); 2 and up are vertical strips.
{-# INLINE tabOrient #-}
tabOrient :: BitField
tabOrient = BitField 2 2

{-# INLINE treeDepth #-}
treeDepth :: BitField
treeDepth = BitField 0 8

{-# INLINE treeHasKids #-}
treeHasKids :: BitField
treeHasKids = BitField 8 1

{-# INLINE treeExpanded #-}
treeExpanded :: BitField
treeExpanded = BitField 9 1

{-# INLINE treeOdd #-}
treeOdd :: BitField
treeOdd = BitField 10 1

-- | A table header's sort mark: 0 none, 1 ascending, 2 descending.
{-# INLINE headerSortMark #-}
headerSortMark :: BitField
headerSortMark = BitField 16 2

-- | A button's or select's label font, packed as a text node packs one.
{-# INLINE labelFont #-}
labelFont :: BitField
labelFont = BitField 32 32

-- Container ----------------------------------------------------------------

{-# INLINE containerInert #-}
containerInert :: BitField
containerInert = BitField 0 1

{-# INLINE containerClip #-}
containerClip :: BitField
containerClip = BitField 1 1

-- | The 'NanoUI.Internal.WidgetText.TabChrome' a tab strip's container
-- paints.
{-# INLINE chromePart #-}
chromePart :: BitField
chromePart = BitField 8 2

{-# INLINE chromeStyle #-}
chromeStyle :: BitField
chromeStyle = BitField 10 2

{-# INLINE chromeOrient #-}
chromeOrient :: BitField
chromeOrient = BitField 12 2

-- Scroll container ---------------------------------------------------------

{-# INLINE scrollPolicyX #-}
scrollPolicyX :: BitField
scrollPolicyX = BitField 0 2

{-# INLINE scrollPolicyY #-}
scrollPolicyY :: BitField
scrollPolicyY = BitField 2 2

-- | Always set, so that no scroll node's style index is 0.
{-# INLINE scrollMarker #-}
scrollMarker :: BitField
scrollMarker = BitField 4 1

{-# INLINE scrollBare #-}
scrollBare :: BitField
scrollBare = BitField 5 1

-- Colour picker ------------------------------------------------------------

-- | The 'NanoUI.Internal.Widgets.ColorPicker.ColorPickerPart' a node paints.
{-# INLINE pickerPart #-}
pickerPart :: BitField
pickerPart = BitField 0 2

-- Layouts ------------------------------------------------------------------

-- | The fields each node type packs into one style index, by name. A button
-- appears once per kind, since each kind reads 'buttonVisual' its own way.
styleLayouts :: [(String, [(String, BitField)])]
styleLayouts =
  [ ("NodeText", textFontFields)
  , ("NodeTextArea", textFontFields)
  , ( "NodeTextInput"
    , textFontFields
        <> [ ("inputSearch", inputSearch)
           , ("inputSelectable", inputSelectable)
           , ("inputPassword", inputPassword)
           , ("inputNumeric", inputNumeric)
           ]
    )
  , ("NodeButton plain", button [("buttonTone", buttonTone)])
  , ("NodeButton close or choice", button [("buttonVariant", buttonVariant)])
  , ("NodeButton tab", button [("tabStyle", tabStyle), ("tabOrient", tabOrient)])
  , ( "NodeButton tree row"
    , button
        [ ("treeDepth", treeDepth)
        , ("treeHasKids", treeHasKids)
        , ("treeExpanded", treeExpanded)
        , ("treeOdd", treeOdd)
        ]
    )
  , ("NodeButton table header", button [("headerSortMark", headerSortMark)])
  , ("NodeSelect", [("labelFont", labelFont)])
  , ( "NodeContainer"
    , [ ("containerInert", containerInert)
      , ("containerClip", containerClip)
      , ("chromePart", chromePart)
      , ("chromeStyle", chromeStyle)
      , ("chromeOrient", chromeOrient)
      ]
    )
  , ("NodePanel", [("stripe", stripe)])
  , ( "NodeScrollContainer"
    , [ ("scrollPolicyX", scrollPolicyX)
      , ("scrollPolicyY", scrollPolicyY)
      , ("scrollMarker", scrollMarker)
      , ("scrollBare", scrollBare)
      ]
    )
  , ("NodeColorPicker", [("pickerPart", pickerPart)])
  ]
  where
    button own = own <> buttonTypeFlags <> [("labelFont", labelFont)]
