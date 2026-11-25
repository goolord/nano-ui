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
-- 16      textTabular
-- 18-20   textTone          tone plus one, 0 for none
-- 21-22   textEllipsis      0 end, 1 middle, 2 start
-- 26      inputSearch       NodeTextInput flags
-- 28      inputSelectable
-- 29      inputPassword
-- 30      inputNumeric
-- 0-23    buttonVisual      NodeButton: the bits below the type flags
-- 0-1     buttonVariant     close button (1 trailing, 2 tab), choice (1 radio)
-- 0-1     tabStyle          tab header ('buttonKind' = ButtonTab)
-- 2-3     tabOrient
-- 0-7     treeDepth         tree row ('buttonKind' = ButtonRow)
-- 8       treeHasKids
-- 9       treeExpanded
-- 10      treeOdd
-- 16-17   headerSortMark    table header ('buttonKind' = ButtonTable)
-- 20-22   buttonTone        plain toggle button: tone plus one
-- 24-27   buttonKind        ButtonKind enum (0 plain .. 8 row)
-- 32-63   labelFont         NodeButton, NodeSelect: a text font packing
-- 0       containerInert    NodeContainer
-- 1       containerClip
-- 2       containerShaderEffect
-- 3       containerButtonGroup
-- 8-9     chromePart        tab strip chrome
-- 10-11   chromeStyle
-- 12-13   chromeOrient
-- 4-7     stripe            NodePanel: a table cell's row stripe
-- 0-2     scrollPolicyX     NodeScrollContainer
-- 3-5     scrollPolicyY
-- 6       scrollMarker      always set
-- 7       scrollBare
-- 8       scrollFocusable
-- 0-1     pickerPart        NodeColorPicker
-- @
--
-- A @NodeBox@ keeps its fill color's 32-bit word and a @NodeImage@ its look
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
  , textTabular
  , textTone
  , textEllipsis
  , textSingleLine
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
  , buttonKind
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
  , containerShaderEffect
  , containerButtonGroup
  , chromePart
  , chromeStyle
  , chromeOrient
  -- * Scroll container
  , scrollPolicyX
  , scrollPolicyY
  , scrollMarker
  , scrollBare
  , scrollFocusable
  -- * Color picker
  , pickerPart
  -- * Layouts
  , styleLayouts
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word64)

-- | @bfWidth@ bits of a 64-bit style index starting at bit @bfShift@. The
-- fields below are INLINE constants, so 'getField' and 'packField' on one of
-- them compile to the shift and mask literals they replace.
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
lowMask :: BitField -> Word64
lowMask f = (1 `shiftL` bfWidth f) - 1

-- | A field's bits in place.
{-# INLINE fieldMask #-}
fieldMask :: BitField -> Word64
fieldMask f = lowMask f `shiftL` bfShift f

-- | The value a 64-bit style index holds in a field.
{-# INLINE getField #-}
getField :: BitField -> Word64 -> Int
getField f si = fromIntegral ((si `shiftR` bfShift f) .&. lowMask f)

-- | @v@ cut to the field's width and moved into place.
{-# INLINE packField #-}
packField :: BitField -> Int -> Word64
packField f v = (fromIntegral v .&. lowMask f) `shiftL` bfShift f

-- | @v@ moved into place, for a value already known to fit the field.
{-# INLINE placeField #-}
placeField :: BitField -> Int -> Word64
placeField f v = fromIntegral v `shiftL` bfShift f

-- | Whether any bit of the field is set.
{-# INLINE testField #-}
testField :: BitField -> Word64 -> Bool
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

-- | Font weight ('NanoUI.Internal.Style.FontWeight').
{-# INLINE textWeight #-}
textWeight :: BitField
textWeight = BitField 8 4

-- | Font slant ('NanoUI.Internal.Style.FontStyle').
{-# INLINE textSlant #-}
textSlant :: BitField
textSlant = BitField 12 2

-- | Text decoration ('NanoUI.Internal.Style.TextDecoration').
{-# INLINE textDecoration #-}
textDecoration :: BitField
textDecoration = BitField 14 2

-- | Whether a text node uses tabular lining figures ('NanoUI.fontTabular').
{-# INLINE textTabular #-}
textTabular :: BitField
textTabular = BitField 16 1

-- | The 'NanoUI.Internal.Style.Tone' plus one, or 0 for none.
{-# INLINE textTone #-}
textTone :: BitField
textTone = BitField 18 3

-- | Where truncated single-line text places its ellipsis (0 end, 1 middle, 2 start).
{-# INLINE textEllipsis #-}
textEllipsis :: BitField
textEllipsis = BitField 21 2

-- | Whether a text node stays on one line and truncates instead of wrapping in a column.
{-# INLINE textSingleLine #-}
textSingleLine :: BitField
textSingleLine = BitField 23 1

-- | Every field of a text font packing, which 'labelFont' must hold.
textFontFields :: [(String, BitField)]
textFontFields =
  [ ("textFace", textFace)
  , ("stripe", stripe)
  , ("textWeight", textWeight)
  , ("textSlant", textSlant)
  , ("textDecoration", textDecoration)
  , ("textTabular", textTabular)
  , ("textTone", textTone)
  , ("textEllipsis", textEllipsis)
  , ("textSingleLine", textSingleLine)
  ]

-- | The fields that pick the font a text node is measured in: face, weight,
-- slant and tabular figures.
{-# INLINE textFontKeyMask #-}
textFontKeyMask :: Word64
textFontKeyMask = fieldMask textFace .|. fieldMask textWeight .|. fieldMask textSlant .|. fieldMask textTabular

-- Text field flags ---------------------------------------------------------

-- | Search input with leading icon and clear button ('NanoUI.searchInput').
{-# INLINE inputSearch #-}
inputSearch :: BitField
inputSearch = BitField 26 1

-- | Read-only selectable text node ('NanoUI.selectableText').
{-# INLINE inputSelectable #-}
inputSelectable :: BitField
inputSelectable = BitField 28 1

-- | Masked password input ('NanoUI.ticPassword').
{-# INLINE inputPassword #-}
inputPassword :: BitField
inputPassword = BitField 29 1

-- | Numeric input with steppers ('NanoUI.numericInput').
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

-- | The 'NanoUI.Internal.WidgetText.ButtonKind' enum packed in bits 24-27
-- (0 plain .. 8 tree row).
{-# INLINE buttonKind #-}
buttonKind :: BitField
buttonKind = BitField 24 4

-- | A tab header's tab style (0-3).
{-# INLINE tabStyle #-}
tabStyle :: BitField
tabStyle = BitField 0 2

-- | A tab header's strip orientation (0-3); 2 and up are vertical strips.
{-# INLINE tabOrient #-}
tabOrient :: BitField
tabOrient = BitField 2 2

-- | Indentation depth (0-255) of a tree row.
{-# INLINE treeDepth #-}
treeDepth :: BitField
treeDepth = BitField 0 8

-- | Whether a tree row has child rows.
{-# INLINE treeHasKids #-}
treeHasKids :: BitField
treeHasKids = BitField 8 1

-- | Whether a tree row's branch is currently expanded.
{-# INLINE treeExpanded #-}
treeExpanded :: BitField
treeExpanded = BitField 9 1

-- | Alternating-row stripe bit for a tree row.
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

-- | Inert container whose subtree ignores pointer and focus interaction.
{-# INLINE containerInert #-}
containerInert :: BitField
containerInert = BitField 0 1

-- | Container clips its children to its own laid-out rectangle.
{-# INLINE containerClip #-}
containerClip :: BitField
containerClip = BitField 1 1

-- | Container wraps its children in a Tier 2 shader effect ('NanoUI.withShader').
{-# INLINE containerShaderEffect #-}
containerShaderEffect :: BitField
containerShaderEffect = BitField 2 1

-- | Container joins adjacent buttons with outer-only rounding and shared
-- internal borders ('NanoUI.buttonGroup').
{-# INLINE containerButtonGroup #-}
containerButtonGroup :: BitField
containerButtonGroup = BitField 3 1

-- | The 'NanoUI.Internal.WidgetText.TabChrome' a tab strip's container
-- paints.
{-# INLINE chromePart #-}
chromePart :: BitField
chromePart = BitField 8 2

-- | Tab strip visual style (0-3) stored on a tab chrome container.
{-# INLINE chromeStyle #-}
chromeStyle :: BitField
chromeStyle = BitField 10 2

-- | Tab strip orientation (0-3) stored on a tab chrome container.
{-# INLINE chromeOrient #-}
chromeOrient :: BitField
chromeOrient = BitField 12 2

-- Scroll container ---------------------------------------------------------

-- | Horizontal 'NanoUI.ScrollPolicy' enum code (0-4).
{-# INLINE scrollPolicyX #-}
scrollPolicyX :: BitField
scrollPolicyX = BitField 0 3

-- | Vertical 'NanoUI.ScrollPolicy' enum code (0-4).
{-# INLINE scrollPolicyY #-}
scrollPolicyY :: BitField
scrollPolicyY = BitField 3 3

-- | Always set, so that no scroll node's style index is 0.
{-# INLINE scrollMarker #-}
scrollMarker :: BitField
scrollMarker = BitField 6 1

-- | Omit the scroll container's background well and border ('NanoUI.scrollBare').
{-# INLINE scrollBare #-}
scrollBare :: BitField
scrollBare = BitField 7 1

-- | Scroll container takes keyboard focus as a Tab stop and on click
-- ('NanoUI.scrollFocusable').
{-# INLINE scrollFocusable #-}
scrollFocusable :: BitField
scrollFocusable = BitField 8 1

-- Color picker -------------------------------------------------------------

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
      , ("containerShaderEffect", containerShaderEffect)
      , ("containerButtonGroup", containerButtonGroup)
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
      , ("scrollFocusable", scrollFocusable)
      ]
    )
  , ("NodeColorPicker", [("pickerPart", pickerPart)])
  ]
  where
    button own = own <> [("buttonKind", buttonKind), ("labelFont", labelFont)]
