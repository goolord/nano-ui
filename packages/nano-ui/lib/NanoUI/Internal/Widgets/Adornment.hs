-- | Icons, short texts and small views drawn inside a widget beside its own
-- text: before or after a text field's value, or beside a button's label.
--
-- An adornment is a value that describes what to draw. The widget adds each
-- side's adornments as one row inside its own node, so they lay out, paint,
-- and damage inside the widget's box. A press on an icon, a text or a view
-- is a press on the widget; a control takes its own presses.
module NanoUI.Internal.Widgets.Adornment
  ( Adornment (..)
  , Adornments (..)
  , leading
  , trailing
  , icon
  , iconSized
  , affix
  , view
  , control
  , adornWidget
  , adornButton
  , adornmentRow
  )
where

import Control.Monad (forM)
import Data.IORef (readIORef)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import NanoUI.Internal.Context (Context (..), getPrevRect, isDisabled, pointerCovered)
import NanoUI.Internal.Font (FontMetrics (..))
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input (inputMousePos)
import NanoUI.Internal.Frame.Hit (nodeInteractionHit)
import NanoUI.Internal.Layout.Arena (arenaCount, getNodeType, getWidgetId, isWidgetNode, lookupNodeByWidgetId)
import NanoUI.Internal.Monad (NanoUI, askContext, askInput, ifM, resolveFontUi, liftIO, uiTheme, withDefaultLayout, (<&&>))
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Color, V2)
import NanoUI.Internal.Widgets.Behavior (keyActivated)
import NanoUI.Internal.Widgets.Display (svgIconWith')
import NanoUI.Internal.Widgets.Layout (labelEx, row')
import NanoUI.Internal.Widgets.Node
import NanoUI.Svg (Svg)

-- | An item drawn beside a widget's text in the widget's adornment colour.
data Adornment
  = AdornIcon !(Maybe Float) !Svg
  -- ^ An SVG icon, this many logical pixels square, or with 'Nothing' about
  -- as tall as a line of the widget's text. A one-colour icon takes the
  -- adornment colour, as 'NanoUI.svgIcon' takes the text colour.
  | AdornText !Text
  -- ^ A short text in the widget's font, such as a unit or a prefix.
  | AdornView (NanoUI ())
  -- ^ Any view, such as a 'NanoUI.spinner'. It runs inside the widget each
  -- frame, with the adornment colour as its default text colour. It is for
  -- display: a press on it is a press on the widget.
  | AdornControl (NanoUI ())
  -- ^ An interactive child view, such as a field's clear button. Pointer
  -- events inside it are handled by the child widgets rather than the
  -- enclosing widget (which does not report the press, take focus, or move
  -- its caret).

-- | A view shows as @AdornView \<view\>@.
instance Show Adornment where
  showsPrec d a = showParen (d > 10) $ case a of
    AdornIcon size doc -> showString "AdornIcon " . showsPrec 11 size . showChar ' ' . showsPrec 11 doc
    AdornText txt -> showString "AdornText " . showsPrec 11 txt
    AdornView _ -> showString "AdornView <view>"
    AdornControl _ -> showString "AdornControl <view>"

-- | What a widget draws before its text and after it. Adornments combine
-- side by side, each side in the order given:
--
-- > leading (icon mail) <> trailing (affix "@example.com")
data Adornments = Adornments
  { adornLeading :: ![Adornment]
  , adornTrailing :: ![Adornment]
  }
  deriving (Show)

instance Semigroup Adornments where
  Adornments l1 t1 <> Adornments l2 t2 = Adornments (l1 <> l2) (t1 <> t2)

instance Monoid Adornments where
  mempty = Adornments [] []

-- | Draw an adornment before the widget's text.
leading :: Adornment -> Adornments
leading a = Adornments [a] []

-- | Draw an adornment after the widget's text.
trailing :: Adornment -> Adornments
trailing a = Adornments [] [a]

-- | An SVG icon about as tall as a line of the widget's text.
icon :: Svg -> Adornment
icon = AdornIcon Nothing

-- | An SVG icon @size@ logical pixels square.
iconSized :: Float -> Svg -> Adornment
iconSized size = AdornIcon (Just size)

-- | A short text, such as @"kg"@ after a weight or @"https://"@ before an
-- address.
affix :: Text -> Adornment
affix = AdornText

-- | Any view, for display, such as a spinner while a field's value is
-- checked. A press on it is a press on the widget.
--
-- > trailing (view (void (spinnerWith' id 14)))
view :: NanoUI () -> Adornment
view = AdornView

-- | An interactive adornment whose widgets handle their own pointer events,
-- such as a button that toggles password visibility:
--
-- > trailing (control (whenM (buttonWith tight "Show") (setShown (not shown))))
control :: NanoUI () -> Adornment
control = AdornControl

-- | Add adornments to widget @wid@ using layout @lay@ and colour @colorOf@.
-- Returns 'True' when an 'AdornControl' captures the pointer ('holdsPointer'),
-- suppressing the enclosing widget's pointer handling.
{-# INLINE adornWidget #-}
adornWidget :: WidgetId -> Layout -> (Theme -> Color) -> Adornments -> NanoUI Bool
adornWidget _ _ _ (Adornments [] []) = pure False
adornWidget wid lay colorOf adorns = addAdornments wid lay colorOf adorns

-- | Add adornments to a button, suppressing pointer clicks on the outer button
-- (while keeping keyboard activation) when an 'AdornControl' captures the
-- pointer.
{-# INLINE adornButton #-}
adornButton :: Response -> Layout -> (Theme -> Color) -> Adornments -> NanoUI Response
adornButton resp lay colorOf adorns = do
  taken <- adornWidget (respId resp) lay colorOf adorns
  if taken then (`setClicked` inertResponse resp) <$> keyActivated (respId resp) else pure resp

-- | 'adornWidget' with something to draw: one row a side, the leading row
-- aligned to the start and the trailing one to the end ('adornRows').
{-# NOINLINE addAdornments #-}
addAdornments :: WidgetId -> Layout -> (Theme -> Color) -> Adornments -> NanoUI Bool
addAdornments wid lay colorOf (Adornments ls ts) = do
  ctx <- askContext
  let na = ctxNodeArena ctx
  inp <- askInput
  color <- colorOf <$> uiTheme
  let font = fontSize (layoutFontSize lay)
      add = \case
        AdornIcon size doc -> do
          s <- case size of
            Just n -> pure (max 0 n)
            Nothing -> do
              fm <- resolveFontUi (layoutFontSize lay) WeightNormal FontStyleNormal FontRegular
              pure (fromIntegral (round (fmLineHeight fm * 0.9) :: Int))
          Nothing <$ svgIconWith' (fixedWH s s . fontColor color) doc
        AdornText txt -> Nothing <$ labelEx (tight . font . fontColor color $ defaultLayout) txt
        AdornView v -> Nothing <$ adornmentRow lay color v
        AdornControl v -> do
          from <- liftIO (arenaCount na)
          withDefaultLayout (font . fontColor color) v
          to <- liftIO (arenaCount na)
          pure (Just (from, to))
      side align pieces
        | null pieces = pure []
        | otherwise = row' (align (adornmentRowLayout lay)) (catMaybes <$> forM pieces add)
  mRanges <- withWidgetChildren wid ((<>) <$> side alignStart ls <*> side alignEnd ts)
  case mRanges of
    Just ranges@(_ : _) -> liftIO (holdsPointer ctx wid (inputMousePos inp) ranges)
    _ -> pure False

-- | An inert row inside a widget with layout @lay@, its labels in the
-- widget's font and colour @color@.
adornmentRow :: Layout -> Color -> NanoUI a -> NanoUI a
adornmentRow lay color =
  inertContainer (adornmentRowLayout lay) . withDefaultLayout (fontSize (layoutFontSize lay) . fontColor color)

adornmentRowLayout :: Layout -> Layout
adornmentRowLayout lay = tight . gap (layoutGap lay) . alignMid $ defaultLayout {layoutDirection = Row}

-- | Whether an 'AdornControl' in one of the node ranges captures the pointer
-- (either active or hovered over an enabled widget node). When @wid@ itself
-- is active, it retains the pointer.
holdsPointer :: Context -> WidgetId -> V2 -> [(Int, Int)] -> IO Bool
holdsPointer ctx wid mouse ranges = do
  let na = ctxNodeArena ctx
      inRanges i = any (\(from, to) -> i >= from && i < to) ranges
      overControl i =
        (isWidgetNode <$> getNodeType na i) <&&> do
          w <- getWidgetId na i
          (not <$> isDisabled ctx w)
            <&&> (not <$> pointerCovered ctx w)
            <&&> (maybe (pure False) (\r -> nodeInteractionHit ctx i r mouse) =<< getPrevRect ctx w)
  active <- readIORef (ctxActiveId ctx)
  pressedControl <- maybe False inRanges <$> lookupNodeByWidgetId na active
  if pressedControl || active == wid
    then pure pressedControl
    else foldr (\i rest -> ifM (overControl i) (pure True) rest) (pure False) [i | (from, to) <- ranges, i <- [from .. to - 1]]
