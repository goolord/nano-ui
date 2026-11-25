-- | Selectable list rows and collapsible disclosure sections built on core
-- containers and canvas primitives.
module NanoUI.Internal.Widgets.Disclosure
  ( selectableRow
  , selectableRow'
  , selectableRowWith
  , selectableRowWith'
  , collapsing
  , collapsingWith
  ) where

import Control.Monad (when)
import Data.Text (Text)
import NanoUI.Internal.Context (registerHoverZone)
import NanoUI.Internal.Input (UiCursorKind (..))
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, askContext, askDefaultLayout, askInput, liftIO, nextId, scope, styled, themed, uiTheme)
import NanoUI.Internal.Style
import NanoUI.Internal.Style qualified as Style
import NanoUI.Internal.Types (Rect (..), V2 (..), colorTransparent, lerpColor, rectNonEmpty)
import NanoUI.Internal.Var (readVar, writeVar)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Widgets.Cursor (withCursorShape)
import NanoUI.Internal.Widgets.Layout (columnWith, labelWith)
import NanoUI.Internal.Widgets.Node (Response, container, resolveInteraction, respClicked, respHovered, respPressed, respRect, tagContainer)
import NanoUI.Path qualified as P
import NanoUI.Widgets.Custom (CanvasConfig (..), CustomDrawContext (..), canvasConfigured, contentKey, defaultCanvasConfig, drawContext, drawPath)

-- | A full-width clickable row that highlights on hover and when @selected@.
-- Returns 'True' on click (presses on interactive children inside the row
-- belong to those children).
{-# INLINE selectableRow #-}
selectableRow :: Bool -> NanoUI () -> NanoUI Bool
selectableRow selected = selectableRowWith selected id

-- | 'selectableRow' returning both the body's result and the row's 'Response'.
{-# INLINE selectableRow' #-}
selectableRow' :: Bool -> NanoUI a -> NanoUI (a, Response)
selectableRow' selected = selectableRowWith' selected id

-- | 'selectableRow' with a layout modifier applied over the default horizontal
-- row layout (@fillW . direction Row . padXY 8 5 . gap 8 . itemsCenter@).
{-# INLINE selectableRowWith #-}
selectableRowWith :: Bool -> LayoutModifier -> NanoUI () -> NanoUI Bool
selectableRowWith selected f body = respClicked . snd <$> selectableRowWith' selected f body

-- | 'selectableRowWith' returning both the body's result and the row's 'Response'.
selectableRowWith' :: Bool -> LayoutModifier -> NanoUI a -> NanoUI (a, Response)
selectableRowWith' selected f body = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  base <- askDefaultLayout
  outerTheme <- uiTheme
  resp <- liftIO (resolveInteraction ctx inp (-1) wid)
  when (rectNonEmpty (respRect resp)) $
    liftIO (registerHoverZone ctx False (respRect resp))
  let lay = f (itemsCenter (gap 8 (padXY 8 5 (fillW (direction Row base)))))
      rowTheme t =
        let panelBg = Style.styleBg (Style.themePanel t)
            panelFg = Style.styleFg (Style.themePanel t)
            rad = Style.styleCornerRadius (Style.themeButton t)
            bg
              | selected && respHovered resp = lerpColor (Style.themeAccent t) panelBg 0.72
              | selected = lerpColor (Style.themeAccent t) panelBg 0.80
              | respPressed resp = lerpColor panelFg panelBg 0.86
              | respHovered resp = lerpColor panelFg panelBg 0.92
              | otherwise = colorTransparent
         in panelStyle (Style.background bg . Style.borderWidth 0 . Style.cornerRadius rad) t
  a <- withCursorShape UiCursorPointer $
    styled rowTheme $
      container NodePanel lay (tagContainer wid >> themed outerTheme body)
  pure (a, resp)

-- | A titled section whose body expands and collapses when its header is
-- clicked, backed by a 'Var Bool'. The body runs inside 'scope' so toggling
-- it never shifts sibling widget ids.
{-# INLINE collapsing #-}
collapsing :: Text -> Var Bool -> NanoUI () -> NanoUI ()
collapsing = collapsingWith id

-- | 'collapsing' with a layout modifier on the outer section column.
collapsingWith :: LayoutModifier -> Text -> Var Bool -> NanoUI () -> NanoUI ()
collapsingWith f title openVar body = do
  open <- readVar openVar
  columnWith (f . tight . gap 4 . fillW) $ do
    clicked <- selectableRowWith False (padXY 6 4 . gap 6 . itemsCenter) $ do
      _ <- canvasConfigured (chevronCfg open) $ \(Rect x y w h) -> do
        cdc <- drawContext
        let col = Style.themeMuted (cdcTheme cdc)
            cx = x + w * 0.5
            cy = y + h * 0.5
            r = min w h * 0.28
            pts
              | open =
                  [ V2 (cx - r) (cy - r * 0.65)
                  , V2 (cx + r) (cy - r * 0.65)
                  , V2 cx (cy + r * 0.85)
                  ]
              | otherwise =
                  [ V2 (cx - r * 0.65) (cy - r)
                  , V2 (cx + r * 0.85) cy
                  , V2 (cx - r * 0.65) (cy + r)
                  ]
        drawPath (P.polygon pts) col
      labelWith fontSemiBold title
    when clicked (writeVar openVar (not open))
    scope $ when open $
      columnWith (tight . padLeft 16 . gap 4 . fillW) body
  where
    chevronCfg isOpen =
      defaultCanvasConfig
        { canvasLayout = fixedWH 12 12 . pointer PointerPass
        , canvasContent = contentKey [if isOpen then 1 else 0]
        }
