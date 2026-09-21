-- | Floating panel overlays: windows, popups and modals (with the modal
-- backdrop), each a menu-style panel with its subtree painted inside.
module NanoUI.Internal.Frame.Overlay
  ( drawWindowOverlays
  , drawModalOverlays
  , drawPopupOverlays
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import NanoUI.Internal.Context (Context (..), nodeTheme)
import NanoUI.Internal.Draw (pushRect, withClip)
import NanoUI.Internal.Frame.Chrome (overlayMenuStyle, overlayModalStyle, overlayWindowStyle, paintMenuPanel)
import NanoUI.Internal.Frame.Hit (modalTreeOpen)
import NanoUI.Internal.Frame.Paint (walkChildren)
import NanoUI.Internal.Layout.Arena (NodeIdx, NodeType (..), forNodes_, getNodeRect, getNodeType, getPadding)
import NanoUI.Internal.Style (Padding (..), Style, Theme, themeOverlayDim, themeSeparator)
import NanoUI.Internal.Types (Rect (..), Size (..))
import NanoUI.Internal.Widgets.Chrome (titleBarChromeHFor, windowChromeSepH)

drawWindowOverlays :: Context -> IO ()
drawWindowOverlays ctx =
  forFloatingNode ctx NodeWindow $ \idx rect@(Rect x y w _) -> do
    theme <- nodeTheme ctx idx
    drawFloatingPanel ctx theme idx (overlayWindowStyle theme) rect
    pad <- getPadding (ctxNodeArena ctx) idx
    let sepY = y + padT pad + titleBarChromeHFor - windowChromeSepH
    pushRect
      (ctxDrawArena ctx)
      (Rect (x + padL pad) sepY (max 0 (w - padL pad - padR pad)) windowChromeSepH)
      (themeSeparator theme)

drawPopupOverlays :: Context -> IO ()
drawPopupOverlays ctx =
  forFloatingNode ctx NodePopup $ \idx rect -> do
    theme <- nodeTheme ctx idx
    drawFloatingPanel ctx theme idx (overlayMenuStyle theme) rect

drawModalOverlays :: Context -> Size -> IO ()
drawModalOverlays ctx (Size ww wh) = do
  found <- modalTreeOpen ctx
  when found $ do
    theme <- readIORef (ctxTheme ctx)
    pushRect (ctxDrawArena ctx) (Rect 0 0 ww wh) (themeOverlayDim theme)
    forFloatingNode ctx NodeModal $ \idx rect -> do
      modalTheme <- nodeTheme ctx idx
      drawFloatingPanel ctx modalTheme idx (overlayModalStyle modalTheme) rect

forFloatingNode :: Context -> NodeType -> (NodeIdx -> Rect -> IO ()) -> IO ()
forFloatingNode ctx nodeType draw =
  forNodes_ (ctxNodeArena ctx) $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    when (nt == nodeType) $ do
      rect <- getNodeRect (ctxNodeArena ctx) idx
      draw idx rect

drawFloatingPanel :: Context -> Theme -> NodeIdx -> Style -> Rect -> IO ()
drawFloatingPanel ctx theme idx style rect = do
  paintMenuPanel (ctxDrawArena ctx) theme style rect
  withClip (ctxDrawArena ctx) rect (walkChildren ctx idx)
