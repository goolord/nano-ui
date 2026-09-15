-- | Floating panel overlays: windows, popups and modals (with the modal
-- backdrop), each a menu-style panel with its subtree painted inside.
module NanoUI.Frame.Overlay
  ( drawWindowOverlays
  , drawModalOverlays
  , drawPopupOverlays
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import NanoUI.Context (Context (..))
import NanoUI.Draw (pushRect, withClip)
import NanoUI.Frame.Chrome (overlayMenuStyle, overlayModalStyle, overlayWindowStyle, paintMenuPanel)
import NanoUI.Frame.Hit (modalTreeOpen)
import NanoUI.Frame.Paint (walkChildren)
import NanoUI.Layout.Arena (NodeIdx, NodeType (..), forNodes_, getNodeType, getPadding, getRect)
import NanoUI.Style (Padding (..), Style, themeOverlayDim, themeSeparator)
import NanoUI.Types (Rect (..), Size (..))
import NanoUI.Widgets.Chrome (titleBarChromeHFor, windowChromeSepH)

drawWindowOverlays :: Context -> IO ()
drawWindowOverlays ctx = do
  theme <- readIORef (ctxTheme ctx)
  forFloatingNode ctx NodeWindow $ \idx rect@(Rect x y w _) -> do
    drawFloatingPanel ctx idx (overlayWindowStyle theme) rect
    pad <- getPadding (ctxNodeArena ctx) idx
    let sepY = y + padT pad + titleBarChromeHFor - windowChromeSepH
    pushRect
      (ctxDrawArena ctx)
      (Rect (x + padL pad) sepY (max 0 (w - padL pad - padR pad)) windowChromeSepH)
      (themeSeparator theme)

drawPopupOverlays :: Context -> IO ()
drawPopupOverlays ctx = do
  theme <- readIORef (ctxTheme ctx)
  forFloatingNode ctx NodePopup $ \idx rect ->
    drawFloatingPanel ctx idx (overlayMenuStyle theme) rect

drawModalOverlays :: Context -> Size -> IO ()
drawModalOverlays ctx (Size ww wh) = do
  found <- modalTreeOpen ctx
  when found $ do
    theme <- readIORef (ctxTheme ctx)
    pushRect (ctxDrawArena ctx) (Rect 0 0 ww wh) (themeOverlayDim theme)
    forFloatingNode ctx NodeModal $ \idx rect ->
      drawFloatingPanel ctx idx (overlayModalStyle theme) rect

forFloatingNode :: Context -> NodeType -> (NodeIdx -> Rect -> IO ()) -> IO ()
forFloatingNode ctx nodeType draw =
  forNodes_ (ctxNodeArena ctx) $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    when (nt == nodeType) $ do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      draw idx (Rect x y w h)

drawFloatingPanel :: Context -> NodeIdx -> Style -> Rect -> IO ()
drawFloatingPanel ctx idx style rect = do
  paintMenuPanel (ctxDrawArena ctx) style rect
  withClip (ctxDrawArena ctx) rect (walkChildren ctx idx)
