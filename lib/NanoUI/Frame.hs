module NanoUI.Frame
  ( runFrame
  , needsRedraw
  ) where

import Control.Monad (forM_, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , anyAnimating
  , drainMessages
  , FrameMsg
  , isDirty
  , setPrevRect
  , tickAnimations
  )
import NanoUI.Draw
  ( DrawData
  , Layer (..)
  , beginLayer
  , finishDraw
  , pushRect
  , pushText
  , resetDrawArena
  )
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), inputChanged)
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , arenaCount
  , getFirstChild
  , getNextSibling
  , getNodeType
  , getRect
  , getText
  , getWidgetId
  , resetNodeArena
  )
import NanoUI.Layout.Solve (solveLayout)
import NanoUI.Monad (UI (..))
import NanoUI.Style (Style (..), themeButton, themePanel, themeSeparator)
import NanoUI.Types (Rect (..), Size (..))

runFrame :: Context -> Input -> UI a -> IO (a, [FrameMsg], DrawData)
runFrame ctx inp ui = do
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxHotId ctx) (WidgetId 0)
  result <- unUI ui ctx inp
  let Size w h = inputWindowSize inp
  solveLayout (ctxNodeArena ctx) (ctxFontMetrics ctx) w h
  beginLayer (ctxDrawArena ctx) LayerBackground
  lowerShapes ctx
  drawData <- finishDraw (ctxDrawArena ctx)
  updatePrevRects ctx
  msgs <- drainMessages ctx
  tickAnimations ctx (inputDeltaTime inp)
  writeIORef (ctxDirty ctx) False
  pure (result, msgs, drawData)

needsRedraw :: Context -> Input -> Input -> IO Bool
needsRedraw ctx prev inp = do
  dirty <- isDirty ctx
  anim <- anyAnimating ctx
  pure (dirty || anim || inputChanged prev inp)

lowerShapes :: Context -> IO ()
lowerShapes ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  forM_ [0 .. count - 1] $ lowerNode ctx

lowerNode :: Context -> NodeIdx -> IO ()
lowerNode ctx idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let rect = Rect x y w h
      fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
  case nt of
    NodeContainer -> do
      let style = themePanel theme
      pushRect (ctxDrawArena ctx) rect (styleBg style)
      walkChildren ctx idx
    NodeText -> do
      txt <- getText (ctxNodeArena ctx) idx
      let style = themePanel theme
      pushRect (ctxDrawArena ctx) rect (styleBg style)
      pushText (ctxDrawArena ctx) fm (x + 2) (y + 2) txt (styleFg style)
    NodeWidget -> do
      txt <- getText (ctxNodeArena ctx) idx
      wid <- getWidgetId (ctxNodeArena ctx) idx
      hot <- readIORef (ctxHotId ctx)
      active <- readIORef (ctxActiveId ctx)
      let style = themeButton theme
          widKey = hashWidgetId wid
          bg =
            if widKey == hashWidgetId active
              then styleActiveBg style
              else if widKey == hashWidgetId hot
                then styleHoverBg style
                else styleBg style
      pushRect (ctxDrawArena ctx) rect bg
      when (not (T.null txt)) $
        pushText (ctxDrawArena ctx) fm (x + 4) (y + 2) txt (styleFg style)
    NodeSeparator ->
      pushRect (ctxDrawArena ctx) rect (themeSeparator theme)
    NodeSpacer -> pure ()

walkChildren :: Context -> NodeIdx -> IO ()
walkChildren ctx idx = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc
  where
    go ci =
      if ci < 0
        then pure ()
        else do
          lowerNode ctx ci
          ns <- getNextSibling (ctxNodeArena ctx) ci
          go ns

updatePrevRects :: Context -> IO ()
updatePrevRects ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  forM_ [0 .. count - 1] $ \idx -> do
    wid <- getWidgetId (ctxNodeArena ctx) idx
    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
    when (hashWidgetId wid /= 0) $
      setPrevRect ctx wid (Rect x y w h)
