{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Input
  ( finalizeTabFocus
  , refreshHover
  , armPointerPress
  , disarmPointerPress
  , finalizePointerPress
  , finalizePointerRelease
  , finalizeTextInputFocus
  , finalizeSelectFocus
  , findTopWidgetUnderMouse
  , isInteractiveNode
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import NanoUI.Context
  ( Context (..)
  , damageWidget
  , getFocusables
  , getStore
  , intKey
  , isDisabled
  , markDirty
  , setAnimationValue
  , setStore
  , setTextInputMenu
  , startAnimation
  )
import NanoUI.Frame.Focus (filterModalFocusables, tabNext, tabNextFocusables)
import NanoUI.Frame.Hit
  ( findNodeByWidgetId
  , modalTreeOpen
  , nodeClippedHit
  , nodeInteractionHit
  , nodeOwnsPointer
  , overlayHitAllowed
  , scrollHitRect
  )
import NanoUI.Frame.Redraw (probeHotId)
import NanoUI.Frame.Spans (widgetHitRect)
import NanoUI.Frame.TextEdit (collapseTextFieldSelection)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , inputKeysElem
  , inputModifiers
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightPressed
  , inputMouseRightReleased
  , modShift
  )
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , findNodeM
  , foldNodesM
  , getNodeRect
  , getNodeType
  , getParent
  , getRect
  , getStyleIdx
  , getWidgetId
  )
import NanoUI.Monad (whenM, (<&&>))
import NanoUI.Store (fieldInt, insertSlot)
import NanoUI.Types (DamageBounds (..), V2 (..), defaultDamageSlop, rectContains)
import NanoUI.WidgetText (buttonVisualStyle, isMenuBarStyle, isMenuItemStyle, isTabButtonStyle)

finalizeTabFocus :: Context -> Input -> IO ()
finalizeTabFocus ctx inp =
  when (inputKeysElem KeyTab (inputKeys inp)) $ do
    open <- modalTreeOpen ctx
    let shift = modShift (inputModifiers inp)
    cur <- readIORef (ctxFocusId ctx)
    next <-
      if not open
        then tabNextFocusables ctx cur shift
        else do
          focusables <- getFocusables ctx
          ids <- filterModalFocusables ctx (filter (/= WidgetId 0) focusables)
          pure (tabNext cur ids shift)
    when (hashWidgetId next /= 0) $ do
      -- Keyboard focus shows its ring until the next pointer press. Focus that
      -- stays put (a lone focusable) changes no focus rect, so damage it here.
      wasVisible <- readIORef (ctxFocusVisible ctx)
      when (next == cur && not wasVisible) $
        damageWidget ctx next (DamageInflated defaultDamageSlop)
      writeIORef (ctxFocusId ctx) next
      writeIORef (ctxFocusVisible ctx) True
      markDirty ctx

-- Flat menu buttons never animate: their hover highlight snaps on and off.
isMenuButtonWidget :: Context -> WidgetId -> IO Bool
isMenuButtonWidget ctx wid
  | hashWidgetId wid == 0 = pure False
  | otherwise =
      findNodeByWidgetId ctx wid >>= \case
        Nothing -> pure False
        Just idx -> do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeButton
            then pure False
            else do
              si <- getStyleIdx (ctxNodeArena ctx) idx
              pure (isMenuItemStyle si || isMenuBarStyle si)

refreshHover :: Context -> Input -> IO ()
refreshHover ctx inp = do
  prevHot <- readIORef (ctxLastHotId ctx)
  newHot <- probeHotId ctx (inputMousePos inp)
  writeIORef (ctxHotId ctx) newHot
  writeIORef (ctxLastHotId ctx) newHot
  when (prevHot /= newHot) $ do
    prevMenu <- isMenuButtonWidget ctx prevHot
    newMenu <- isMenuButtonWidget ctx newHot
    when (hashWidgetId prevHot /= 0 && not prevMenu) $ startAnimation ctx prevHot 1 0 0.12
    when (hashWidgetId newHot /= 0 && not newMenu) $ startAnimation ctx newHot 0 1 0.12

-- | Remember where a press landed, before the UI builds: widgets resolve their
-- click against this point, so a release that drifted onto a neighbour fires
-- nowhere. Runs every frame; 'disarmPointerPress' clears it once the button
-- comes up and the frame has consumed the release.
armPointerPress :: Context -> Input -> IO ()
armPointerPress ctx inp = do
  let here = Just (inputMousePos inp)
  when (inputMousePressed inp) $ do
    writeIORef (ctxPressPos ctx) here
    -- A pointer press hides the keyboard focus ring.
    writeIORef (ctxFocusVisible ctx) False
  when (inputMouseRightPressed inp) $ writeIORef (ctxRightPressPos ctx) here

disarmPointerPress :: Context -> Input -> IO ()
disarmPointerPress ctx inp = do
  when (inputMouseReleased inp) $ writeIORef (ctxPressPos ctx) Nothing
  when (inputMouseRightReleased inp) $ writeIORef (ctxRightPressPos ctx) Nothing

-- Same walk as refreshHover: later nodes paint first, earlier widget hits win.
finalizePointerPress :: Context -> Input -> IO ()
finalizePointerPress ctx inp =
  when (inputMousePressed inp) $ do
    mWid <- findTopWidgetUnderMouse ctx (inputMousePos inp) isInteractiveNode
    forM_ mWid $ \wid ->
      whenM (not <$> isDisabled ctx wid) $
        writeIORef (ctxActiveId ctx) wid

-- | The widget of a wanted type under @mouse@ that hover would pick: the
-- first in arena order, since earlier siblings paint over later ones.
findTopWidgetUnderMouse :: Context -> V2 -> (NodeType -> Bool) -> IO (Maybe WidgetId)
findTopWidgetUnderMouse ctx mouse wanted = do
  let na = ctxNodeArena ctx
  mIdx <-
    findNodeM na $ \idx -> do
      nt <- getNodeType na idx
      pure (wanted nt) <&&> do
        (x, y, w, h) <- getRect na idx
        rect <- widgetHitRect ctx nt idx x y w h
        nodeClippedHit ctx idx rect mouse <&&> overlayHitAllowed ctx idx mouse
  traverse (getWidgetId na) mIdx

isInteractiveNode :: NodeType -> Bool
isInteractiveNode nt =
  nt == NodeButton
    || nt == NodeCheckbox
    || nt == NodeRadio
    || nt == NodeTree
    || nt == NodeSlider
    || nt == NodeSelect
    || nt == NodeColorPicker
    || nt == NodeTextInput
    || nt == NodeTextArea
    || nt == NodeDrawing

-- Clicks are finalized against solved layout rects; widgets only track press state.
-- Radio/tab selection is written here. Clickable widgets use the same solved
-- hit; if in-UI prev-rect tests missed, ctxClickedId fires next frame.
finalizePointerRelease :: Context -> Input -> IO ()
finalizePointerRelease ctx inp =
  when (inputMouseReleased inp) $ do
    let mouse = inputMousePos inp
        na = ctxNodeArena ctx
    active <- readIORef (ctxActiveId ctx)
    when (hashWidgetId active /= 0) $ do
      releasedClicked <- readIORef (ctxReleaseClickedId ctx)
      -- Every node carrying the active id takes the release; the first
      -- one decides whether the pointer came up over the widget.
      let release over idx = do
            wid <- getWidgetId na idx
            if wid /= active
              then pure over
              else do
                nt <- getNodeType na idx
                rect <- getNodeRect na idx
                visible <- nodeClippedHit ctx idx rect mouse
                when visible $ do
                  case nt of
                    NodeRadio -> getStyleIdx na idx >>= setParentSelection ctx idx
                    NodeButton -> do
                      packed <- getStyleIdx na idx
                      when (isTabButtonStyle packed) $
                        setParentSelection ctx idx (buttonVisualStyle packed `div` 4)
                    _ -> pure ()
                  when (postsLayoutClick nt && releasedClicked /= active) $ do
                    uiHit <- inUiClickHit ctx active mouse
                    unless uiHit $ writeIORef (ctxClickedId ctx) active
                pure (over <|> Just visible)
      releasedOver <- foldNodesM na release Nothing
      writeIORef (ctxActiveId ctx) (WidgetId 0)
      when (releasedOver == Just True) $
        setAnimationValue ctx active 1

-- Radio options and tab buttons keep their selection on the parent group.
setParentSelection :: Context -> NodeIdx -> Int -> IO ()
setParentSelection ctx idx selected = do
  parent <- getParent (ctxNodeArena ctx) idx
  when (parent >= 0) $ do
    store <- getStore ctx
    groupWid <- getWidgetId (ctxNodeArena ctx) parent
    setStore ctx (insertSlot fieldInt (intKey groupWid) selected store)

postsLayoutClick :: NodeType -> Bool
postsLayoutClick nt =
  nt == NodeButton || nt == NodeTree || nt == NodeSelect || nt == NodeCheckbox

inUiClickHit :: Context -> WidgetId -> V2 -> IO Bool
inUiClickHit ctx wid mouse = do
  disabled <- isDisabled ctx wid
  mrect <- scrollHitRect ctx wid
  case mrect of
    Just r | not disabled ->
      findNodeByWidgetId ctx wid >>= \case
        Nothing -> pure (rectContains r mouse)
        -- The view saw the release only if the pointer was routed to it.
        Just idx -> do
          owns <- nodeOwnsPointer ctx idx
          if owns then nodeInteractionHit ctx idx r mouse else pure False
    _ -> pure False

-- Focus text inputs using solved layout rects so the caret appears on first press.
-- A press on the text-edit menu or an open dropdown never gets here, so it
-- leaves focus alone: a combo's dropdown is visible exactly while its field
-- holds focus, and the pick still has to find its owner.
finalizeTextInputFocus :: Context -> Input -> IO ()
finalizeTextInputFocus ctx inp =
  when (inputMousePressed inp) $ do
    prevFocus <- readIORef (ctxFocusId ctx)
    mFocused <- findTextInputUnderMouse ctx (inputMousePos inp)
    case mFocused of
      Nothing -> do
        when (prevFocus /= WidgetId 0) $ markDirty ctx
        collapseTextFieldSelection ctx prevFocus
        writeIORef (ctxFocusId ctx) (WidgetId 0)
        setTextInputMenu ctx Nothing
      Just wid -> do
        writeIORef (ctxFocusId ctx) wid
        when (prevFocus /= wid) $ markDirty ctx

-- | A press on a select's field focuses it, whether that opens or closes it.
finalizeSelectFocus :: Context -> Input -> IO ()
finalizeSelectFocus ctx inp =
  when (inputMousePressed inp) $ do
    mWid <- findTopWidgetUnderMouse ctx (inputMousePos inp) (== NodeSelect)
    forM_ mWid $ \wid ->
      whenM (not <$> isDisabled ctx wid) $ do
        prev <- readIORef (ctxFocusId ctx)
        writeIORef (ctxFocusId ctx) wid
        when (prev /= wid) $ markDirty ctx

findTextInputUnderMouse :: Context -> V2 -> IO (Maybe WidgetId)
findTextInputUnderMouse ctx mouse = do
  mWid <- findTopWidgetUnderMouse ctx mouse (\nt -> nt == NodeTextInput || nt == NodeTextArea)
  -- A press on a disabled field lands on nothing: it takes focus from
  -- whichever field had it and gives it to none.
  case mWid of
    Just wid -> do
      disabled <- isDisabled ctx wid
      pure (if disabled then Nothing else Just wid)
    Nothing -> pure Nothing
