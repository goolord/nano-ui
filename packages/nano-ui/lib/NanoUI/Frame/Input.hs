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
import Control.Monad (unless, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (isJust, isNothing)
import NanoUI.Context
  ( Context (..)
  , TextInputMenu (..)
  , WidgetStore (..)
  , damageWidget
  , getFocusables
  , getMenuPointerGesture
  , getStore
  , getTextInputMenu
  , intKey
  , isDisabled
  , markDirty
  , pointerBlockedByOverlay
  , setAnimationValue
  , setMenuPointerGesture
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
  , overlayHitAllowed
  , scrollHitRect
  )
import NanoUI.Frame.Redraw (probeHotId)
import NanoUI.Frame.Select (findSelectUnderMouse, overlayMenuOwnerAt)
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
  , findNodeRevM
  , foldNodesM
  , getNodeType
  , getParent
  , getRect
  , getStyleIdx
  , getWidgetId
  )
import NanoUI.Monad (whenM)
import NanoUI.Types (DamageBounds (..), Rect (..), V2 (..), defaultDamageSlop, rectContains, rectH, rectW)
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
          pure (if null ids then WidgetId 0 else tabNext cur ids shift)
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
  when (inputMousePressed inp) $ writeIORef (ctxPressPos ctx) here
  when (inputMouseRightPressed inp) $ writeIORef (ctxRightPressPos ctx) here

disarmPointerPress :: Context -> Input -> IO ()
disarmPointerPress ctx inp = do
  when (inputMouseReleased inp) $ writeIORef (ctxPressPos ctx) Nothing
  when (inputMouseRightReleased inp) $ writeIORef (ctxRightPressPos ctx) Nothing

-- Same walk as refreshHover: later nodes paint first, earlier widget hits win.
finalizePointerPress :: Context -> Input -> IO ()
finalizePointerPress ctx inp =
  when (inputMousePressed inp) $ do
    -- A pointer press hides the keyboard focus ring.
    writeIORef (ctxFocusVisible ctx) False
    gesture <- getMenuPointerGesture ctx
    if gesture
      then writeIORef (ctxActiveId ctx) (WidgetId 0)
      else do
        let mouse = inputMousePos inp
        mMenu <- overlayMenuOwnerAt ctx mouse
        case mMenu of
          Just _ -> do
            setMenuPointerGesture ctx True
            writeIORef (ctxActiveId ctx) (WidgetId 0)
          Nothing -> do
            mWid <- findTopWidgetUnderMouse ctx mouse isInteractiveNode
            case mWid of
              Nothing -> pure ()
              Just wid ->
                whenM (not <$> isDisabled ctx wid) $
                  writeIORef (ctxActiveId ctx) wid

findTopWidgetUnderMouse :: Context -> V2 -> (NodeType -> Bool) -> IO (Maybe WidgetId)
findTopWidgetUnderMouse ctx mouse wanted = do
  let na = ctxNodeArena ctx
  mIdx <-
    findNodeRevM na $ \idx -> do
      nt <- getNodeType na idx
      if not (wanted nt)
        then pure False
        else do
          (x, y, w, h) <- getRect na idx
          rect <- widgetHitRect ctx nt idx x y w h
          if rectW rect > 0 && rectH rect > 0
            then do
              hit <- nodeClippedHit ctx idx rect mouse
              if hit then overlayHitAllowed ctx idx mouse else pure False
            else pure False
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
    gesture <- getMenuPointerGesture ctx
    mMenu <- overlayMenuOwnerAt ctx mouse
    if gesture || isJust mMenu
      then do
        writeIORef (ctxActiveId ctx) (WidgetId 0)
        setMenuPointerGesture ctx False
      else do
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
                    (x, y, w, h) <- getRect na idx
                    visible <- nodeClippedHit ctx idx (Rect x y w h) mouse
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
    setStore ctx store {storeInt = IM.insert (intKey groupWid) selected (storeInt store)}

postsLayoutClick :: NodeType -> Bool
postsLayoutClick nt =
  nt == NodeButton || nt == NodeTree || nt == NodeSelect || nt == NodeCheckbox

inUiClickHit :: Context -> WidgetId -> V2 -> IO Bool
inUiClickHit ctx wid mouse = do
  disabled <- isDisabled ctx wid
  blocked <- pointerBlockedByOverlay ctx mouse
  if disabled || blocked
    then pure False
    else do
      mrect <- scrollHitRect ctx wid
      case mrect of
        Nothing -> pure False
        Just r ->
          findNodeByWidgetId ctx wid >>= \case
            Nothing -> pure (rectContains r mouse)
            Just idx -> nodeInteractionHit ctx idx r mouse

-- Focus text inputs using solved layout rects so the caret appears on first press.
-- A press on an open dropdown overlay (select menu or a focused combo's
-- suggestions) must not clear focus first: the combo's dropdown is visible
-- exactly while its field holds focus, and the select finalizers below need
-- the owner still resolvable to route the pick.
finalizeTextInputFocus :: Context -> Input -> IO ()
finalizeTextInputFocus ctx inp =
  when (inputMousePressed inp) $ do
    mMenu <- getTextInputMenu ctx
    let mouse = inputMousePos inp
    mDrop <- overlayMenuOwnerAt ctx mouse
    let onMenu = maybe False (\menu -> rectContains (textInputMenuRect menu) mouse) mMenu
    when (not onMenu && isNothing mDrop) $ do
      prevFocus <- readIORef (ctxFocusId ctx)
      mFocused <- findTextInputUnderMouse ctx mouse
      case mFocused of
        Nothing -> do
          when (prevFocus /= WidgetId 0) $ markDirty ctx
          collapseTextFieldSelection ctx prevFocus
          writeIORef (ctxFocusId ctx) (WidgetId 0)
          setTextInputMenu ctx Nothing
        Just wid -> do
          writeIORef (ctxFocusId ctx) wid
          when (prevFocus /= wid) $ markDirty ctx

finalizeSelectFocus :: Context -> Input -> IO ()
finalizeSelectFocus ctx inp =
  when (inputMousePressed inp) $ do
    let mouse = inputMousePos inp
    mOpen <- findSelectUnderMouse ctx mouse
    -- A press on a select's own field that just closed its dropdown leaves no
    -- open dropdown under the pointer, but the select keeps focus all the same.
    mWid <- maybe (findTopWidgetUnderMouse ctx mouse (== NodeSelect)) (pure . Just) mOpen
    case mWid of
      Nothing -> pure ()
      Just wid ->
        whenM (not <$> isDisabled ctx wid) $ do
          prev <- readIORef (ctxFocusId ctx)
          writeIORef (ctxFocusId ctx) wid
          when (prev /= wid) $ markDirty ctx

findTextInputUnderMouse :: Context -> V2 -> IO (Maybe WidgetId)
findTextInputUnderMouse ctx mouse = do
  let na = ctxNodeArena ctx
  mIdx <-
    findNodeM na $ \idx -> do
      nt <- getNodeType na idx
      if nt /= NodeTextInput && nt /= NodeTextArea
        then pure False
        else do
          (x, y, w, h) <- getRect na idx
          rect <- widgetHitRect ctx nt idx x y w h
          hit <- nodeClippedHit ctx idx rect mouse
          if hit then overlayHitAllowed ctx idx mouse else pure False
  mWid <- traverse (getWidgetId na) mIdx
  -- A press on a disabled field lands on nothing: it takes focus from
  -- whichever field had it and gives it to none.
  case mWid of
    Just wid -> do
      disabled <- isDisabled ctx wid
      pure (if disabled then Nothing else Just wid)
    Nothing -> pure Nothing
