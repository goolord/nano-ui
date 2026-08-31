{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NanoUI.Frame.Input
  ( finalizeTabFocus
  , refreshHover
  , finalizePointerPress
  , finalizePointerRelease
  , finalizeTextInputFocus
  , finalizeSelectFocus
  , finalizeTextInputMouse
  , findTopWidgetUnderMouse
  , isInteractiveNode
  , findTextInputUnderMouse
  ) where


import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context
  ( Context (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WidgetStore (..)
  , boolInt
  , getFocusables
  , getStore
  , intBool
  , intKey
  , isDisabled
  , markDirty
  , setAnimationValue
  , setStore
  , startAnimation
  )
import NanoUI.Host (isCellHost)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , inputKeysElem
  , inputMouseClicks
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputModifiers
  , modShift
  )
import NanoUI.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , findNodeRevM
  , getNodeType
  , getRect
  , getText
  , getWidgetId
  )
import NanoUI.Types (Rect (..), V2 (..), rectContains, rectH, rectW, v2X)
import NanoUI.WidgetText (radioParseOption)
import NanoUI.Frame.Focus (filterModalFocusables, tabNext, tabNextFocusables)
import NanoUI.Frame.Hit (modalTreeOpen, overlayHitAllowed)
import NanoUI.Frame.Redraw (probeHotId)
import NanoUI.Frame.Select (findSelectUnderMouse)
import NanoUI.Frame.Spans (widgetHitRect)
import NanoUI.Frame.TextInput (collapseTextInputSelection, textInputGeomForWidget, applyTextInputClick, applyTextInputDrag, textInputCharAtX)

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = mb >>= \b -> when b ma

finalizeTabFocus :: Context -> Input -> IO ()
finalizeTabFocus ctx inp =
  when (inputKeysElem KeyTab (inputKeys inp)) $ do
    open <- modalTreeOpen ctx
    if not open
      then do
        cur <- readIORef (ctxFocusId ctx)
        next <- tabNextFocusables ctx cur (modShift (inputModifiers inp))
        when (hashWidgetId next /= 0) $ do
          writeIORef (ctxFocusId ctx) next
          markDirty ctx
      else do
        focusables <- getFocusables ctx
        let raw = filter (/= WidgetId 0) focusables
        ids <- filterModalFocusables ctx raw
        if null ids
          then pure ()
          else do
            cur <- readIORef (ctxFocusId ctx)
            let shift = modShift (inputModifiers inp)
                next = tabNext cur ids shift
            writeIORef (ctxFocusId ctx) next
            markDirty ctx


refreshHover :: Context -> Input -> IO ()
refreshHover ctx inp = do
  prevHot <- readIORef (ctxLastHotId ctx)
  newHot <- probeHotId ctx (inputMousePos inp)
  writeIORef (ctxHotId ctx) newHot
  writeIORef (ctxLastHotId ctx) newHot
  let terminal = isCellHost (ctxHostProfile ctx)
  when (prevHot /= newHot) $ do
    unless terminal $ do
      when (hashWidgetId prevHot /= 0) $ startAnimation ctx prevHot 1 0 0.12
      when (hashWidgetId newHot /= 0) $ startAnimation ctx newHot 0 1 0.12

-- Same walk as refreshHover: later nodes paint first, earlier widget hits win.
finalizePointerPress :: Context -> Input -> IO ()
finalizePointerPress ctx inp =
  when (inputMousePressed inp) $ do
    let mouse = inputMousePos inp
    mWid <- findTopWidgetUnderMouse ctx mouse isInteractiveNode
    case mWid of
      Nothing -> pure ()
      Just wid ->
        whenM (not <$> isDisabled ctx wid) $
          writeIORef (ctxActiveId ctx) wid

findTopWidgetUnderMouse ::
  Context -> V2 -> (NodeType -> Bool) -> IO (Maybe WidgetId)
findTopWidgetUnderMouse ctx mouse wanted = do
  mIdx <-
    findNodeRevM (ctxNodeArena ctx) $ \idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      if not (wanted nt)
        then pure False
        else do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
          rect <- widgetHitRect ctx nt idx x y w h
          if rectW rect > 0 && rectH rect > 0 && rectContains rect mouse
            then overlayHitAllowed ctx idx mouse
            else pure False
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> Just <$> getWidgetId (ctxNodeArena ctx) idx

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

-- Clicks are finalized against solved layout rects; widgets only track press state.
finalizePointerRelease :: Context -> Input -> IO ()
finalizePointerRelease ctx inp =
  if not (inputMouseReleased inp)
    then pure ()
    else do
      active <- readIORef (ctxActiveId ctx)
      when (hashWidgetId active /= 0) $ do
        let mouse = inputMousePos inp
        count <- arenaCount (ctxNodeArena ctx)
        releasedOver <-
          if count <= 0
            then pure False
            else checkReleasedOver ctx count active mouse
        forM_ [0 .. count - 1] $ \idx -> do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          when (wid == active) $ do
            nt <- getNodeType (ctxNodeArena ctx) idx
            (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
            let rect = Rect x y w h
            when (w > 0 && h > 0 && rectContains rect mouse) $
              case nt of
                NodeCheckbox -> do
                  store <- getStore ctx
                  let key = intKey wid
                      current =
                        intBool (IM.findWithDefault 0 key (storeInt store))
                      newVal = not current
                  setStore
                    ctx
                    ( store
                        { storeInt = IM.insert key (boolInt newVal) (storeInt store)
                        }
                    )
                NodeRadio -> do
                  store <- getStore ctx
                  txt <- getText (ctxNodeArena ctx) idx
                  let (groupKey, optIdx, _) = radioParseOption txt
                  setStore
                    ctx
                    ( store
                        { storeInt = IM.insert groupKey optIdx (storeInt store)
                        }
                    )
                _ -> pure ()
        writeIORef (ctxActiveId ctx) (WidgetId 0)
        when releasedOver $
          unless (isCellHost (ctxHostProfile ctx)) $
            setAnimationValue ctx active 1

checkReleasedOver :: Context -> Int -> WidgetId -> V2 -> IO Bool
checkReleasedOver ctx count active mouse = go 0
  where
    go idx
      | idx >= count = pure False
      | otherwise = do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          if wid /= active
            then go (idx + 1)
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              let rect = Rect x y w h
              pure (w > 0 && h > 0 && rectContains rect mouse)

-- Focus text inputs using solved layout rects so the caret appears on first press.
finalizeTextInputFocus :: Context -> Input -> IO ()
finalizeTextInputFocus ctx inp =
  when (inputMousePressed inp) $ do
    mMenu <- readIORef (ctxTextInputMenu ctx)
    let mouse = inputMousePos inp
    when (case mMenu of
            Just menu -> not (rectContains (textInputMenuRect menu) mouse)
            Nothing -> True) $ do
      prevFocus <- readIORef (ctxFocusId ctx)
      count <- arenaCount (ctxNodeArena ctx)
      mFocused <- findTextInputUnderMouse ctx count mouse
      case mFocused of
        Nothing -> do
          when (prevFocus /= WidgetId 0) $ markDirty ctx
          collapseTextInputSelection ctx prevFocus
          writeIORef (ctxFocusId ctx) (WidgetId 0)
          writeIORef (ctxTextInputMenu ctx) Nothing
        Just wid -> do
          writeIORef (ctxFocusId ctx) wid
          when (prevFocus /= wid) $ markDirty ctx

finalizeSelectFocus :: Context -> Input -> IO ()
finalizeSelectFocus ctx inp =
  when (inputMousePressed inp) $ do
    mWid <- findSelectUnderMouse ctx (inputMousePos inp)
    case mWid of
      Nothing -> pure ()
      Just wid ->
        whenM (not <$> isDisabled ctx wid) $ do
          prev <- readIORef (ctxFocusId ctx)
          writeIORef (ctxFocusId ctx) wid
          when (prev /= wid) $ markDirty ctx

finalizeTextInputMouse :: Context -> Input -> IO ()
finalizeTextInputMouse ctx inp = do
  focus <- readIORef (ctxFocusId ctx)
  when (hashWidgetId focus /= 0) $ do
    mGeom <- textInputGeomForWidget ctx focus
    case mGeom of
      Nothing -> pure ()
      Just (fieldRect, contentX, value) -> do
        let mouse = inputMousePos inp
            inField = rectContains fieldRect mouse
        if inputMousePressed inp && inField
          then do
            idx <- textInputCharAtX ctx value contentX (v2X mouse)
            let clicks = max 1 (inputMouseClicks inp)
            applyTextInputClick ctx focus value idx clicks
            writeIORef (ctxTextInputDrag ctx) (Just (TextInputDrag focus idx clicks))
          else do
            mDrag <- readIORef (ctxTextInputDrag ctx)
            case mDrag of
              Just drag
                | textInputDragWidget drag == focus && (inputMouseDown inp || inputMouseReleased inp) -> do
                    idx <- textInputCharAtX ctx value contentX (v2X mouse)
                    applyTextInputDrag ctx focus value (textInputDragAnchor drag) idx (textInputDragClicks drag)
              _ -> pure ()
  when (inputMouseReleased inp) $
    writeIORef (ctxTextInputDrag ctx) Nothing

findTextInputUnderMouse :: Context -> Int -> V2 -> IO (Maybe WidgetId)
findTextInputUnderMouse ctx count mouse = go 0
  where
    go idx
      | idx >= count = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt == NodeTextInput
            then do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              rect <- widgetHitRect ctx nt idx x y w h
              if rectW rect > 0 && rectH rect > 0 && rectContains rect mouse
                then do
                  allow <- overlayHitAllowed ctx idx mouse
                  if allow then pure (Just wid) else go (idx + 1)
                else go (idx + 1)
            else go (idx + 1)

