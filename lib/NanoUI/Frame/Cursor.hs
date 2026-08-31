{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NanoUI.Frame.Cursor
  ( UiCursorKind (..)
  , grabHoverKind
  , grabDragKind
  , uiCursorKind
  , pointerCursorWanted
  , cursorKindIs
  ) where
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Context (Context (..), WidgetStore (..), getHotId, getScrollOffset, getStore, intKey, isDisabled)
import NanoUI.Font (sliderTrackBounds)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), inputMouseDown, inputMousePos)
import NanoUI.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , getDirection
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getText
  , getWidgetId
  , isScrollNode
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Types (Rect (..), V2 (..), rectContains)
import NanoUI.WidgetText (selectParseOptions, sliderLabelText)
import NanoUI.Frame.CursorKind (UiCursorKind (..), grabDragKind, grabHoverKind)
import NanoUI.Frame.Chrome (widgetNodeTypeTable)
import NanoUI.Frame.Hit (findNodeByWidgetId, scrollHitRect)
import NanoUI.Frame.Scroll (scrollBarLayout, ScrollBarLayout (..))
import NanoUI.Frame.Select (selectDropRect)
import NanoUI.Frame.TextInput (TextInputGeom (..), textInputGeom, textInputMenuCursorKind)
import NanoUI.Frame.Window (windowResizeCursorKind)

uiCursorKind :: Context -> Input -> IO UiCursorKind
uiCursorKind ctx inp = do
  mMenu <- textInputMenuCursorKind ctx inp
  case mMenu of
    Just k -> pure k
    Nothing -> do
      let mouse = inputMousePos inp
      table <- widgetNodeTypeTable ctx
      mDrop <- selectDropdownCursorKind ctx inp
      case mDrop of
        Just k -> pure k
        Nothing -> do
          mResize <- windowResizeCursorKind ctx inp
          case mResize of
            Just k -> pure k
            Nothing -> do
              mScroll <- scrollThumbCursorKind ctx inp
              case mScroll of
                Just k -> pure k
                Nothing -> do
                  active <- readIORef (ctxActiveId ctx)
                  activeKind <- cursorKindAt table ctx active mouse inp
                  if activeKind /= UiCursorDefault
                    then pure activeKind
                    else do
                      hot <- getHotId ctx
                      cursorKindAt table ctx hot mouse inp

selectDropdownCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
selectDropdownCursorKind ctx inp = do
  let mouse = inputMousePos inp
  dropPress <- readIORef (ctxSelectDropPress ctx)
  store <- getStore ctx
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt /= NodeSelect
              then go (idx + 1)
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                let key = intKey wid
                    open = IM.findWithDefault False key (storeSelectOpen store)
                txt <- getText (ctxNodeArena ctx) idx
                let (_, opts) = selectParseOptions txt
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                let fm = ctxFontMetrics ctx
                    dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                    inDrop = rectContains dropRect mouse
                if inDrop && (open || dropPress)
                  then pure (Just UiCursorPointer)
                  else go (idx + 1)
  go 0

scrollThumbCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
scrollThumbCursorKind ctx inp = do
  mDrag <- readIORef (ctxScrollDrag ctx)
  let clicking = inputMouseDown inp
  if clicking && isJust mDrag
    then pure (Just UiCursorGrabbing)
    else do
      onThumb <- scrollThumbHit ctx (inputMousePos inp)
      if onThumb
        then pure (Just (grabHoverKind True inp))
        else pure Nothing

scrollThumbHit :: Context -> V2 -> IO Bool
scrollThumbHit ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  go 0 count
  where
    go idx count
      | idx >= count = pure False
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if not (isScrollNode nt)
            then go (idx + 1) count
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              pad <- getPadding (ctxNodeArena ctx) idx
              contentSize <- getNodeValue (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              dir <- getDirection (ctxNodeArena ctx) idx
              off <- getScrollOffset ctx wid
              let fm = ctxFontMetrics ctx
              slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
              case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
                Nothing -> go (idx + 1) count
                Just layout ->
                  if rectContains (sbThumb layout) mouse
                    then pure True
                    else go (idx + 1) count

cursorKindAt :: IM.IntMap NodeType -> Context -> WidgetId -> V2 -> Input -> IO UiCursorKind
cursorKindAt table ctx wid mouse inp
  | hashWidgetId wid == 0 = pure UiCursorDefault
  | otherwise = do
      disabled <- isDisabled ctx wid
      if disabled
        then pure UiCursorDefault
        else
          case IM.lookup (intKey wid) table of
            Just NodeButton -> pure UiCursorPointer
            Just NodeCheckbox -> pure UiCursorPointer
            Just NodeRadio -> pure UiCursorPointer
            Just NodeTree -> pure UiCursorPointer
            Just NodeSelect -> selectCursorKind ctx wid mouse
            Just NodeColorPicker -> pure UiCursorPointer
            Just NodeTextInput -> textInputCursorKind ctx wid mouse
            Just NodeSlider -> sliderCursorKind ctx wid mouse inp
            _ -> pure UiCursorDefault

selectCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
selectCursorKind ctx wid mouse = do
  mrect <- scrollHitRect ctx wid
  pure $
    case mrect of
      Nothing -> UiCursorDefault
      Just rect ->
        if rectContains rect mouse
          then UiCursorPointer
          else UiCursorDefault

sliderCursorKind :: Context -> WidgetId -> V2 -> Input -> IO UiCursorKind
sliderCursorKind ctx wid mouse inp = do
  mrect <- scrollHitRect ctx wid
  active <- readIORef (ctxActiveId ctx)
  let fm = ctxFontMetrics ctx
      dragging = active == wid && inputMouseDown inp
  lbl <-
    findNodeByWidgetId ctx wid >>= \case
      Nothing -> pure T.empty
      Just idx -> do
        txt <- getText (ctxNodeArena ctx) idx
        pure (sliderLabelText (T.takeWhile (/= '\US') txt))
  pure $
    case mrect of
      Nothing -> UiCursorDefault
      Just (Rect x y w h) ->
        grabDragKind (rectContains (sliderTrackBounds (ctxHostProfile ctx) fm lbl x y w h) mouse) dragging inp

textInputCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
textInputCursorKind ctx wid mouse = do
  mrect <- scrollHitRect ctx wid
  case mrect of
    Nothing -> pure UiCursorDefault
    Just (Rect x y w h) -> do
      let fm = ctxFontMetrics ctx
          field = tigFieldRect (textInputGeom (ctxHostProfile ctx) fm x y w h)
      pure $
        if rectContains field mouse
          then UiCursorText
          else UiCursorDefault

pointerCursorWanted :: Context -> Input -> IO Bool
pointerCursorWanted ctx inp = cursorKindIs ctx inp UiCursorPointer

cursorKindIs :: Context -> Input -> UiCursorKind -> IO Bool
cursorKindIs ctx inp want = (== want) <$> uiCursorKind ctx inp

