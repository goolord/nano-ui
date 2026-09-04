{-# LANGUAGE StrictData #-}

module NanoUI.Context
  ( Context (..)
  , MeasureCacheKey
  , TextInputMenu (..)
  , TextInputDrag (..)
  , TextFieldClickCell (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , DamageState (..)
  , OverlayState (..)
  , AnimationState (..)
  , DrawingCacheState (..)
  , DrawFitCache (..)
  , intKey
  , markDirty
  , modifyIORefList
  , clearDirty
  , isDirty
  , setWakeLoop
  , takeDamage
  , setDamage
  , getLastWindowSize
  , setDamageAndWindowSize
  , DamageRequest (..)
  , requestDamage
  , damageWidget
  , damageKey
  , damageRect
  , damagePeers
  , damageFull
  , clearDamageRequests
  , getDamageRequests
  , registerPopupConfig
  , lookupPopupConfig
  , clearPopupConfigs
  , registerDrawing
  , lookupDrawing
  , cachedDrawingOps
  , cachedWidgetLayout
  , pruneDrawOpCache
  , clearDrawings
  , getWidgetNodeTypes
  , setWidgetNodeTypes
  , resetDrawingScopeCache
  , getStore
  , setStore
  , isDisabled
  , setDisabled
  , getScrollOffset
  , setScrollOffset
  , getScrollOffset2D
  , setScrollOffset2D
  , getScrollConfig
  , setScrollConfig
  , getScrollContentExtent
  , setScrollContentExtent
  , linkScrollAxes
  , getPrevRectByKey
  , getPrevRect
  , getPrevClipRectByKey
  , getPrevClipRect
  , setPrevRect
  , getPrevRects
  , getPrevClips
  , setPrevRectsAndClips
  , atlasTextureId
  , registerImage
  , registerImages
  , lookupImageUv
  , atlasSnapshot
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , wrapMeasureCache
  , clearMeasureCache
  , withExternalText
  , withTheme
  , withIcons
  , withHostProfile
  , withClipboard
  , enableMeasureCache
  , setHost
  , askHostIO
  , pushMessage
  , drainMessages
  -- Constructors
  , newContext
  , newPixelHostContext
  -- Focus
  , getFocusId
  , getHotId
  , registerFocusable
  , getFocusables
  , getFocusablesPrim
  -- Modal & Overlay
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , resetEscapeConsumed
  , pointerBlockedByModal
  , pointerBlockedByOverlay
  , menuPointerGestureActive
  , armMenuPointerCapture
  , seedFloatingPanel
  , beginModal
  , endModal
  , beginFrameModal
  , modalDamageFlip
  , getCurrentFloatingId
  , setCurrentFloatingId
  , getLastPointerBlocked
  , getPrevFloatingRects
  , setPrevFloatingPanels
  , getFloatingAncestor
  , setFloatingAncestor
  -- Animation
  , anyAnimating
  , getLiveAnimations
  , takeAnimSettled
  , lookupAnimation
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  , FrameMsg (..)
  , decodeMessages
  , reduceMessages
  , reduceUpdates
  , WidgetStore (..)
  , bumpMirror
  , slotKey
  , slotDisabled
  , slotCursor
  , slotAnchor
  , slotDrag
  , slotDragW
  , slotWinSize
  , boolInt
  , intBool
  , anySelectOpen
  , isSelectOpen
  , setSelectOpen
  , closeSelects
  , Ease (..)
  , Animation (..)
  , SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  , applyEase
  , easeSameSpec
  , approxEq
  , animInProgress
  ) where

import Control.Monad (forM, forM_, when)
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , copyMutablePrimArray
  , freezePrimArray
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (TypeRep, Typeable, cast, typeOf, typeRep)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Exts (RealWorld)

import NanoUI.Animation
  ( Animation (..)
  , Ease (..)
  , SpringParams (..)
  , animInProgress
  , animationValue
  , applyEase
  , approxEq
  , easeSameSpec
  , presetBouncy
  , presetSmooth
  , presetStiff
  , springEps
  , stepAnim
  , writeRest
  )
import NanoUI.Atlas (ImageAtlas, atlasTextureId)
import NanoUI.Atlas qualified as Atlas
import Data.Vector (Vector)
import Data.Vector qualified as V
import NanoUI.Draw (DrawArena, DrawingBuild, DrawOp, newDrawArena, shiftDrawOp)
import NanoUI.Font (FontMetrics, measureText, monospaceMetrics)
import NanoUI.Frame.SpanArena (SpanArena, newSpanArena)
import NanoUI.Types (HostProfile (..), isCellHost)
import NanoUI.Icons (IconSet, Icons, asciiIcons, iconsFor)
import NanoUI.Frame.Scroll.Geometry
  ( ScrollConfig (..)
  , decodeScrollConfig
  , defaultScrollConfig
  , encodeScrollConfig
  , scrollConfigNative2D
  )
import NanoUI.Id (IdContext, WidgetId (..), hashWidgetId, initialIdContext)
import NanoUI.Input (Input (..), Key (KeyEscape), inputKeys, inputKeysElem, inputMousePos, inputMousePressed)
import NanoUI.Layout.Arena (NodeArena, NodeType, getRect, lookupNodeByKey, newNodeArena)


import NanoUI.Store
  ( WidgetStore (..)
  , anySelectOpen
  , boolInt
  , bumpMirror
  , closeSelects
  , emptyWidgetStore
  , intBool
  , isSelectOpen
  , setSelectOpen
  , slotAnchor
  , slotCursor
  , slotDisabled
  , slotDrag
  , slotDragW
  , slotKey
  , slotScrollCfg
  , slotScrollContent
  , slotScrollOff
  , slotScrollCross
  , slotScrollLinkX
  , slotScrollLinkY
  , slotWinSize
  )
import NanoUI.Style (Layout, Theme, defaultTheme)
import NanoUI.Types
  ( Damage (..)
  , DamageBounds (..)
  , ImageId (..)
  , PopupAnchor (..)
  , PopupPlacement (..)
  , Rect (..)
  , Size (..)
  , V2 (..)
  , defaultDamageSlop
  , rectContains
  , rectH
  , rectW
  , v2X
  , v2Y
  )

data FrameMsg where
  FrameMsg :: Typeable a => a -> FrameMsg

decodeMessages :: Typeable a => [FrameMsg] -> [a]
decodeMessages = mapMaybe (\(FrameMsg x) -> cast x)

reduceMessages :: Typeable msg => (msg -> model -> model) -> model -> [FrameMsg] -> model
reduceMessages update model = foldl' (flip update) model . decodeMessages

reduceUpdates :: Typeable model => model -> [FrameMsg] -> model
reduceUpdates = reduceMessages ($)

type MeasureCacheKey = (Text, Float)

-- | Explicit damage invalidation request queued during frame evaluation.
data DamageRequest
  = ReqWidget !WidgetId !DamageBounds      -- ^ Invalidate widget layout bounds (old & new)
  | ReqKey !Int !DamageBounds              -- ^ Invalidate widget bounds by integer key
  | ReqRect !Rect                          -- ^ Invalidate an explicit window-space rectangle
  | ReqPeers ![WidgetId] !DamageBounds     -- ^ Invalidate a collection of widgets
  | ReqFull                                -- ^ Force full window invalidation
  deriving (Eq, Show)

data TextInputMenu = TextInputMenu
  { textInputMenuWidget :: WidgetId
  , textInputMenuRect :: Rect
  }
  deriving (Eq, Show)

data TextInputDrag = TextInputDrag
  { textInputDragWidget :: WidgetId
  , textInputDragAnchor :: Int
  , textInputDragAnchorRow :: Int
  , textInputDragAnchorCol :: Int
  , textInputDragMultiline :: Bool
  , textInputDragClicks :: Int
  }
  deriving (Eq, Show)

data TextFieldClickCell = TextFieldClickCell
  { textFieldClickWidget :: WidgetId
  , textFieldClickFlat :: Int
  , textFieldClickRow :: Int
  , textFieldClickCol :: Int
  , textFieldClickMultiline :: Bool
  }
  deriving (Eq, Show)

data WindowResizeEdge
  = ResizeN
  | ResizeS
  | ResizeE
  | ResizeW
  | ResizeNE
  | ResizeNW
  | ResizeSE
  | ResizeSW
  deriving (Eq, Show)

data WindowResizeDrag = WindowResizeDrag
  { wrdWidget :: WidgetId
  , wrdEdge :: WindowResizeEdge
  , wrdGrabX :: Float
  , wrdGrabY :: Float
  , wrdStartX :: Float
  , wrdStartY :: Float
  , wrdStartW :: Float
  , wrdStartH :: Float
  , wrdMinW :: Float
  , wrdMinH :: Float
  , wrdMaxW :: Float
  , wrdMaxH :: Float
  }
data DamageState = DamageState
  { dsDirty :: !Bool
  , dsDamage :: !Damage
  , dsRequests :: ![DamageRequest]
  , dsLastWindowSize :: !Size
  , dsPrevRects :: !(IntMap Rect)
  , dsPrevClips :: !(IntMap Rect)
  , dsPrevNodeTexts :: !(IntMap Text)
  }

initialDamageState :: DamageState
initialDamageState = DamageState
  { dsDirty = True
  , dsDamage = DamageFull
  , dsRequests = []
  , dsLastWindowSize = Size 0 0
  , dsPrevRects = IM.empty
  , dsPrevClips = IM.empty
  , dsPrevNodeTexts = IM.empty
  }

data OverlayState = OverlayState
  { osModalWasActive :: !Bool
  , osModalActive :: !Bool
  , osModalDepth :: !Int
  , osEscapeConsumed :: !Bool
  , osPrevFloatingRects :: !(IntMap Rect)
  , osPrevFloatingOrder :: ![Int]
  , osTopmostCache :: !(Maybe (V2, Maybe WidgetId))
  , osCurrentFloatingId :: !(Maybe WidgetId)
  , osLastPointerBlocked :: !Bool
  , osFloatingAncestor :: !(Maybe (IntMap (Maybe NodeType)))
  }

initialOverlayState :: OverlayState
initialOverlayState = OverlayState
  { osModalWasActive = False
  , osModalActive = False
  , osModalDepth = 0
  , osEscapeConsumed = False
  , osPrevFloatingRects = IM.empty
  , osPrevFloatingOrder = []
  , osTopmostCache = Nothing
  , osCurrentFloatingId = Nothing
  , osLastPointerBlocked = False
  , osFloatingAncestor = Nothing
  }

data AnimationState = AnimationState
  { asAnimations :: !(IntMap Animation)
  , asAnimRest :: !(IntMap Float)
  , asAnyAnimating :: !Bool
  , asAnimSettled :: !Bool
  }

initialAnimationState :: AnimationState
initialAnimationState = AnimationState
  { asAnimations = IM.empty
  , asAnimRest = IM.empty
  , asAnyAnimating = False
  , asAnimSettled = False
  }

data DrawingCacheState = DrawingCacheState
  { dcsPopupConfigs :: !(IntMap (PopupAnchor, PopupPlacement, Float))
  , dcsDrawings :: !(IntMap DrawingBuild)
  , dcsDrawOpCache :: !(IntMap (Rect, Vector DrawOp))
  , dcsDrawFitCache :: !(IntMap DrawFitCache)
  , dcsWidgetNodeTypes :: !(Maybe (IntMap NodeType))
  }

initialDrawingCacheState :: DrawingCacheState
initialDrawingCacheState = DrawingCacheState
  { dcsPopupConfigs = IM.empty
  , dcsDrawings = IM.empty
  , dcsDrawOpCache = IM.empty
  , dcsDrawFitCache = IM.empty
  , dcsWidgetNodeTypes = Nothing
  }

data Context = Context
  { ctxNodeArena :: NodeArena
  , ctxDrawArena :: DrawArena
  , ctxHotId :: IORef WidgetId
  , ctxLastHotId :: IORef WidgetId
  , ctxActiveId :: IORef WidgetId
  , ctxClickedId :: IORef WidgetId
  , ctxReleaseClickedId :: IORef WidgetId
  , ctxFocusId :: IORef WidgetId
  , ctxStore :: IORef WidgetStore
  , ctxDamageState :: IORef DamageState
  , ctxOverlayState :: IORef OverlayState
  , ctxAnimationState :: IORef AnimationState
  , ctxDrawingCache :: IORef DrawingCacheState
  , ctxIdContext :: IORef IdContext
  , ctxFontMetrics :: FontMetrics
  , ctxMonoFontMetrics :: FontMetrics
  , ctxMeasureText :: Text -> IO (Float, Float)
  , ctxMeasureCache :: Maybe (IORef (HashMap MeasureCacheKey (Float, Float)))
  , ctxExternalText :: Bool
  , ctxTheme :: Theme
  , ctxIcons :: Icons
  , ctxContainerStack :: IORef [Int]
  , ctxMessages :: IORef [FrameMsg]
  , ctxFocusables :: IORef (MutablePrimArray RealWorld WidgetId)
  , ctxFocusablesCount :: IORef Int
  , ctxFocusablesCap :: IORef Int
  , ctxSpanBase :: SpanArena
  , ctxSpanOverlay :: SpanArena
  , ctxScrollDrag :: IORef (Maybe (WidgetId, Float))
  , ctxTextInputDrag :: IORef (Maybe TextInputDrag)
  , ctxTextFieldClickCell :: IORef (Maybe TextFieldClickCell)
  , ctxTextInputMenu :: IORef (Maybe TextInputMenu)
  , ctxClipboardGet :: IO (Maybe Text)
  , ctxClipboardSet :: Text -> IO Bool
  , ctxSelectDropPress :: IORef Bool
  , ctxOpenSelectDrop :: IORef (Maybe (WidgetId, Rect))
  , ctxMenuPointerGesture :: IORef Bool
  , ctxWindowDrag :: IORef (Maybe (WidgetId, Float, Float))
  , ctxWindowResize :: IORef (Maybe WindowResizeDrag)
  , ctxImageAtlas :: ImageAtlas
  , ctxWakeLoop :: IORef (Maybe (IO ()))
  , ctxHost :: IORef (Map TypeRep Dynamic)
  , ctxHostProfile :: HostProfile
  }

{-# INLINE intKey #-}
intKey :: WidgetId -> Int
intKey = fromIntegral . hashWidgetId

{-# INLINE requestDamage #-}
requestDamage :: Context -> DamageRequest -> IO ()
requestDamage ctx req =
  modifyIORef' (ctxDamageState ctx) $ \ds ->
    ds {dsRequests = req : dsRequests ds}

{-# INLINE damageWidget #-}
damageWidget :: Context -> WidgetId -> DamageBounds -> IO ()
damageWidget ctx wid bounds
  | hashWidgetId wid == 0 = pure ()
  | otherwise = requestDamage ctx (ReqWidget wid bounds)

{-# INLINE damageKey #-}
damageKey :: Context -> Int -> DamageBounds -> IO ()
damageKey ctx k bounds
  | k == 0 = pure ()
  | otherwise = requestDamage ctx (ReqKey k bounds)

{-# INLINE damageRect #-}
damageRect :: Context -> Rect -> IO ()
damageRect ctx r
  | rectW r <= 0 || rectH r <= 0 = pure ()
  | otherwise = requestDamage ctx (ReqRect r)

{-# INLINE damagePeers #-}
damagePeers :: Context -> [WidgetId] -> DamageBounds -> IO ()
damagePeers ctx wids bounds =
  case filter (\w -> hashWidgetId w /= 0) wids of
    [] -> pure ()
    valid -> requestDamage ctx (ReqPeers valid bounds)

{-# INLINE damageFull #-}
damageFull :: Context -> IO ()
damageFull ctx = requestDamage ctx ReqFull

{-# INLINE clearDamageRequests #-}
clearDamageRequests :: Context -> IO ()
clearDamageRequests ctx =
  modifyIORef' (ctxDamageState ctx) $ \ds ->
    ds {dsRequests = []}

{-# INLINE getDamageRequests #-}
getDamageRequests :: Context -> IO [DamageRequest]
getDamageRequests ctx = dsRequests <$> readIORef (ctxDamageState ctx)

{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = do
  modifyIORef' (ctxDamageState ctx) $ \ds -> ds {dsDirty = True}
  wake <- readIORef (ctxWakeLoop ctx)
  case wake of
    Just act -> act
    Nothing -> pure ()

{-# INLINE modifyIORefList #-}
modifyIORefList :: IORef [a] -> (a -> [a] -> [a]) -> a -> IO ()
modifyIORefList ref cons val = do
  xs <- readIORef ref
  writeIORef ref (cons val xs)

{-# INLINE clearDirty #-}
clearDirty :: Context -> IO ()
clearDirty ctx =
  modifyIORef' (ctxDamageState ctx) $ \ds -> ds {dsDirty = False}

{-# INLINE isDirty #-}
isDirty :: Context -> IO Bool
isDirty ctx = dsDirty <$> readIORef (ctxDamageState ctx)

{-# INLINE setWakeLoop #-}
setWakeLoop :: Context -> IO () -> IO ()
setWakeLoop ctx wake = writeIORef (ctxWakeLoop ctx) (Just wake)

{-# INLINE takeDamage #-}
takeDamage :: Context -> IO Damage
takeDamage ctx = dsDamage <$> readIORef (ctxDamageState ctx)

{-# INLINE setDamage #-}
setDamage :: Context -> Damage -> IO ()
setDamage ctx dmg =
  modifyIORef' (ctxDamageState ctx) $ \ds -> ds {dsDamage = dmg}

{-# INLINE getLastWindowSize #-}
getLastWindowSize :: Context -> IO Size
getLastWindowSize ctx = dsLastWindowSize <$> readIORef (ctxDamageState ctx)

{-# INLINE setDamageAndWindowSize #-}
setDamageAndWindowSize :: Context -> Damage -> Size -> IO ()
setDamageAndWindowSize ctx dmg sz =
  modifyIORef' (ctxDamageState ctx) $ \ds ->
    ds {dsDamage = dmg, dsLastWindowSize = sz, dsRequests = []}

{-# INLINE registerPopupConfig #-}
registerPopupConfig :: Context -> WidgetId -> PopupAnchor -> PopupPlacement -> Float -> IO ()
registerPopupConfig ctx wid anchor placement offset =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    dc {dcsPopupConfigs = IM.insert (intKey wid) (anchor, placement, offset) (dcsPopupConfigs dc)}

{-# INLINE lookupPopupConfig #-}
lookupPopupConfig :: Context -> WidgetId -> IO (Maybe (PopupAnchor, PopupPlacement, Float))
lookupPopupConfig ctx wid = do
  dc <- readIORef (ctxDrawingCache ctx)
  pure (IM.lookup (intKey wid) (dcsPopupConfigs dc))

{-# INLINE clearPopupConfigs #-}
clearPopupConfigs :: Context -> IO ()
clearPopupConfigs ctx =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    dc {dcsPopupConfigs = IM.empty}

data DrawFitCache = DrawFitCache
  { dfcDw :: {-# UNPACK #-} !Double
  , dfcDh :: {-# UNPACK #-} !Double
  , dfcLh :: {-# UNPACK #-} !Float
  , dfcContent :: {-# UNPACK #-} !Int
  , dfcIn :: !Layout
  , dfcOut :: !Layout
  }

{-# INLINE registerDrawing #-}
registerDrawing :: Context -> WidgetId -> DrawingBuild -> IO ()
registerDrawing ctx wid build =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    dc {dcsDrawings = IM.insert (intKey wid) build (dcsDrawings dc)}

{-# INLINE lookupDrawing #-}
lookupDrawing :: Context -> WidgetId -> IO (Maybe DrawingBuild)
lookupDrawing ctx wid = do
  dc <- readIORef (ctxDrawingCache ctx)
  pure (IM.lookup (intKey wid) (dcsDrawings dc))

-- | Rebuild draw ops when width or height change. A move only translates.
cachedDrawingOps :: Context -> WidgetId -> Rect -> DrawingBuild -> IO (Vector DrawOp)
cachedDrawingOps ctx wid rect build = do
  let k = intKey wid
  dc <- readIORef (ctxDrawingCache ctx)
  case IM.lookup k (dcsDrawOpCache dc) of
    Just (r, ops)
      | rectW r == rectW rect && rectH r == rectH rect ->
          if rectX r == rectX rect && rectY r == rectY rect
            then pure ops
            else do
              let ops' =
                    V.map
                      (shiftDrawOp (rectX rect - rectX r) (rectY rect - rectY r))
                      ops
              modifyIORef' (ctxDrawingCache ctx) $ \s ->
                s {dcsDrawOpCache = IM.insert k (rect, ops') (dcsDrawOpCache s)}
              pure ops'
    _ -> do
      let ops = build rect
      modifyIORef' (ctxDrawingCache ctx) $ \s ->
        s {dcsDrawOpCache = IM.insert k (rect, ops) (dcsDrawOpCache s)}
      pure ops

-- | Reuse a derived layout while envelope, font, content key, and caller layout match.
cachedWidgetLayout ::
  Context ->
  WidgetId ->
  Double ->
  Double ->
  Float ->
  Int ->
  Layout ->
  IO Layout ->
  IO Layout
cachedWidgetLayout ctx wid dw dh lh content incoming compute = do
  let k = intKey wid
  dc <- readIORef (ctxDrawingCache ctx)
  case IM.lookup k (dcsDrawFitCache dc) of
    Just e
      | dfcDw e == dw
          && dfcDh e == dh
          && dfcLh e == lh
          && dfcContent e == content
          && dfcIn e == incoming ->
          pure (dfcOut e)
    _ -> do
      out <- compute
      modifyIORef' (ctxDrawingCache ctx) $ \s ->
        s {dcsDrawFitCache = IM.insert k (DrawFitCache dw dh lh content incoming out) (dcsDrawFitCache s)}
      pure out

-- | Drop cached ops for drawings that did not rebuild this frame.
pruneDrawOpCache :: Context -> IO ()
pruneDrawOpCache ctx =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    let live = dcsDrawings dc
     in dc
          { dcsDrawOpCache = dcsDrawOpCache dc `IM.intersection` live
          , dcsDrawFitCache = dcsDrawFitCache dc `IM.intersection` live
          }

{-# INLINE clearDrawings #-}
clearDrawings :: Context -> IO ()
clearDrawings ctx =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    dc {dcsDrawings = IM.empty}

{-# INLINE getWidgetNodeTypes #-}
getWidgetNodeTypes :: Context -> IO (Maybe (IntMap NodeType))
getWidgetNodeTypes ctx = dcsWidgetNodeTypes <$> readIORef (ctxDrawingCache ctx)

{-# INLINE setWidgetNodeTypes #-}
setWidgetNodeTypes :: Context -> Maybe (IntMap NodeType) -> IO ()
setWidgetNodeTypes ctx m =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    dc {dcsWidgetNodeTypes = m}

{-# INLINE resetDrawingScopeCache #-}
resetDrawingScopeCache :: Context -> IO ()
resetDrawingScopeCache ctx =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    dc
      { dcsDrawings = IM.empty
      , dcsPopupConfigs = IM.empty
      , dcsWidgetNodeTypes = Nothing
      }

{-# INLINE getStore #-}
getStore :: Context -> IO WidgetStore
getStore ctx = readIORef (ctxStore ctx)

{-# INLINE setStore #-}
setStore :: Context -> WidgetStore -> IO ()
setStore ctx store = do
  prev <- readIORef (ctxStore ctx)
  writeIORef (ctxStore ctx) store
  when (prev /= store) $ do
    let changedKeys =
          diffKeys (storeInt prev) (storeInt store)
            ++ diffKeys (storeFloat prev) (storeFloat store)
            ++ diffKeys (storePoint prev) (storePoint store)
            ++ diffKeys (storeText prev) (storeText store)
            ++ diffKeys (storeFloatList prev) (storeFloatList store)
            ++ diffKeys (storeIntSet prev) (storeIntSet store)
    forM_ changedKeys $ \k -> damageKey ctx k (DamageInflated defaultDamageSlop)
    markDirty ctx

diffKeys :: Eq a => IntMap a -> IntMap a -> [Int]
diffKeys old new =
  IM.keys
    ( IM.mergeWithKey
        (\_ a b -> if a == b then Nothing else Just ())
        (IM.map (const ()))
        (IM.map (const ()))
        old
        new
    )

{-# INLINE isDisabled #-}
isDisabled :: Context -> WidgetId -> IO Bool
isDisabled ctx wid = do
  s <- getStore ctx
  pure (intBool (IM.findWithDefault 0 (slotKey slotDisabled (intKey wid)) (storeInt s)))

{-# INLINE setDisabled #-}
setDisabled :: Context -> WidgetId -> Bool -> IO ()
setDisabled ctx wid dis = do
  s <- getStore ctx
  setStore ctx (s {storeInt = IM.insert (slotKey slotDisabled (intKey wid)) (boolInt dis) (storeInt s)})

{-# INLINE getScrollOffset #-}
getScrollOffset :: Context -> WidgetId -> IO Float
getScrollOffset ctx wid = do
  cfg <- getScrollConfig ctx wid
  if scrollConfigNative2D cfg
    then v2Y <$> getScrollOffset2D ctx wid
    else do
      s <- getStore ctx
      pure (IM.findWithDefault 0 (intKey wid) (storeFloat s))

{-# INLINE setScrollOffset #-}
setScrollOffset :: Context -> WidgetId -> Float -> IO ()
setScrollOffset ctx wid off = do
  cfg <- getScrollConfig ctx wid
  if scrollConfigNative2D cfg
    then do
      cur <- getScrollOffset2D ctx wid
      setScrollOffset2D ctx wid (V2 (v2X cur) off)
    else do
      store <- getStore ctx
      let key = intKey wid
          prev = IM.findWithDefault 0 key (storeFloat store)
      when (prev /= off) $ do
        let floats0 = IM.insert key off (storeFloat store)
            yKey = IM.findWithDefault 0 (slotKey slotScrollLinkY key) (storeInt store)
        if yKey == 0
          then setStore ctx (store {storeFloat = floats0})
          else do
            let offKey = slotKey slotScrollOff yKey
                crossKey = slotKey slotScrollCross yKey
                prevY = IM.findWithDefault 0 yKey floats0
                floats1 = IM.insert yKey prevY $ IM.insert crossKey off floats0
                points = IM.insert offKey (off, prevY) (storePoint store)
            setStore ctx (store {storeFloat = floats1, storePoint = points})

{-# INLINE getScrollOffset2D #-}
getScrollOffset2D :: Context -> WidgetId -> IO V2
getScrollOffset2D ctx wid = do
  s <- getStore ctx
  let widKey = intKey wid
      offKey = slotKey slotScrollOff widKey
      crossKey = slotKey slotScrollCross widKey
  case IM.lookup offKey (storePoint s) of
    Just (x, y) -> pure (V2 x y)
    Nothing ->
      pure
        ( V2
            (IM.findWithDefault 0 crossKey (storeFloat s))
            (IM.findWithDefault 0 widKey (storeFloat s))
        )

{-# INLINE setScrollOffset2D #-}
setScrollOffset2D :: Context -> WidgetId -> V2 -> IO ()
setScrollOffset2D ctx wid off = do
  store <- getStore ctx
  let widKey = intKey wid
      offKey = slotKey slotScrollOff widKey
      crossKey = slotKey slotScrollCross widKey
      prev = IM.lookup offKey (storePoint store)
      next = (v2X off, v2Y off)
      prevY = IM.findWithDefault 0 widKey (storeFloat store)
      prevX = IM.findWithDefault 0 crossKey (storeFloat store)
      xLink = IM.findWithDefault 0 (slotKey slotScrollLinkX widKey) (storeInt store)
  when (prev /= Just next || prevY /= v2Y off || prevX /= v2X off) $ do
    let floats0 =
          IM.insert widKey (v2Y off) $
            IM.insert crossKey (v2X off) (storeFloat store)
        floats1 =
          if xLink == 0 then floats0 else IM.insert xLink (v2X off) floats0
    setStore ctx
      ( store
          { storePoint = IM.insert offKey next (storePoint store)
          , storeFloat = floats1
          }
      )

{-# INLINE linkScrollAxes #-}
linkScrollAxes :: Context -> WidgetId -> WidgetId -> IO ()
linkScrollAxes ctx yWid xWid = do
  store <- getStore ctx
  let yKey = intKey yWid
      xKey = intKey xWid
      ints =
        IM.insert (slotKey slotScrollLinkX yKey) xKey $
          IM.insert (slotKey slotScrollLinkY xKey) yKey (storeInt store)
  setStore ctx (store {storeInt = ints})
  V2 x2 y <- getScrollOffset2D ctx yWid
  x1 <- do
    s <- getStore ctx
    pure (IM.findWithDefault 0 xKey (storeFloat s))
  let x = if x2 == 0 && x1 /= 0 then x1 else x2
  when (x /= x2 || x /= x1) $
    setScrollOffset2D ctx yWid (V2 x y)

{-# INLINE getScrollConfig #-}
getScrollConfig :: Context -> WidgetId -> IO ScrollConfig
getScrollConfig ctx wid = do
  s <- getStore ctx
  let cfgKey = slotKey slotScrollCfg (intKey wid)
      bits = IM.findWithDefault (encodeScrollConfig defaultScrollConfig) cfgKey (storeInt s)
  pure (decodeScrollConfig bits)

{-# INLINE setScrollConfig #-}
setScrollConfig :: Context -> WidgetId -> ScrollConfig -> IO ()
setScrollConfig ctx wid cfg = do
  store <- getStore ctx
  let cfgKey = slotKey slotScrollCfg (intKey wid)
      bits = encodeScrollConfig cfg
      prev = IM.findWithDefault (encodeScrollConfig defaultScrollConfig) cfgKey (storeInt store)
  when (prev /= bits) $
    setStore ctx (store {storeInt = IM.insert cfgKey bits (storeInt store)})

{-# INLINE getScrollContentExtent #-}
getScrollContentExtent :: Context -> WidgetId -> IO (Float, Float)
getScrollContentExtent ctx wid = do
  s <- getStore ctx
  let key = slotKey slotScrollContent (intKey wid)
  case IM.lookup key (storePoint s) of
    Just (w, h) -> pure (w, h)
    Nothing -> pure (0, 0)

{-# INLINE setScrollContentExtent #-}
setScrollContentExtent :: Context -> WidgetId -> Float -> Float -> IO ()
setScrollContentExtent ctx wid w h = do
  store <- getStore ctx
  let key = slotKey slotScrollContent (intKey wid)
      next = (w, h)
      prev = IM.lookup key (storePoint store)
  when (prev /= Just next) $
    setStore ctx (store {storePoint = IM.insert key next (storePoint store)})

{-# INLINE getPrevRects #-}
getPrevRects :: Context -> IO (IntMap Rect)
getPrevRects ctx = dsPrevRects <$> readIORef (ctxDamageState ctx)

{-# INLINE getPrevClips #-}
getPrevClips :: Context -> IO (IntMap Rect)
getPrevClips ctx = dsPrevClips <$> readIORef (ctxDamageState ctx)

{-# INLINE setPrevRectsAndClips #-}
setPrevRectsAndClips :: Context -> IntMap Rect -> IntMap Rect -> IO ()
setPrevRectsAndClips ctx rects clips =
  modifyIORef' (ctxDamageState ctx) $ \ds ->
    ds {dsPrevRects = rects, dsPrevClips = clips}

{-# INLINE getPrevRectByKey #-}
getPrevRectByKey :: Context -> Int -> IO (Maybe Rect)
getPrevRectByKey ctx k = do
  m <- dsPrevRects <$> readIORef (ctxDamageState ctx)
  pure (IM.lookup k m)

{-# INLINE getPrevRect #-}
getPrevRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevRect ctx wid = getPrevRectByKey ctx (intKey wid)

{-# INLINE getPrevClipRectByKey #-}
getPrevClipRectByKey :: Context -> Int -> IO (Maybe Rect)
getPrevClipRectByKey ctx k = do
  m <- dsPrevClips <$> readIORef (ctxDamageState ctx)
  pure (IM.lookup k m)

{-# INLINE getPrevClipRect #-}
getPrevClipRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevClipRect ctx wid = getPrevClipRectByKey ctx (intKey wid)

{-# INLINE setPrevRect #-}
setPrevRect :: Context -> WidgetId -> Rect -> IO ()
setPrevRect ctx wid r =
  modifyIORef' (ctxDamageState ctx) $ \ds ->
    ds {dsPrevRects = IM.insert (intKey wid) r (dsPrevRects ds)}

{-# INLINE registerImage #-}
registerImage :: Context -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerImage ctx iid w h px = do
  ok <- Atlas.registerImage (ctxImageAtlas ctx) iid w h px
  when ok (markDirty ctx)
  pure ok

{-# INLINE registerImages #-}
registerImages :: Context -> [(ImageId, Int, Int, ByteString)] -> IO Bool
registerImages ctx imgs = do
  results <- mapM (\(iid, w, h, px) -> registerImage ctx iid w h px) imgs
  pure (and results)

{-# INLINE lookupImageUv #-}
lookupImageUv :: Context -> ImageId -> IO (Maybe (Float, Float, Float, Float))
lookupImageUv ctx = Atlas.lookupImageUv (ctxImageAtlas ctx)

{-# INLINE atlasSnapshot #-}
atlasSnapshot :: Context -> IO (Maybe (Int, Int, ForeignPtr Word8, Int))
atlasSnapshot ctx = Atlas.atlasSnapshot (ctxImageAtlas ctx)

withFontMetrics :: Context -> FontMetrics -> Context
withFontMetrics ctx fm =
  ctx
    { ctxFontMetrics = fm
    , ctxMonoFontMetrics = if isCellHost (ctxHostProfile ctx) then fm else ctxMonoFontMetrics ctx
    , ctxMeasureText = \txt ->
        pure (measureText (ctxHostProfile ctx) fm txt)
    }

withMonoFontMetrics :: Context -> FontMetrics -> Context
withMonoFontMetrics ctx mono =
  ctx {ctxMonoFontMetrics = mono}

withMeasureText :: Context -> (Text -> IO (Float, Float)) -> Context
withMeasureText ctx fn = ctx {ctxMeasureText = fn}

cacheMeasureText ::
  IORef (HashMap MeasureCacheKey (Float, Float)) ->
  Float ->
  (Text -> IO (Float, Float)) ->
  Text ->
  IO (Float, Float)
cacheMeasureText ref scale base txt = do
  let key = (txt, scale)
  m <- readIORef ref
  case HashMap.lookup key m of
    Just sz -> pure sz
    Nothing -> do
      sz <- base txt
      modifyIORef' ref (HashMap.insert key sz)
      pure sz

{-# INLINE wrapMeasureCache #-}
wrapMeasureCache :: Float -> Context -> (Text -> IO (Float, Float)) -> Context
wrapMeasureCache scale ctx measure =
  case ctxMeasureCache ctx of
    Nothing -> ctx {ctxMeasureText = measure}
    Just ref -> ctx {ctxMeasureText = cacheMeasureText ref scale measure}

clearMeasureCache :: Context -> IO ()
clearMeasureCache ctx =
  case ctxMeasureCache ctx of
    Just ref -> writeIORef ref HashMap.empty
    Nothing -> pure ()

withExternalText :: Context -> Bool -> Context
withExternalText ctx ext = ctx {ctxExternalText = ext}

withTheme :: Context -> Theme -> Context
withTheme ctx theme = ctx {ctxTheme = theme}

withIcons :: Context -> IconSet -> Context
withIcons ctx iset = ctx {ctxIcons = iconsFor iset}

withHostProfile :: Context -> HostProfile -> Context
withHostProfile ctx prof = ctx {ctxHostProfile = prof}

withClipboard :: Context -> IO (Maybe Text) -> (Text -> IO Bool) -> Context
withClipboard ctx getter setter = ctx {ctxClipboardGet = getter, ctxClipboardSet = setter}

enableMeasureCache :: Context -> IO Context
enableMeasureCache ctx =
  case ctxMeasureCache ctx of
    Just _ -> pure ctx
    Nothing -> do
      ref <- newIORef HashMap.empty
      pure ctx {ctxMeasureCache = Just ref, ctxMeasureText = cacheMeasureText ref 0 (ctxMeasureText ctx)}

{-# INLINE setHost #-}
setHost :: forall a. (Typeable a) => Context -> a -> IO ()
setHost ctx val = do
  m <- readIORef (ctxHost ctx)
  let k = typeOf val
  writeIORef (ctxHost ctx) (Map.insert k (toDyn val) m)

{-# INLINE askHostIO #-}
askHostIO :: forall a. (Typeable a) => Context -> IO (Maybe a)
askHostIO ctx = do
  m <- readIORef (ctxHost ctx)
  let k = typeRep (Proxy :: Proxy a)
  pure (Map.lookup k m >>= fromDynamic)

{-# INLINE pushMessage #-}
pushMessage :: Context -> FrameMsg -> IO ()
pushMessage ctx msg = modifyIORefList (ctxMessages ctx) (:) msg

{-# INLINE drainMessages #-}
drainMessages :: Context -> IO [FrameMsg]
drainMessages ctx = do
  msgs <- readIORef (ctxMessages ctx)
  writeIORef (ctxMessages ctx) []
  pure (reverse msgs)

-- =============================================================================
-- Constructors
-- =============================================================================

{-# INLINE newContext #-}
newContext :: IO Context
newContext = do
  nodeArena <- newNodeArena
  drawArena <- newDrawArena
  ctxHotId <- newIORef (WidgetId 0)
  ctxLastHotId <- newIORef (WidgetId 0)
  ctxActiveId <- newIORef (WidgetId 0)
  ctxClickedId <- newIORef (WidgetId 0)
  ctxReleaseClickedId <- newIORef (WidgetId 0)
  ctxFocusId <- newIORef (WidgetId 0)
  ctxStore <- newIORef emptyWidgetStore
  ctxDamageState <- newIORef initialDamageState
  ctxOverlayState <- newIORef initialOverlayState
  ctxAnimationState <- newIORef initialAnimationState
  ctxDrawingCache <- newIORef initialDrawingCacheState
  ctxIdContext <- newIORef initialIdContext
  ctxContainerStack <- newIORef []
  ctxMessages <- newIORef []
  let initCap = 64
  ctxFocusables <- newIORef =<< newPrimArray initCap
  ctxFocusablesCount <- newIORef 0
  ctxFocusablesCap <- newIORef initCap
  ctxSpanBase <- newSpanArena 64
  ctxSpanOverlay <- newSpanArena 64
  ctxScrollDrag <- newIORef Nothing
  ctxTextInputDrag <- newIORef Nothing
  ctxTextFieldClickCell <- newIORef Nothing
  ctxTextInputMenu <- newIORef Nothing
  ctxSelectDropPress <- newIORef False
  ctxOpenSelectDrop <- newIORef Nothing
  ctxMenuPointerGesture <- newIORef False
  ctxWindowDrag <- newIORef Nothing
  ctxWindowResize <- newIORef Nothing
  ctxImageAtlas <- Atlas.newImageAtlas
  ctxWakeLoop <- newIORef Nothing
  ctxHost <- newIORef Map.empty
  let fm0 = monospaceMetrics 12
  pure Context
    { ctxNodeArena = nodeArena
    , ctxDrawArena = drawArena
    , ctxHotId
    , ctxLastHotId
    , ctxActiveId
    , ctxClickedId
    , ctxReleaseClickedId
    , ctxFocusId
    , ctxStore
    , ctxDamageState
    , ctxOverlayState
    , ctxAnimationState
    , ctxDrawingCache
    , ctxIdContext
    , ctxFontMetrics = fm0
    , ctxMonoFontMetrics = fm0
    , ctxMeasureText = \txt -> pure (measureText PixelHost fm0 txt)
    , ctxMeasureCache = Nothing
    , ctxExternalText = False
    , ctxTheme = defaultTheme
    , ctxIcons = asciiIcons
    , ctxContainerStack
    , ctxMessages
    , ctxFocusables
    , ctxFocusablesCount
    , ctxFocusablesCap
    , ctxSpanBase
    , ctxSpanOverlay
    , ctxScrollDrag
    , ctxTextInputDrag
    , ctxTextFieldClickCell
    , ctxTextInputMenu
    , ctxClipboardGet = pure Nothing
    , ctxClipboardSet = \_ -> pure False
    , ctxSelectDropPress
    , ctxOpenSelectDrop
    , ctxMenuPointerGesture
    , ctxWindowDrag
    , ctxWindowResize
    , ctxImageAtlas
    , ctxWakeLoop
    , ctxHost
    , ctxHostProfile = PixelHost
    }

{-# INLINE newPixelHostContext #-}
newPixelHostContext :: IO Context
newPixelHostContext = do
  ctx0 <- newContext
  ctx <- enableMeasureCache ctx0
  pure (withExternalText (withTheme (withFontMetrics ctx (monospaceMetrics 16)) defaultTheme) True)

-- =============================================================================
-- Focus
-- =============================================================================

{-# INLINE getFocusId #-}
getFocusId :: Context -> IO WidgetId
getFocusId ctx = readIORef (ctxFocusId ctx)

{-# INLINE getHotId #-}
getHotId :: Context -> IO WidgetId
getHotId ctx = readIORef (ctxHotId ctx)

{-# INLINE registerFocusable #-}
registerFocusable :: Context -> WidgetId -> IO ()
registerFocusable ctx wid = do
  idx <- readIORef (ctxFocusablesCount ctx)
  cap <- readIORef (ctxFocusablesCap ctx)
  arr <- readIORef (ctxFocusables ctx)
  arr' <-
    if idx >= cap
      then do
        let newCap = max 16 (cap * 2)
        newArr <- newPrimArray newCap
        copyMutablePrimArray newArr 0 arr 0 idx
        writeIORef (ctxFocusables ctx) newArr
        writeIORef (ctxFocusablesCap ctx) newCap
        pure newArr
      else pure arr
  writePrimArray arr' idx wid
  writeIORef (ctxFocusablesCount ctx) (idx + 1)

{-# INLINE getFocusables #-}
getFocusables :: Context -> IO [WidgetId]
getFocusables ctx = do
  count <- readIORef (ctxFocusablesCount ctx)
  arr <- readIORef (ctxFocusables ctx)
  forM [0 .. count - 1] (readPrimArray arr)

{-# INLINE getFocusablesPrim #-}
getFocusablesPrim :: Context -> IO (PrimArray WidgetId)
getFocusablesPrim ctx = do
  count <- readIORef (ctxFocusablesCount ctx)
  arr <- readIORef (ctxFocusables ctx)
  freezePrimArray arr 0 count

-- =============================================================================
-- Modal & Overlay
-- =============================================================================

textInputEditActive :: Context -> IO Bool
textInputEditActive ctx = do
  focus <- readIORef (ctxFocusId ctx)
  menu <- readIORef (ctxTextInputMenu ctx)
  pure (hashWidgetId focus /= 0 || menu /= Nothing)

modalActive :: Context -> IO Bool
modalActive ctx = do
  os <- readIORef (ctxOverlayState ctx)
  pure (osModalWasActive os || osModalActive os)

overlayConsumesQuit :: Context -> Input -> IO Bool
overlayConsumesQuit ctx inp = do
  os <- readIORef (ctxOverlayState ctx)
  let esc = inputKeysElem KeyEscape (inputKeys inp)
  pure (esc && osEscapeConsumed os)

markEscapeConsumed :: Context -> IO ()
markEscapeConsumed ctx =
  modifyIORef' (ctxOverlayState ctx) $ \os ->
    os {osEscapeConsumed = True}

resetEscapeConsumed :: Context -> IO ()
resetEscapeConsumed ctx =
  modifyIORef' (ctxOverlayState ctx) $ \os ->
    os {osEscapeConsumed = False}

pointerBlockedByModal :: Context -> IO Bool
pointerBlockedByModal ctx = do
  os <- readIORef (ctxOverlayState ctx)
  if osModalDepth os > 0 then pure False else pure (osModalWasActive os || osModalActive os)

pointerBlockedByOverlay :: Context -> V2 -> IO Bool
pointerBlockedByOverlay ctx mouse = do
  gesture <- readIORef (ctxMenuPointerGesture ctx)
  blocked <-
    if gesture
      then pure True
      else do
        menuBlocked <- overlayMenuBlocksPointer ctx mouse
        if menuBlocked
          then pure True
          else do
            modalBlocked <- pointerBlockedByModal ctx
            if modalBlocked
              then pure True
              else do
                mTop <- cachedTopmost ctx mouse
                case mTop of
                  Nothing -> pure False
                  Just top -> do
                    mCur <- getCurrentFloatingId ctx
                    pure (mCur /= Just top)
  modifyIORef' (ctxOverlayState ctx) $ \os ->
    os {osLastPointerBlocked = blocked}
  pure blocked

menuPointerGestureActive :: Context -> IO Bool
menuPointerGestureActive ctx = readIORef (ctxMenuPointerGesture ctx)

armMenuPointerCapture :: Context -> Input -> IO ()
armMenuPointerCapture ctx inp =
  when (inputMousePressed inp) $ do
    blocked <- overlayMenuBlocksPointer ctx (inputMousePos inp)
    writeIORef (ctxMenuPointerGesture ctx) blocked

overlayMenuBlocksPointer :: Context -> V2 -> IO Bool
overlayMenuBlocksPointer ctx mouse = do
  mMenu <- readIORef (ctxTextInputMenu ctx)
  let textMenu =
        case mMenu of
          Just m | rectContains (textInputMenuRect m) mouse -> True
          _ -> False
  if textMenu
    then pure True
    else do
      mDrop <- readIORef (ctxOpenSelectDrop ctx)
      pure
        ( case mDrop of
            Just (_, r) -> rectContains r mouse
            Nothing -> False
        )

cachedTopmost :: Context -> V2 -> IO (Maybe WidgetId)
cachedTopmost ctx mouse = do
  cache <- osTopmostCache <$> readIORef (ctxOverlayState ctx)
  case cache of
    Just (p, t) | p == mouse -> pure t
    _ -> do
      t <- topmostFloatingAtMouse ctx mouse
      modifyIORef' (ctxOverlayState ctx) $ \os ->
        os {osTopmostCache = Just (mouse, t)}
      pure t

topmostFloatingAtMouse :: Context -> V2 -> IO (Maybe WidgetId)
topmostFloatingAtMouse ctx mouse = do
  os <- readIORef (ctxOverlayState ctx)
  let rects = osPrevFloatingRects os
      order = osPrevFloatingOrder os
  if IM.null rects && null order
    then pure Nothing
    else
      let hit k = case IM.lookup k rects of
            Just r | rectW r > 0 && rectH r > 0 && rectContains r mouse -> True
            _ -> False
          picked = foldl (\acc k -> if hit k then Just k else acc) Nothing order
       in case picked of
            Just k -> pure (Just (WidgetId (fromIntegral k)))
            Nothing -> pure Nothing

seedFloatingPanel :: Context -> WidgetId -> Rect -> IO ()
seedFloatingPanel ctx wid rect
  | rectW rect <= 0 || rectH rect <= 0 = pure ()
  | otherwise = do
      let k = intKey wid
      modifyIORef' (ctxOverlayState ctx) $ \os ->
        let rects = IM.insert k rect (osPrevFloatingRects os)
            order = filter (/= k) (osPrevFloatingOrder os) ++ [k]
         in os
              { osPrevFloatingRects = rects
              , osPrevFloatingOrder = order
              , osTopmostCache = Nothing
              }

beginModal :: Context -> IO ()
beginModal ctx =
  modifyIORef' (ctxOverlayState ctx) $ \os ->
    os {osModalActive = True, osModalDepth = osModalDepth os + 1}

endModal :: Context -> IO ()
endModal ctx =
  modifyIORef' (ctxOverlayState ctx) $ \os ->
    os {osModalDepth = max 0 (osModalDepth os - 1)}

beginFrameModal :: Context -> IO ()
beginFrameModal ctx =
  modifyIORef' (ctxOverlayState ctx) $ \os ->
    os
      { osModalWasActive = osModalActive os
      , osModalActive = False
      , osModalDepth = 0
      , osTopmostCache = Nothing
      , osCurrentFloatingId = Nothing
      , osLastPointerBlocked = False
      , osEscapeConsumed = False
      }

modalDamageFlip :: Context -> IO Bool
modalDamageFlip ctx = do
  os <- readIORef (ctxOverlayState ctx)
  pure (osModalWasActive os /= osModalActive os)

{-# INLINE getCurrentFloatingId #-}
getCurrentFloatingId :: Context -> IO (Maybe WidgetId)
getCurrentFloatingId ctx = osCurrentFloatingId <$> readIORef (ctxOverlayState ctx)

{-# INLINE setCurrentFloatingId #-}
setCurrentFloatingId :: Context -> Maybe WidgetId -> IO ()
setCurrentFloatingId ctx m =
  modifyIORef' (ctxOverlayState ctx) $ \os ->
    os {osCurrentFloatingId = m}

{-# INLINE getLastPointerBlocked #-}
getLastPointerBlocked :: Context -> IO Bool
getLastPointerBlocked ctx = osLastPointerBlocked <$> readIORef (ctxOverlayState ctx)

{-# INLINE getPrevFloatingRects #-}
getPrevFloatingRects :: Context -> IO (IntMap Rect)
getPrevFloatingRects ctx = osPrevFloatingRects <$> readIORef (ctxOverlayState ctx)

{-# INLINE setPrevFloatingPanels #-}
setPrevFloatingPanels :: Context -> IntMap Rect -> [Int] -> IO ()
setPrevFloatingPanels ctx rects order =
  modifyIORef' (ctxOverlayState ctx) $ \os ->
    os {osPrevFloatingRects = rects, osPrevFloatingOrder = order}

{-# INLINE getFloatingAncestor #-}
getFloatingAncestor :: Context -> IO (Maybe (IntMap (Maybe NodeType)))
getFloatingAncestor ctx = osFloatingAncestor <$> readIORef (ctxOverlayState ctx)

{-# INLINE setFloatingAncestor #-}
setFloatingAncestor :: Context -> Maybe (IntMap (Maybe NodeType)) -> IO ()
setFloatingAncestor ctx m =
  modifyIORef' (ctxOverlayState ctx) $ \os ->
    os {osFloatingAncestor = m}

-- =============================================================================
-- Animation
-- =============================================================================

{-# INLINE anyAnimating #-}
anyAnimating :: Context -> IO Bool
anyAnimating ctx = asAnyAnimating <$> readIORef (ctxAnimationState ctx)

{-# INLINE getLiveAnimations #-}
getLiveAnimations :: Context -> IO (IntMap Animation)
getLiveAnimations ctx = IM.filter animInProgress . asAnimations <$> readIORef (ctxAnimationState ctx)

{-# INLINE takeAnimSettled #-}
takeAnimSettled :: Context -> IO Bool
takeAnimSettled ctx = do
  as <- readIORef (ctxAnimationState ctx)
  if asAnimSettled as
    then do
      writeIORef (ctxAnimationState ctx) (as {asAnimSettled = False})
      pure True
    else pure False

{-# INLINE lookupAnimation #-}
lookupAnimation :: Context -> WidgetId -> IO (Maybe Animation)
lookupAnimation ctx wid = do
  as <- readIORef (ctxAnimationState ctx)
  pure (IM.lookup (intKey wid) (asAnimations as))

{-# INLINE startAnimation #-}
startAnimation :: Context -> WidgetId -> Float -> Float -> Float -> IO ()
startAnimation ctx wid start end dur = startAnimationEase ctx wid start end dur EaseLinear

{-# INLINE startAnimationEase #-}
startAnimationEase :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> IO ()
startAnimationEase ctx wid start end dur ease = startAnimationEaseDelay ctx wid start end dur ease 0

{-# INLINE startAnimationEaseDelay #-}
startAnimationEaseDelay :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> Float -> IO ()
startAnimationEaseDelay ctx wid start end dur ease delay = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  if dur <= 0 || approxEq start end
    then settleKey ctx key end
    else do
      let req = max 0 delay
          (elapsed, delayLeft) = case IM.lookup key (asAnimations as) of
            Just (EaseAnim aStart aEnd aDur aElapsed aEase aDelay aDelayReq)
              | approxEq aStart start && approxEq aEnd end && aEase == ease && approxEq aDur dur && approxEq req aDelayReq ->
                  (aElapsed, aDelay)
            _ -> (0, req)
      modifyIORef' (ctxAnimationState ctx) $ \s ->
        s
          { asAnimRest = IM.delete key (asAnimRest s)
          , asAnimations = IM.insert key (EaseAnim start end dur elapsed ease delayLeft req) (asAnimations s)
          , asAnyAnimating = True
          }
      markDirtyIfOrphan ctx key

startSpring :: Context -> WidgetId -> SpringParams -> Float -> IO ()
startSpring ctx wid params target = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  cur <- case IM.lookup key (asAnimations as) of
    Just a -> pure (animationValue a)
    Nothing -> pure (IM.findWithDefault 0 key (asAnimRest as))
  let (pos, vel) = case IM.lookup key (asAnimations as) of
        Just (SpringAnim p v _ _) -> (p, v)
        _ -> (cur, 0)
  if abs (pos - target) <= springEps && abs vel <= springEps
    then settleKey ctx key target
    else do
      modifyIORef' (ctxAnimationState ctx) $ \s ->
        s
          { asAnimRest = IM.delete key (asAnimRest s)
          , asAnimations = IM.insert key (SpringAnim pos vel target params) (asAnimations s)
          , asAnyAnimating = True
          }
      markDirtyIfOrphan ctx key

{-# INLINE setAnimationValue #-}
setAnimationValue :: Context -> WidgetId -> Float -> IO ()
setAnimationValue ctx wid val = settleKey ctx (intKey wid) val

{-# INLINE tickAnimations #-}
tickAnimations :: Context -> Float -> IO ()
tickAnimations ctx dt =
  modifyIORef' (ctxAnimationState ctx) $ \as ->
    if IM.null (asAnimations as)
      then as {asAnyAnimating = False, asAnimSettled = False}
      else
        let stepped = IM.map (stepAnim dt) (asAnimations as)
            (live, done) = IM.partition animInProgress stepped
            rest' = IM.foldlWithKey' writeRest (asAnimRest as) done
         in as
              { asAnimations = live
              , asAnimRest = rest'
              , asAnyAnimating = not (IM.null live)
              , asAnimSettled = not (IM.null done)
              }

markDirtyIfOrphan :: Context -> Int -> IO ()
markDirtyIfOrphan ctx key = do
  mprev <- getPrevRectByKey ctx key
  hasNow <- nodeHasKey ctx key
  when (isNothing mprev && not hasNow) (markDirty ctx)

nodeHasKey :: Context -> Int -> IO Bool
nodeHasKey ctx key = do
  mIdx <- lookupNodeByKey (ctxNodeArena ctx) key
  case mIdx of
    Nothing -> pure False
    Just idx -> do
      (_, _, w, h) <- getRect (ctxNodeArena ctx) idx
      pure (w > 0 && h > 0)

settleKey :: Context -> Int -> Float -> IO ()
settleKey ctx key val = do
  as <- readIORef (ctxAnimationState ctx)
  let prevRest = IM.findWithDefault 0 key (asAnimRest as)
      prevLive = fmap animationValue (IM.lookup key (asAnimations as))
      changed = case prevLive of
        Just v -> not (approxEq v val)
        Nothing -> not (approxEq prevRest val)
      anims' = IM.delete key (asAnimations as)
      rest' = if approxEq val 0 then IM.delete key (asAnimRest as) else IM.insert key val (asAnimRest as)
  writeIORef (ctxAnimationState ctx) $
    as
      { asAnimations = anims'
      , asAnimRest = rest'
      , asAnyAnimating = any animInProgress anims'
      }
  when changed $ do
    damageKey ctx key (DamageInflated defaultDamageSlop)
    markDirty ctx

{-# INLINE getAnimationValue #-}
getAnimationValue :: Context -> WidgetId -> IO Float
getAnimationValue ctx wid = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  case IM.lookup key (asAnimations as) of
    Just a -> pure (animationValue a)
    Nothing -> pure (IM.findWithDefault 0 key (asAnimRest as))
