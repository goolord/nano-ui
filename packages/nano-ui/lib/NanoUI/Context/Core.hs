-- | Accessors the other Context modules build on: interaction, overlay and
-- damage state, damage requests, the dirty flag, and widget store writes.
module NanoUI.Context.Core
  ( getsInteraction
  , modifyInteraction
  , getsOverlay
  , modifyOverlay
  , getsDamage
  , modifyDamage
  -- Interaction
  , getScrollDrag
  , setScrollDrag
  , getTextInputDrag
  , setTextInputDrag
  , getTextFieldClickCell
  , setTextFieldClickCell
  , getTextInputMenu
  , setTextInputMenu
  , setTextEditLastAction
  , takeTextEditLastAction
  , getSelectDropPress
  , setSelectDropPress
  , setOpenSelectDrop
  , getOpenSelectDrop
  , getMenuPointerGesture
  , setMenuPointerGesture
  , getWindowDrag
  , setWindowDrag
  , getWindowResize
  , setWindowResize
  -- Damage
  , markDirty
  , clearDirty
  , isDirty
  , setWakeLoop
  , takeDamage
  , getLastWindowSize
  , setDamageAndWindowSize
  , requestDamage
  , damageWidget
  , damageKey
  , damageRect
  , damagePeers
  , damageFull
  , getDamageRequests
  , getPrevRect
  , getPrevClipRect
  , getPrevRects
  , getPrevClips
  , setPrevRectsAndClips
  , getPrevNodeTexts
  , setPrevNodeTexts
  -- Store
  , getStore
  , setStore
  , deleteWidgetStore
  , getStoreBool
  , setStoreBool
  , writeStoreInt
  , writeStoreFloat
  , writeStoreText
  , writeStoreBool
  , adoptStoreInt
  , adoptStoreFloat
  , adoptStoreText
  , recordStoreInt
  , recordStoreFloat
  , recordStoreText
  , isDisabled
  -- Theme scopes
  , newThemeScopes
  , beginThemeScopes
  , pushThemeScope
  , themeScopesChanged
  , scopeTheme
  , scopeRawTheme
  , currentTheme
  , nodeTheme
  , widgetTheme
  ) where

import Control.Monad (forM_, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Primitive.SmallArray (copySmallMutableArray, newSmallArray, readSmallArray, getSizeofSmallMutableArray, writeSmallArray)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)

import NanoUI.Context.Types
  ( Context (..)
  , DamageRequest (..)
  , DamageState (..)
  , InteractionState (..)
  , OverlayState
  , TextFieldClickCell
  , TextInputDrag
  , TextInputMenu
  , ThemeScopes (..)
  , WindowResizeDrag
  , intKey
  )
import NanoUI.Id (WidgetId, hashWidgetId)
import NanoUI.Layout.Arena (DirTag, NodeIdx, getArenaScope, getNodeScope, getScopeSignature, lookupNodeByWidgetId)
import NanoUI.Store
  ( WidgetStore (..)
  , boolInt
  , deleteWidgetState
  , intBool
  , ptrEq
  , slotKey
  , slotSeen
  )
import NanoUI.Style (Theme)
import NanoUI.Types (Damage, DamageBounds (..), Rect, Size, defaultDamageSlop, rectH, rectW)
import NanoUI.Widgets.TextCommand (TextCommand)

-- =============================================================================
-- State records
-- =============================================================================

{-# INLINE getsInteraction #-}
getsInteraction :: Context -> (InteractionState -> a) -> IO a
getsInteraction ctx f = f <$> readIORef (ctxInteractionState ctx)

{-# INLINE modifyInteraction #-}
modifyInteraction :: Context -> (InteractionState -> InteractionState) -> IO ()
modifyInteraction ctx = modifyIORef' (ctxInteractionState ctx)

{-# INLINE getsOverlay #-}
getsOverlay :: Context -> (OverlayState -> a) -> IO a
getsOverlay ctx f = f <$> readIORef (ctxOverlayState ctx)

{-# INLINE modifyOverlay #-}
modifyOverlay :: Context -> (OverlayState -> OverlayState) -> IO ()
modifyOverlay ctx = modifyIORef' (ctxOverlayState ctx)

{-# INLINE getsDamage #-}
getsDamage :: Context -> (DamageState -> a) -> IO a
getsDamage ctx f = f <$> readIORef (ctxDamageState ctx)

{-# INLINE modifyDamage #-}
modifyDamage :: Context -> (DamageState -> DamageState) -> IO ()
modifyDamage ctx = modifyIORef' (ctxDamageState ctx)

-- =============================================================================
-- Interaction
-- =============================================================================

{-# INLINE getScrollDrag #-}
getScrollDrag :: Context -> IO (Maybe (WidgetId, DirTag, Float))
getScrollDrag ctx = getsInteraction ctx isScrollDrag

{-# INLINE setScrollDrag #-}
setScrollDrag :: Context -> Maybe (WidgetId, DirTag, Float) -> IO ()
setScrollDrag ctx v = modifyInteraction ctx (\s -> s {isScrollDrag = v})

{-# INLINE getTextInputDrag #-}
getTextInputDrag :: Context -> IO (Maybe TextInputDrag)
getTextInputDrag ctx = getsInteraction ctx isTextInputDrag

{-# INLINE setTextInputDrag #-}
setTextInputDrag :: Context -> Maybe TextInputDrag -> IO ()
setTextInputDrag ctx v = modifyInteraction ctx (\s -> s {isTextInputDrag = v})

{-# INLINE getTextFieldClickCell #-}
getTextFieldClickCell :: Context -> IO (Maybe TextFieldClickCell)
getTextFieldClickCell ctx = getsInteraction ctx isTextFieldClickCell

{-# INLINE setTextFieldClickCell #-}
setTextFieldClickCell :: Context -> Maybe TextFieldClickCell -> IO ()
setTextFieldClickCell ctx v = modifyInteraction ctx (\s -> s {isTextFieldClickCell = v})

{-# INLINE getTextInputMenu #-}
getTextInputMenu :: Context -> IO (Maybe TextInputMenu)
getTextInputMenu ctx = getsInteraction ctx isTextInputMenu

{-# INLINE setTextInputMenu #-}
setTextInputMenu :: Context -> Maybe TextInputMenu -> IO ()
setTextInputMenu ctx v = modifyInteraction ctx (\s -> s {isTextInputMenu = v})

{-# INLINE setTextEditLastAction #-}
setTextEditLastAction :: Context -> Maybe (WidgetId, TextCommand) -> IO ()
setTextEditLastAction ctx v = modifyInteraction ctx (\s -> s {isTextEditLastAction = v})

takeTextEditLastAction :: Context -> IO (Maybe (WidgetId, TextCommand))
takeTextEditLastAction ctx = do
  act <- getsInteraction ctx isTextEditLastAction
  setTextEditLastAction ctx Nothing
  pure act

{-# INLINE getSelectDropPress #-}
getSelectDropPress :: Context -> IO Bool
getSelectDropPress ctx = getsInteraction ctx isSelectDropPress

{-# INLINE setSelectDropPress #-}
setSelectDropPress :: Context -> Bool -> IO ()
setSelectDropPress ctx v = modifyInteraction ctx (\s -> s {isSelectDropPress = v})

{-# INLINE getOpenSelectDrop #-}
getOpenSelectDrop :: Context -> IO (Maybe (WidgetId, Rect))
getOpenSelectDrop ctx = getsInteraction ctx isOpenSelectDrop

{-# INLINE setOpenSelectDrop #-}
setOpenSelectDrop :: Context -> Maybe (WidgetId, Rect) -> IO ()
setOpenSelectDrop ctx v = modifyInteraction ctx (\s -> s {isOpenSelectDrop = v})

{-# INLINE getMenuPointerGesture #-}
getMenuPointerGesture :: Context -> IO Bool
getMenuPointerGesture ctx = getsInteraction ctx isMenuPointerGesture

{-# INLINE setMenuPointerGesture #-}
setMenuPointerGesture :: Context -> Bool -> IO ()
setMenuPointerGesture ctx v = modifyInteraction ctx (\s -> s {isMenuPointerGesture = v})

{-# INLINE getWindowDrag #-}
getWindowDrag :: Context -> IO (Maybe (WidgetId, Float, Float))
getWindowDrag ctx = getsInteraction ctx isWindowDrag

{-# INLINE setWindowDrag #-}
setWindowDrag :: Context -> Maybe (WidgetId, Float, Float) -> IO ()
setWindowDrag ctx v = modifyInteraction ctx (\s -> s {isWindowDrag = v})

{-# INLINE getWindowResize #-}
getWindowResize :: Context -> IO (Maybe WindowResizeDrag)
getWindowResize ctx = getsInteraction ctx isWindowResize

{-# INLINE setWindowResize #-}
setWindowResize :: Context -> Maybe WindowResizeDrag -> IO ()
setWindowResize ctx v = modifyInteraction ctx (\s -> s {isWindowResize = v})

-- =============================================================================
-- Damage
-- =============================================================================

{-# INLINE requestDamage #-}
requestDamage :: Context -> DamageRequest -> IO ()
requestDamage ctx req = modifyDamage ctx (\ds -> ds {dsRequests = req : dsRequests ds})

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

{-# INLINE getDamageRequests #-}
getDamageRequests :: Context -> IO [DamageRequest]
getDamageRequests ctx = getsDamage ctx dsRequests

{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = do
  modifyDamage ctx (\ds -> ds {dsDirty = True})
  readIORef (ctxWakeLoop ctx) >>= sequence_

{-# INLINE clearDirty #-}
clearDirty :: Context -> IO ()
clearDirty ctx = modifyDamage ctx (\ds -> ds {dsDirty = False})

{-# INLINE isDirty #-}
isDirty :: Context -> IO Bool
isDirty ctx = getsDamage ctx dsDirty

{-# INLINE setWakeLoop #-}
setWakeLoop :: Context -> IO () -> IO ()
setWakeLoop ctx wake = writeIORef (ctxWakeLoop ctx) (Just wake)

{-# INLINE takeDamage #-}
takeDamage :: Context -> IO Damage
takeDamage ctx = getsDamage ctx dsDamage

{-# INLINE getLastWindowSize #-}
getLastWindowSize :: Context -> IO Size
getLastWindowSize ctx = getsDamage ctx dsLastWindowSize

{-# INLINE setDamageAndWindowSize #-}
setDamageAndWindowSize :: Context -> Damage -> Size -> IO ()
setDamageAndWindowSize ctx dmg sz =
  modifyDamage ctx (\ds -> ds {dsDamage = dmg, dsLastWindowSize = sz, dsRequests = []})

{-# INLINE getPrevRects #-}
getPrevRects :: Context -> IO (IntMap Rect)
getPrevRects ctx = getsDamage ctx dsPrevRects

{-# INLINE getPrevClips #-}
getPrevClips :: Context -> IO (IntMap Rect)
getPrevClips ctx = getsDamage ctx dsPrevClips

{-# INLINE setPrevRectsAndClips #-}
setPrevRectsAndClips :: Context -> IntMap Rect -> IntMap Rect -> IO ()
setPrevRectsAndClips ctx rects clips =
  modifyDamage ctx (\ds -> ds {dsPrevRects = rects, dsPrevClips = clips})

{-# INLINE getPrevNodeTexts #-}
getPrevNodeTexts :: Context -> IO (IntMap Text)
getPrevNodeTexts ctx = getsDamage ctx dsPrevNodeTexts

{-# INLINE setPrevNodeTexts #-}
setPrevNodeTexts :: Context -> IntMap Text -> IO ()
setPrevNodeTexts ctx texts = modifyDamage ctx (\ds -> ds {dsPrevNodeTexts = texts})

{-# INLINE getPrevRect #-}
getPrevRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevRect ctx wid = getsDamage ctx (IM.lookup (intKey wid) . dsPrevRects)

{-# INLINE getPrevClipRect #-}
getPrevClipRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevClipRect ctx wid = getsDamage ctx (IM.lookup (intKey wid) . dsPrevClips)

-- =============================================================================
-- Store
-- =============================================================================

{-# INLINE getStore #-}
getStore :: Context -> IO WidgetStore
getStore ctx = readIORef (ctxStore ctx)

setStore :: Context -> WidgetStore -> IO ()
setStore ctx store = do
  prev <- readIORef (ctxStore ctx)
  -- WHNF-force the incoming record: record-update arguments are unevaluated
  -- thunks, and writeIORef would otherwise park one in the long-lived store
  -- every frame.
  writeIORef (ctxStore ctx) $! store
  let changedKeys =
        diffKeys (storeInt prev) (storeInt store)
          ++ diffKeys (storeFloat prev) (storeFloat store)
          ++ diffKeys (storeDouble prev) (storeDouble store)
          ++ diffKeys (storePoint prev) (storePoint store)
          ++ diffKeys (storeText prev) (storeText store)
          ++ diffKeys (storeFloatList prev) (storeFloatList store)
          ++ diffKeys (storeIntList prev) (storeIntList store)
          ++ diffKeys (storeIntSet prev) (storeIntSet store)
          ++ diffKeysBy ptrEq (storeDyn prev) (storeDyn store)
  -- The key diff doubles as the store comparison: checking 'prev /= store'
  -- first would walk every changed map twice.
  when
    ( storeMirrorGen prev /= storeMirrorGen store
        || storeOpenSelect prev /= storeOpenSelect store
        || not (null changedKeys)
    )
    $ do
      forM_ changedKeys $ \k -> damageKey ctx k (DamageInflated defaultDamageSlop)
      markDirty ctx

deleteWidgetStore :: Context -> WidgetId -> IO ()
deleteWidgetStore ctx wid = do
  st <- getStore ctx
  setStore ctx (deleteWidgetState wid st)

diffKeysBy :: (a -> a -> Bool) -> IntMap a -> IntMap a -> [Int]
diffKeysBy eq old new
  -- Unchanged maps keep their identity through a record update; skip the
  -- whole merge when the caller only rebuilt a different field.
  | ptrEq old new = []
  | otherwise =
      IM.keys
        ( IM.mergeWithKey
            (\_ a b -> if eq a b then Nothing else Just ())
            (IM.map (const ()))
            (IM.map (const ()))
            old
            new
        )

diffKeys :: Eq a => IntMap a -> IntMap a -> [Int]
diffKeys = diffKeysBy (==)

-- | Targeted single-slot write: compares only the target slot, updates one map
-- field, damages the owning widget and wakes the loop. Unlike 'setStore' it
-- never diffs the whole store, and an equal write is a no-op.
{-# INLINE writeSlot #-}
writeSlot ::
  Eq a =>
  (WidgetStore -> IntMap a) ->
  (IntMap a -> WidgetStore -> WidgetStore) ->
  Context ->
  WidgetId ->
  Int ->
  a ->
  IO ()
writeSlot field setField ctx owner k v = do
  st <- readIORef (ctxStore ctx)
  case IM.lookup k (field st) of
    Just old | old == v -> pure ()
    _ -> do
      writeIORef (ctxStore ctx) $! setField (IM.insert k v (field st)) st
      damageWidget ctx owner DamageSelf
      markDirty ctx

writeStoreInt :: Context -> WidgetId -> Int -> Int -> IO ()
writeStoreInt = writeSlot storeInt (\m st -> st {storeInt = m})

writeStoreFloat :: Context -> WidgetId -> Int -> Float -> IO ()
writeStoreFloat = writeSlot storeFloat (\m st -> st {storeFloat = m})

writeStoreText :: Context -> WidgetId -> Int -> Text -> IO ()
writeStoreText = writeSlot storeText (\m st -> st {storeText = m})

{-# INLINE writeStoreBool #-}
writeStoreBool :: Context -> WidgetId -> Bool -> IO ()
writeStoreBool ctx owner v = writeStoreInt ctx owner (intKey owner) (boolInt v)

-- | Controlled widgets take their value from the caller every frame. The
-- caller's value replaces the stored one only when it differs from the value
-- the widget last returned ('recordSlot'). An edit applied between frames,
-- such as a menu cut, then survives a caller that passes the previous result
-- back, while a value changed by the application still wins.
{-# INLINE adoptSlot #-}
adoptSlot ::
  Eq a =>
  (WidgetStore -> IntMap a) ->
  (IntMap a -> WidgetStore -> WidgetStore) ->
  Context ->
  WidgetId ->
  Int ->
  a ->
  IO ()
adoptSlot field setField ctx owner k v = do
  st <- readIORef (ctxStore ctx)
  let
    m = field st
    seenK = slotKey slotSeen k
  when (IM.lookup seenK m /= Just v) $ do
    writeIORef (ctxStore ctx) $! setField (IM.insert seenK v (IM.insert k v m)) st
    when (IM.lookup k m /= Just v) $ do
      damageWidget ctx owner DamageSelf
      markDirty ctx

-- | Remember the value a controlled widget returned this frame.
{-# INLINE recordSlot #-}
recordSlot ::
  Eq a =>
  (WidgetStore -> IntMap a) ->
  (IntMap a -> WidgetStore -> WidgetStore) ->
  Context ->
  Int ->
  a ->
  IO ()
recordSlot field setField ctx k v = do
  st <- readIORef (ctxStore ctx)
  let seenK = slotKey slotSeen k
  when (IM.lookup seenK (field st) /= Just v) $
    writeIORef (ctxStore ctx) $! setField (IM.insert seenK v (field st)) st

adoptStoreInt :: Context -> WidgetId -> Int -> Int -> IO ()
adoptStoreInt = adoptSlot storeInt (\m st -> st {storeInt = m})

adoptStoreFloat :: Context -> WidgetId -> Int -> Float -> IO ()
adoptStoreFloat = adoptSlot storeFloat (\m st -> st {storeFloat = m})

adoptStoreText :: Context -> WidgetId -> Int -> Text -> IO ()
adoptStoreText = adoptSlot storeText (\m st -> st {storeText = m})

recordStoreInt :: Context -> Int -> Int -> IO ()
recordStoreInt = recordSlot storeInt (\m st -> st {storeInt = m})

recordStoreFloat :: Context -> Int -> Float -> IO ()
recordStoreFloat = recordSlot storeFloat (\m st -> st {storeFloat = m})

recordStoreText :: Context -> Int -> Text -> IO ()
recordStoreText = recordSlot storeText (\m st -> st {storeText = m})

{-# INLINE getStoreBool #-}
getStoreBool :: Context -> WidgetId -> Bool -> IO Bool
getStoreBool ctx wid def =
  intBool . IM.findWithDefault (boolInt def) (intKey wid) . storeInt <$> getStore ctx

-- | Same as 'writeStoreBool': an equal write is a no-op.
{-# INLINE setStoreBool #-}
setStoreBool :: Context -> WidgetId -> Bool -> IO ()
setStoreBool = writeStoreBool

-- | Whether @wid@ was declared inside a disabled scope. A widget asks before
-- its node exists, while the scope it is declared in is still the arena's.
{-# INLINE isDisabled #-}
isDisabled :: Context -> WidgetId -> IO Bool
isDisabled ctx wid = do
  ts <- readIORef (ctxThemeScopes ctx)
  if tsDisabled ts then scopeDisabled ctx wid else pure False

{-# NOINLINE scopeDisabled #-}
scopeDisabled :: Context -> WidgetId -> IO Bool
scopeDisabled ctx wid = do
  let na = ctxNodeArena ctx
  mIdx <- lookupNodeByWidgetId na wid
  scope <- maybe (getArenaScope na) (getNodeScope na) mIdx
  pure (scope .&. 1 /= 0)

-- =============================================================================
-- Theme scopes
-- =============================================================================

newThemeScopes :: IO ThemeScopes
newThemeScopes = do
  let unset = error "theme scope: unset"
  cur <- newSmallArray 8 unset
  raw <- newSmallArray 8 unset
  prev <- newSmallArray 8 unset
  prevRaw <- newSmallArray 8 unset
  pure
    ThemeScopes
      { tsCount = 0
      , tsThemes = cur
      , tsRaw = raw
      , tsPrevCount = 0
      , tsPrev = prev
      , tsPrevRaw = prevRaw
      , tsDisabled = False
      , tsChanged = False
      , tsPrevSig = 0
      }

-- | Start a view pass with no pushed themes. The first pass of a frame keeps
-- last frame's themes to compare against; a rebuild pass keeps comparing
-- against the same ones.
beginThemeScopes :: Context -> Bool -> IO ()
beginThemeScopes ctx newFrame = do
  ts <- readIORef (ctxThemeScopes ctx)
  if newFrame
    then do
      sig <- getScopeSignature (ctxNodeArena ctx)
      writeIORef (ctxThemeScopes ctx) $!
        ts
          { tsCount = 0
          , tsThemes = tsPrev ts
          , tsRaw = tsPrevRaw ts
          , tsPrevCount = tsCount ts
          , tsPrev = tsThemes ts
          , tsPrevRaw = tsRaw ts
          , tsDisabled = False
          , tsChanged = False
          , tsPrevSig = sig
          }
    else writeIORef (ctxThemeScopes ctx) $! ts {tsCount = 0, tsDisabled = False, tsChanged = False}

-- | Add a scope drawn with @theme@, whose nested scopes modify @raw@ and which
-- is disabled or not, and
-- return its theme index. A theme equal to last frame's at the same index
-- keeps last frame's value.
pushThemeScope :: Context -> Bool -> Theme -> Theme -> IO Int
pushThemeScope ctx disabled raw theme = do
  ts <- readIORef (ctxThemeScopes ctx)
  let !i = tsCount ts
  cap <- getSizeofSmallMutableArray (tsThemes ts)
  (themes, raws) <-
    if i < cap
      then pure (tsThemes ts, tsRaw ts)
      else do
        grown <- newSmallArray (cap * 2) theme
        copySmallMutableArray grown 0 (tsThemes ts) 0 i
        grownRaw <- newSmallArray (cap * 2) raw
        copySmallMutableArray grownRaw 0 (tsRaw ts) 0 i
        pure (grown, grownRaw)
  same <-
    if i < tsPrevCount ts
      then (== theme) <$> readSmallArray (tsPrev ts) i
      else pure False
  writeSmallArray themes i theme
  writeSmallArray raws i raw
  writeIORef (ctxThemeScopes ctx) $!
    ts {tsCount = i + 1, tsThemes = themes, tsRaw = raws, tsDisabled = tsDisabled ts || disabled, tsChanged = tsChanged ts || not same}
  pure (i + 1)

-- | Whether this frame's scopes look different from last frame's: a theme
-- changed, scopes were added or dropped, or nodes moved between scopes.
themeScopesChanged :: Context -> IO Bool
themeScopesChanged ctx = do
  ts <- readIORef (ctxThemeScopes ctx)
  sig <- getScopeSignature (ctxNodeArena ctx)
  pure (tsChanged ts || tsCount ts /= tsPrevCount ts || sig /= tsPrevSig ts)

{-# INLINE scopeTheme #-}
scopeTheme :: Context -> Int -> IO Theme
scopeTheme ctx scope
  | ti == 0 = readIORef (ctxTheme ctx)
  | otherwise = do
      ts <- readIORef (ctxThemeScopes ctx)
      readSmallArray (tsThemes ts) (ti - 1)
  where
    !ti = scope `shiftR` 1

-- | A scope's theme before any disabled scope faded it.
scopeRawTheme :: Context -> Int -> IO Theme
scopeRawTheme ctx scope
  | ti == 0 = readIORef (ctxTheme ctx)
  | otherwise = do
      ts <- readIORef (ctxThemeScopes ctx)
      readSmallArray (tsRaw ts) (ti - 1)
  where
    !ti = scope `shiftR` 1

-- | The theme of the scope the view is declaring in.
{-# INLINE currentTheme #-}
currentTheme :: Context -> IO Theme
currentTheme ctx = getArenaScope (ctxNodeArena ctx) >>= scopeTheme ctx

{-# INLINE nodeTheme #-}
nodeTheme :: Context -> NodeIdx -> IO Theme
nodeTheme ctx idx = getNodeScope (ctxNodeArena ctx) idx >>= scopeTheme ctx

-- | The theme of @wid@'s node, or of the current scope before it has one.
widgetTheme :: Context -> WidgetId -> IO Theme
widgetTheme ctx wid = do
  ts <- readIORef (ctxThemeScopes ctx)
  if tsCount ts == 0
    then readIORef (ctxTheme ctx)
    else lookupNodeByWidgetId (ctxNodeArena ctx) wid >>= maybe (currentTheme ctx) (nodeTheme ctx)
