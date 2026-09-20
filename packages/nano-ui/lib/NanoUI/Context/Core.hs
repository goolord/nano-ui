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
  , setTextInputDrag
  , getTextInputMenu
  , setTextInputMenu
  , takeTextEditLastAction
  , getPointerRoute
  , pointerHeldOffLayers
  , getWindowDrag
  , getWindowResize
  -- Damage
  , markDirty
  , clearDirty
  , isDirty
  , setWakeLoop
  , requestWakeAt
  , requestWakeAfter
  , getWakeAt
  , clearWakeAt
  , takeDamage
  , requestDamage
  , damageWidget
  , damageKey
  , damageRect
  , damagePeers
  , damageFull
  , getPrevRect
  , getPrevClipRect
  -- Store
  , getStore
  , setStore
  , modifyStore
  , writeSlots
  , writeSlot
  , adoptSlot
  , recordSlot
  , getStoreBool
  , writeStoreInt
  , writeStoreFloat
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

import Control.Monad (forM_, unless, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Primitive.SmallArray (copySmallMutableArray, newSmallArray, readSmallArray, getSizeofSmallMutableArray, writeSmallArray)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)

import NanoUI.Context.Types
  ( Context (..)
  , DamageRequest (..)
  , DamageState (..)
  , InteractionState (..)
  , PointerRoute (..)
  , OverlayState
  , TextInputDrag
  , TextInputMenu
  , ThemeScopes (..)
  , WindowResizeDrag
  , intKey
  )
import NanoUI.Id (WidgetId, hashWidgetId)
import NanoUI.Layout.Arena (DirTag, NodeIdx, getArenaScope, getNodeScope, getScopeSignature, lookupNodeByWidgetId)
import NanoUI.Store
  ( Field
  , Slot (..)
  , SlotWrites (..)
  , WidgetStore (..)
  , boolInt
  , fieldFloat
  , fieldInt
  , fieldText
  , findSlot
  , insertSlot
  , intBool
  , lookupSlot
  , ptrEq
  , slotKey
  )
import NanoUI.Style (Theme)
import NanoUI.Types (Damage, DamageBounds (..), Rect, defaultDamageSlop, rectH, rectW)
import NanoUI.Widgets.TextCommand (TextCommand)

-- =============================================================================
-- State records
-- =============================================================================

-- | Read a projection of the current pointer, drag, and text-menu state.
{-# INLINE getsInteraction #-}
getsInteraction :: Context -> (InteractionState -> a) -> IO a
getsInteraction ctx f = f <$> readIORef (ctxInteractionState ctx)

-- | Strictly update interaction state on the UI thread. Does not request repaint.
{-# INLINE modifyInteraction #-}
modifyInteraction :: Context -> (InteractionState -> InteractionState) -> IO ()
modifyInteraction ctx = modifyIORef' (ctxInteractionState ctx)

-- | Read a projection of modal and floating-panel state.
{-# INLINE getsOverlay #-}
getsOverlay :: Context -> (OverlayState -> a) -> IO a
getsOverlay ctx f = f <$> readIORef (ctxOverlayState ctx)

-- | Strictly update overlay state on the UI thread. Does not request repaint.
{-# INLINE modifyOverlay #-}
modifyOverlay :: Context -> (OverlayState -> OverlayState) -> IO ()
modifyOverlay ctx = modifyIORef' (ctxOverlayState ctx)

-- | Read a projection of pending damage and previous-frame geometry.
{-# INLINE getsDamage #-}
getsDamage :: Context -> (DamageState -> a) -> IO a
getsDamage ctx f = f <$> readIORef (ctxDamageState ctx)

-- | Strictly update damage bookkeeping without waking the event loop.
{-# INLINE modifyDamage #-}
modifyDamage :: Context -> (DamageState -> DamageState) -> IO ()
modifyDamage ctx = modifyIORef' (ctxDamageState ctx)

-- =============================================================================
-- Interaction
-- =============================================================================

-- | Active scrollbar drag: widget, axis, and grab offset, or 'Nothing'.
{-# INLINE getScrollDrag #-}
getScrollDrag :: Context -> IO (Maybe (WidgetId, DirTag, Float))
getScrollDrag ctx = getsInteraction ctx isScrollDrag

-- | Replace or clear the active text-selection drag.
{-# INLINE setTextInputDrag #-}
setTextInputDrag :: Context -> Maybe TextInputDrag -> IO ()
setTextInputDrag ctx v = modifyInteraction ctx (\s -> s {isTextInputDrag = v})

-- | Open text-edit context menu, or 'Nothing'.
{-# INLINE getTextInputMenu #-}
getTextInputMenu :: Context -> IO (Maybe TextInputMenu)
getTextInputMenu ctx = getsInteraction ctx isTextInputMenu

-- | Replace or close the text-edit context menu. The caller handles damage.
{-# INLINE setTextInputMenu #-}
setTextInputMenu :: Context -> Maybe TextInputMenu -> IO ()
setTextInputMenu ctx v = modifyInteraction ctx (\s -> s {isTextInputMenu = v})

-- | Read and clear the last command run by a text-edit menu, with its target id.
takeTextEditLastAction :: Context -> IO (Maybe (WidgetId, TextCommand))
takeTextEditLastAction ctx = do
  act <- getsInteraction ctx isTextEditLastAction
  modifyInteraction ctx (\s -> s {isTextEditLastAction = Nothing})
  pure act

-- | Pointer destination chosen before the current view pass.
{-# INLINE getPointerRoute #-}
getPointerRoute :: Context -> IO PointerRoute
getPointerRoute ctx = getsInteraction ctx isPointerRoute

-- | Whether a held button went down on a menu or dropdown rather than on a
-- layer's widgets. Nothing in the layers is hot while it lasts.
{-# INLINE pointerHeldOffLayers #-}
pointerHeldOffLayers :: Context -> IO Bool
pointerHeldOffLayers ctx =
  getsInteraction ctx $ \s ->
    isPointerHeld s && case isPointerRoute s of
      RouteLayer _ -> False
      _ -> True

-- | Dragged window id and pointer-to-window x/y offsets, or 'Nothing'.
{-# INLINE getWindowDrag #-}
getWindowDrag :: Context -> IO (Maybe (WidgetId, Float, Float))
getWindowDrag ctx = getsInteraction ctx isWindowDrag

-- | Active window resize gesture, including starting bounds and size limits.
{-# INLINE getWindowResize #-}
getWindowResize :: Context -> IO (Maybe WindowResizeDrag)
getWindowResize ctx = getsInteraction ctx isWindowResize

-- =============================================================================
-- Damage
-- =============================================================================

-- | Queue repaint bounds for the frame's damage pass. Does not wake the loop;
-- call 'markDirty' as well when a new frame must be scheduled.
{-# INLINE requestDamage #-}
requestDamage :: Context -> DamageRequest -> IO ()
requestDamage ctx req = modifyDamage ctx (\ds -> ds {dsRequests = req : dsRequests ds})

-- | Queue damage relative to a widget's previous and current bounds. Zero ids
-- are ignored. Does not itself schedule a frame.
{-# INLINE damageWidget #-}
damageWidget :: Context -> WidgetId -> DamageBounds -> IO ()
damageWidget ctx wid bounds
  | hashWidgetId wid == 0 = pure ()
  | otherwise = requestDamage ctx (ReqWidget wid bounds)

-- | Queue widget damage by integer store key. Zero keys are ignored.
{-# INLINE damageKey #-}
damageKey :: Context -> Int -> DamageBounds -> IO ()
damageKey ctx k bounds
  | k == 0 = pure ()
  | otherwise = requestDamage ctx (ReqKey k bounds)

-- | Queue an explicit logical window rectangle. Empty rectangles are ignored.
{-# INLINE damageRect #-}
damageRect :: Context -> Rect -> IO ()
damageRect ctx r
  | rectW r <= 0 || rectH r <= 0 = pure ()
  | otherwise = requestDamage ctx (ReqRect r)

-- | Queue the same damage rule for a group of nonzero widget ids.
{-# INLINE damagePeers #-}
damagePeers :: Context -> [WidgetId] -> DamageBounds -> IO ()
damagePeers ctx wids bounds =
  case filter (\w -> hashWidgetId w /= 0) wids of
    [] -> pure ()
    valid -> requestDamage ctx (ReqPeers valid bounds)

-- | Queue a whole-window repaint. Call 'markDirty' if the loop also needs waking.
{-# INLINE damageFull #-}
damageFull :: Context -> IO ()
damageFull ctx = requestDamage ctx ReqFull

-- | Request another view pass and invoke the installed event-loop wake action.
-- Dirty state schedules work; damage determines which pixels are repainted.
{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = do
  modifyDamage ctx (\ds -> ds {dsDirty = True})
  readIORef (ctxWakeLoop ctx) >>= sequence_

-- | Clear the follow-up-frame request without clearing queued repaint bounds.
{-# INLINE clearDirty #-}
clearDirty :: Context -> IO ()
clearDirty ctx = modifyDamage ctx (\ds -> ds {dsDirty = False})

-- | Whether state changes require another view pass.
{-# INLINE isDirty #-}
isDirty :: Context -> IO Bool
isDirty ctx = getsDamage ctx dsDirty

-- | Install the backend action that interrupts an event wait. A background
-- producer should invoke the wake action after publishing synchronised data.
{-# INLINE setWakeLoop #-}
setWakeLoop :: Context -> IO () -> IO ()
setWakeLoop ctx wake = writeIORef (ctxWakeLoop ctx) (Just wake)

-- | Ask for a frame at a monotonic time ('getMonotonicTime') even if no input
-- arrives. The earliest request wins. Each frame starts with none pending, so
-- a widget that still needs a later frame asks again as it is built; one that
-- is gone stops asking, and the loop sleeps. Call it from the UI thread: a
-- background thread wakes the loop through 'ctxWakeLoop' instead.
requestWakeAt :: Context -> Double -> IO ()
requestWakeAt ctx t = do
  cur <- readIORef (ctxWakeAt ctx)
  when (t > 0 && (cur <= 0 || t < cur)) $ writeIORef (ctxWakeAt ctx) t

-- | 'requestWakeAt', in seconds from now.
requestWakeAfter :: Context -> Double -> IO ()
requestWakeAfter ctx sec = do
  now <- getMonotonicTime
  requestWakeAt ctx (now + max 0 sec)

-- | The pending wake time, or 0 when nothing asked for one.
{-# INLINE getWakeAt #-}
getWakeAt :: Context -> IO Double
getWakeAt ctx = readIORef (ctxWakeAt ctx)

-- | Clear the timed wake request at frame start; live widgets request it again.
{-# INLINE clearWakeAt #-}
clearWakeAt :: Context -> IO ()
clearWakeAt ctx = writeIORef (ctxWakeAt ctx) 0

-- | Read the last computed damage. Despite the name, this does not clear it.
{-# INLINE takeDamage #-}
takeDamage :: Context -> IO Damage
takeDamage ctx = getsDamage ctx dsDamage

-- | Last recorded widget bounds in logical window coordinates, with scrolling
-- applied. 'Nothing' means the damage pass recorded no bounds for this id.
{-# INLINE getPrevRect #-}
getPrevRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevRect ctx wid = getsDamage ctx (IM.lookup (intKey wid) . dsPrevRects)

-- | Last recorded widget clip in logical window coordinates, or 'Nothing'.
{-# INLINE getPrevClipRect #-}
getPrevClipRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevClipRect ctx wid = getsDamage ctx (IM.lookup (intKey wid) . dsPrevClips)

-- =============================================================================
-- Store
-- =============================================================================

-- | Current immutable store value. Mutations must be published through store
-- operations so the context can track damage and follow-up frames.
{-# INLINE getStore #-}
getStore :: Context -> IO WidgetStore
getStore ctx = readIORef (ctxStore ctx)

-- | Replace the store, diff changed slots for damage, and wake on a change.
setStore :: Context -> WidgetStore -> IO ()
setStore ctx store = modifyStore ctx (const store)

-- | Replace the store with @f@ of it, damaging the keys whose values changed
-- and waking the loop when anything did.
modifyStore :: Context -> (WidgetStore -> WidgetStore) -> IO ()
modifyStore ctx f = do
  prev <- readIORef (ctxStore ctx)
  -- WHNF-force the new record: record-update arguments are unevaluated
  -- thunks, and writeIORef would otherwise park one in the long-lived store
  -- every frame.
  let !store = f prev
  writeIORef (ctxStore ctx) store
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
  -- first would walk every changed map twice. Its lazy concatenation stops at
  -- the first changed key and allocates less than a list per map.
  when
    ( storeMirrorGen prev /= storeMirrorGen store
        || storeOpenSelect prev /= storeOpenSelect store
        || not (null changedKeys)
    )
    $ do
      forM_ changedKeys $ \k -> damageKey ctx k (DamageInflated defaultDamageSlop)
      markDirty ctx

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

-- | Run slot writes, unless every slot already holds its value: an idle
-- widget then neither rebuilds the store nor has it diffed.
{-# INLINE writeSlots #-}
writeSlots :: Context -> SlotWrites -> IO ()
writeSlots ctx (SlotWrites same f) = do
  st <- readIORef (ctxStore ctx)
  unless (same st) (setStore ctx (f st))

-- | Targeted single-slot write: compares only the target slot, updates one map
-- field, damages the owning widget and wakes the loop. Unlike 'setStore' it
-- never diffs the whole store, and an equal write is a no-op.
{-# INLINE writeSlot #-}
writeSlot :: Eq a => Field a -> Context -> WidgetId -> Int -> a -> IO ()
writeSlot field ctx owner k v = do
  st <- readIORef (ctxStore ctx)
  case lookupSlot field k st of
    Just old | old == v -> pure ()
    _ -> do
      writeIORef (ctxStore ctx) $! insertSlot field k v st
      damageWidget ctx owner DamageSelf
      markDirty ctx

-- | Write an integer slot by key, damaging its owning widget only when changed.
writeStoreInt :: Context -> WidgetId -> Int -> Int -> IO ()
writeStoreInt = writeSlot fieldInt

-- | Write a float slot by key, damaging its owning widget only when changed.
writeStoreFloat :: Context -> WidgetId -> Int -> Float -> IO ()
writeStoreFloat = writeSlot fieldFloat

-- | Write a boolean at the owner's base integer key, with change detection.
{-# INLINE writeStoreBool #-}
writeStoreBool :: Context -> WidgetId -> Bool -> IO ()
writeStoreBool ctx owner v = writeStoreInt ctx owner (intKey owner) (boolInt v)

-- | Controlled widgets take their value from the caller every frame. The
-- caller's value replaces the stored one only when it differs from the value
-- the widget last returned ('recordSlot'). An edit applied between frames,
-- such as a menu cut, then survives a caller that passes the previous result
-- back, while a value changed by the application still wins. Returns the
-- slot's value after adopting.
{-# INLINE adoptSlot #-}
adoptSlot :: Eq a => Field a -> Context -> WidgetId -> Int -> a -> IO a
adoptSlot field ctx owner k v = do
  st <- readIORef (ctxStore ctx)
  let seenK = slotKey SlotSeen k
  if lookupSlot field seenK st == Just v
    then pure $! findSlot field v k st
    else do
      writeIORef (ctxStore ctx) $! insertSlot field seenK v (insertSlot field k v st)
      when (lookupSlot field k st /= Just v) $ do
        damageWidget ctx owner DamageSelf
        markDirty ctx
      pure v

-- | Remember the value a controlled widget returned this frame.
{-# INLINE recordSlot #-}
recordSlot :: Eq a => Field a -> Context -> Int -> a -> IO ()
recordSlot field ctx k v = do
  st <- readIORef (ctxStore ctx)
  let seenK = slotKey SlotSeen k
  when (lookupSlot field seenK st /= Just v) $
    writeIORef (ctxStore ctx) $! insertSlot field seenK v st

-- | Adopt a controlled integer value. A value different from the last
-- 'recordStoreInt' result wins; otherwise retain edits made between frames.
adoptStoreInt :: Context -> WidgetId -> Int -> Int -> IO Int
adoptStoreInt = adoptSlot fieldInt

-- | Float form of 'adoptStoreInt', paired with 'recordStoreFloat'.
adoptStoreFloat :: Context -> WidgetId -> Int -> Float -> IO Float
adoptStoreFloat = adoptSlot fieldFloat

-- | Text form of 'adoptStoreInt', paired with 'recordStoreText'.
adoptStoreText :: Context -> WidgetId -> Int -> Text -> IO Text
adoptStoreText = adoptSlot fieldText

-- | Remember a controlled widget's returned integer for next frame's adoption.
-- Does not request a frame or modify the widget's value slot.
recordStoreInt :: Context -> Int -> Int -> IO ()
recordStoreInt = recordSlot fieldInt

-- | Remember the returned float for 'adoptStoreFloat'.
recordStoreFloat :: Context -> Int -> Float -> IO ()
recordStoreFloat = recordSlot fieldFloat

-- | Remember the returned text for 'adoptStoreText'.
recordStoreText :: Context -> Int -> Text -> IO ()
recordStoreText = recordSlot fieldText

-- | Read the boolean at a widget's base integer key, using the supplied default.
{-# INLINE getStoreBool #-}
getStoreBool :: Context -> WidgetId -> Bool -> IO Bool
getStoreBool ctx wid def =
  intBool . findSlot fieldInt (boolInt def) (intKey wid) <$> getStore ctx

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

-- | Allocate empty current/previous scope arrays. Only entries below their
-- recorded counts are initialised and may be read.
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

-- | Add a scope with disabled status, raw theme, and painted theme, in that
-- order. Returns a one-based theme index and records whether the painted
-- theme differs from the previous frame's theme at that index.
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

-- | Painted theme for a packed arena scope. Theme index zero uses the base
-- theme; other indices must name a scope registered during this view pass.
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

-- | Painted theme recorded on a live node, including disabled styling.
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
