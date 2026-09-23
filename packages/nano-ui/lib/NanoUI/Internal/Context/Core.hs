-- | Accessors the other Context modules build on: interaction, overlay and
-- damage state, damage requests, the dirty flag, and widget store writes.
module NanoUI.Internal.Context.Core
  ( getsInteraction
  , modifyInteraction
  , getsOverlay
  , modifyOverlay
  , getsDamage
  , modifyDamage
  -- Interaction
  , takeTextEditLastAction
  , pointerHeldOffLayers
  -- Damage
  , markDirty
  , markDirtyCovered
  , clearDirty
  , isDirty
  , setWakeLoop
  , requestWakeAt
  , requestWakeAfter
  , getWakeAt
  , clearWakeAt
  , takeDamage
  , takeDamagePieces
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
  , writeStoreBool
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
  )
where

import Control.Monad (forM_, unless, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Primitive.SmallArray (SmallMutableArray, copySmallMutableArray, newSmallArray, readSmallArray, getSizeofSmallMutableArray, writeSmallArray)
import Data.IntMap.Strict qualified as IM
import GHC.Clock (getMonotonicTime)
import GHC.Exts (RealWorld)

import NanoUI.Internal.Context.Types
  ( Context (..)
  , DamageRequest (..)
  , DamageState (..)
  , InteractionState (..)
  , PointerRoute (..)
  , OverlayState
  , ThemeScopes (..)
  , intKey
  )
import NanoUI.Internal.Id (WidgetId, hashWidgetId)
import NanoUI.Internal.Layout.Arena (NodeIdx, getArenaScope, getNodeScope, getScopeSignature, lookupNodeByWidgetId)
import NanoUI.Internal.Store
  ( Field
  , Slot (..)
  , SlotWrites (..)
  , WidgetStore (..)
  , boolInt
  , diffKeys
  , slotChangedKeys
  , fieldInt
  , findSlot
  , insertSlot
  , lookupSlot
  , slotKey
  )
import NanoUI.Internal.Style (Theme, disabledTheme)
import NanoUI.Internal.Types (Damage, DamageBounds (..), Rect, defaultDamageSlop, rectH, rectW)
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

-- | Read and clear the last command run by a text-edit menu, with its target id.
takeTextEditLastAction :: Context -> IO (Maybe (WidgetId, TextCommand))
takeTextEditLastAction ctx = do
  act <- getsInteraction ctx isTextEditLastAction
  modifyInteraction ctx (\s -> s {isTextEditLastAction = Nothing})
  pure act

-- | Whether a held button went down on a menu or dropdown rather than on a
-- layer's widgets. Nothing in the layers is hot while it lasts.
{-# INLINE pointerHeldOffLayers #-}
pointerHeldOffLayers :: Context -> IO Bool
pointerHeldOffLayers ctx =
  getsInteraction ctx $ \s ->
    isPointerHeld s && case isPointerRoute s of
      RouteLayer _ -> False
      _ -> True

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
-- The request is opaque: its follow-up frame repaints the whole window.
{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = do
  modifyDamage ctx (\ds -> ds {dsDirty = True, dsDirtyOpaque = True})
  readIORef (ctxWakeLoop ctx) >>= sequence_

-- | 'markDirty' for a request whose visible effects the frame's own damage
-- machinery already covers: store writes are damaged per key ('modifyStore',
-- 'writeSlot', 'adoptSlot'). The follow-up frame these request clips instead
-- of repainting the whole window.
{-# INLINE markDirtyCovered #-}
markDirtyCovered :: Context -> IO ()
markDirtyCovered ctx = do
  modifyDamage ctx (\ds -> ds {dsDirty = True})
  readIORef (ctxWakeLoop ctx) >>= sequence_

-- | Clear the follow-up-frame request without clearing queued repaint bounds.
{-# INLINE clearDirty #-}
clearDirty :: Context -> IO ()
clearDirty ctx = modifyDamage ctx (\ds -> ds {dsDirty = False, dsDirtyOpaque = False})

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

-- | The disjoint rects a 'DamageClip' frame repaints, when its damage lies in
-- two or more places far apart; empty when the whole clip repaints. A
-- backend that draws text itself must clip it to each; draw commands
-- already are.
{-# INLINE takeDamagePieces #-}
takeDamagePieces :: Context -> IO [Rect]
takeDamagePieces ctx = getsDamage ctx dsDamagePieces

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
        slotChangedKeys prev store
          ++ diffKeys (storeFloat prev) (storeFloat store)
          ++ diffKeys (storePoint prev) (storePoint store)
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
      markDirtyCovered ctx

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
      markDirtyCovered ctx

-- | Write a boolean at the owner's base integer key, with change detection.
{-# INLINE writeStoreBool #-}
writeStoreBool :: Context -> WidgetId -> Bool -> IO ()
writeStoreBool ctx owner v = writeSlot fieldInt ctx owner (intKey owner) (boolInt v)

-- | Controlled widgets take their value from the caller every frame. The
-- caller's value replaces the stored one only when it differs from the value
-- the widget last returned ('recordSlot'). An edit applied between frames,
-- such as a menu cut, then survives a caller that passes the previous result
-- back, while a value changed by the application still wins. Returns the
-- value of the owner's slot after adopting.
{-# INLINE adoptSlot #-}
adoptSlot :: Eq a => Field a -> Context -> WidgetId -> a -> IO a
adoptSlot field ctx owner v = do
  st <- readIORef (ctxStore ctx)
  let k = intKey owner
      seenK = slotKey SlotSeen k
  if lookupSlot field seenK st == Just v
    then pure $! findSlot field v k st
    else do
      writeIORef (ctxStore ctx) $! insertSlot field seenK v (insertSlot field k v st)
      when (lookupSlot field k st /= Just v) $ do
        damageWidget ctx owner DamageSelf
        markDirtyCovered ctx
      pure v

-- | Remember the value a controlled widget returned this frame.
{-# INLINE recordSlot #-}
recordSlot :: Eq a => Field a -> Context -> Int -> a -> IO ()
recordSlot field ctx k v = do
  st <- readIORef (ctxStore ctx)
  let seenK = slotKey SlotSeen k
  when (lookupSlot field seenK st /= Just v) $
    writeIORef (ctxStore ctx) $! insertSlot field seenK v st

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
  pure
    ThemeScopes
      { tsCount = 0
      , tsThemes = cur
      , tsRaw = raw
      , tsPrevCount = 0
      , tsPrev = prev
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
          , tsPrevCount = tsCount ts
          , tsPrev = tsThemes ts
          , tsDisabled = False
          , tsChanged = False
          , tsPrevSig = sig
          }
    else writeIORef (ctxThemeScopes ctx) $! ts {tsCount = 0, tsDisabled = False, tsChanged = False}

-- | Add a scope with its disabled status and raw theme, painted faded
-- ('disabledTheme') when disabled. Returns the packed arena scope (the
-- one-based theme index and the disabled bit) and records whether the painted
-- theme differs from the previous frame's theme at that index.
pushThemeScope :: Context -> Bool -> Theme -> IO Int
pushThemeScope ctx disabled raw = do
  ts <- readIORef (ctxThemeScopes ctx)
  let !i = tsCount ts
      theme = if disabled then disabledTheme raw else raw
      -- The two arrays grow separately: 'tsThemes' trades places with
      -- 'tsPrev' each frame, so its capacity can differ from that of 'tsRaw'.
      withRoom arr fill = do
        cap <- getSizeofSmallMutableArray arr
        if i < cap
          then pure arr
          else do
            grown <- newSmallArray (cap * 2) fill
            copySmallMutableArray grown 0 arr 0 i
            pure grown
  themes <- withRoom (tsThemes ts) theme
  raws <- withRoom (tsRaw ts) raw
  same <-
    if i < tsPrevCount ts
      then (== theme) <$> readSmallArray (tsPrev ts) i
      else pure False
  writeSmallArray themes i theme
  writeSmallArray raws i raw
  writeIORef (ctxThemeScopes ctx) $!
    ts {tsCount = i + 1, tsThemes = themes, tsRaw = raws, tsDisabled = tsDisabled ts || disabled, tsChanged = tsChanged ts || not same}
  pure (((i + 1) `shiftL` 1) .|. fromEnum disabled)

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
scopeTheme = scopeThemeIn tsThemes

-- | A scope's theme before any disabled scope faded it.
scopeRawTheme :: Context -> Int -> IO Theme
scopeRawTheme = scopeThemeIn tsRaw

-- | The theme a packed arena scope names in one of the scope arrays, or the
-- base theme for theme index zero.
{-# INLINE scopeThemeIn #-}
scopeThemeIn :: (ThemeScopes -> SmallMutableArray RealWorld Theme) -> Context -> Int -> IO Theme
scopeThemeIn arr ctx scope
  | ti == 0 = readIORef (ctxTheme ctx)
  | otherwise = do
      ts <- readIORef (ctxThemeScopes ctx)
      readSmallArray (arr ts) (ti - 1)
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
