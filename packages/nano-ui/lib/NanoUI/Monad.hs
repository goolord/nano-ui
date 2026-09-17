{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module NanoUI.Monad
  ( NanoUI
  , Ui
  , runNanoUI
  , runUi
  , uiIO
  , emit
  , withKey
  , keyed
  , keyedTag
  , scope
  , nextId
  , burstNextIds
  , currentId
  , askContext
  , askInput
  , askDefaultLayout
  , withDefaultLayout
  , withLayout
  , askHost
  , uiFontMetrics
  , uiTime
  , uiTheme
  , setUiTheme
  , styled
  , themed
  , disabledWhen
  , uiMousePos
  , windowSize
  , windowWidth
  , windowHeight
  , damageWidgetNow
  , damageKeyNow
  , damageRectNow
  , damageGroupNow
  , damageFullNow
  , FrameMsg (..)
  , decodeMessages
  , reduceMessages
  , reduceUpdates
  , whenM
  , unlessM
  , ifM
  )
where


import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.Hashable (Hashable, hash)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Effectful
  ( Dispatch (Static)
  , DispatchOf
  , Eff
  , Effect
  , IOE
  , runEff
  , type (:>)
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects)
  , StaticRep
  , evalStaticRep
  , getStaticRep
  , localStaticRep
  , unEff
  , unsafeEff
  , unsafeEff_
  )
import GHC.Clock (getMonotonicTime)
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , askHostIO
  , damageFull
  , damageKey
  , damagePeers
  , damageRect
  , damageWidget
  , decodeMessages
  , currentTheme
  , pushMessage
  , pushThemeScope
  , scopeRawTheme
  , setTheme
  , reduceMessages
  , reduceUpdates
  )
import NanoUI.Font (FontMetrics)
import NanoUI.Id
  ( IdContext (siblingId)
  , WidgetId
  , enterKeyed
  , enterScope
  , idContextWidgetId
  , scopeTag
  )
import NanoUI.Layout.Arena (getArenaScope, setArenaScope)
import NanoUI.Style (Layout, Theme, disabledTheme)
import NanoUI.Input (Input (..), inputMousePos, inputWindowSize, stripInteractionInput)
import NanoUI.Types (DamageBounds, Rect, Size (..), V2)

type NanoUI = Eff '[Ui, IOE]

data Ui :: Effect

type instance DispatchOf Ui = Static WithSideEffects

data instance StaticRep Ui = UiRep !Context !Input !Layout

{-# INLINE runUi #-}
runUi :: IOE :> es => Context -> Input -> Eff (Ui : es) a -> Eff es a
runUi ctx inp ui = do
  lay <- unsafeEff_ (readIORef (ctxDefaultLayout ctx))
  evalStaticRep (UiRep ctx inp lay) ui

{-# INLINE runNanoUI #-}
runNanoUI :: Context -> Input -> NanoUI a -> IO a
runNanoUI ctx inp = runEff . runUi ctx inp

{-# INLINE uiIO #-}
uiIO :: Ui :> es => IO a -> Eff es a
uiIO m = do
  UiRep {} <- getStaticRep
  unsafeEff_ m

{-# INLINE emit #-}
emit :: (Typeable msg, Ui :> es) => msg -> Eff es ()
emit msg = do
  ctx <- askContext
  uiIO (pushMessage ctx (FrameMsg msg))

-- | The id 'nextId' would issue, without consuming it.
{-# INLINE currentId #-}
currentId :: Ui :> es => Eff es WidgetId
currentId = do
  ctx <- askContext
  ic <- uiIO (readIORef (ctxIdContext ctx))
  pure (idContextWidgetId ic)

{-# INLINE nextId #-}
nextId :: Ui :> es => Eff es WidgetId
nextId = do
  ctx <- askContext
  uiIO $ do
    ic <- readIORef (ctxIdContext ctx)
    writeIORef (ctxIdContext ctx) (ic {siblingId = siblingId ic + 1})
    pure (idContextWidgetId ic)

-- | Issue many widget ids in one IO loop (avoids deep Eff bind chains).
{-# INLINE burstNextIds #-}
burstNextIds :: Ui :> es => Int -> Eff es ()
burstNextIds n
  | n <= 0 = pure ()
  | otherwise = do
      ctx <- askContext
      uiIO $ modifyIORef' (ctxIdContext ctx) $ \ic ->
        let !sid = siblingId ic + fromIntegral n
         in ic {siblingId = sid}

-- Run @m@ in the child context from @enter@, then restore the advanced parent
-- (also on exceptions).
{-# INLINE withIdFrame #-}
withIdFrame ::
  Ui :> es => (IdContext -> (IdContext, IdContext)) -> Eff es a -> Eff es a
withIdFrame enter m = do
  ctx <- askContext
  unsafeEff $ \es ->
    bracket
      (do
        old <- readIORef (ctxIdContext ctx)
        let (p, c) = enter old
        writeIORef (ctxIdContext ctx) c
        pure p)
      (\parent' -> writeIORef (ctxIdContext ctx) parent')
      (\_ -> unEff m es)

{-# INLINE scope #-}
scope :: Ui :> es => Eff es a -> Eff es a
scope = withIdFrame (enterScope scopeTag)

{-# INLINE keyed #-}

-- | Stable child path from @tag@. Keys must be unique among siblings in the same scope.
keyed :: (Hashable k, Ui :> es) => k -> Eff es a -> Eff es a
keyed k = keyedTag (fromIntegral (hash k))

{-# INLINE keyedTag #-}
keyedTag :: Ui :> es => Word64 -> Eff es a -> Eff es a
keyedTag tag = withIdFrame (enterKeyed tag)

{-# INLINE withKey #-}
withKey :: (Hashable k, Ui :> es) => k -> Eff es a -> Eff es a
withKey = keyed

{-# INLINE askContext #-}
askContext :: Ui :> es => Eff es Context
askContext = do
  UiRep ctx _ _ <- getStaticRep
  pure ctx

{-# INLINE askDefaultLayout #-}
askDefaultLayout :: Ui :> es => Eff es Layout
askDefaultLayout = do
  UiRep _ _ l <- getStaticRep
  pure l

{-# INLINE withDefaultLayout #-}
withDefaultLayout :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
withDefaultLayout f = localStaticRep (\(UiRep ctx inp l) -> UiRep ctx inp (f l))

{-# INLINE withLayout #-}
withLayout :: Ui :> es => Layout -> Eff es a -> Eff es a
withLayout l = localStaticRep (\(UiRep ctx inp _) -> UiRep ctx inp l)

{-# INLINE uiFontMetrics #-}
uiFontMetrics :: Ui :> es => Eff es FontMetrics
uiFontMetrics = fmap ctxFontMetrics askContext

{-# INLINE uiTime #-}
-- | Monotonic seconds since some fixed epoch (process boot), as a 'Double'.
-- Use it for time-based animation math inside the UI effect. It stays in
-- 'Double' on purpose: converting wall-clock seconds to 'Float' loses ~3 ms
-- of resolution at 8 h uptime (worse longer), which is coarser than a frame
-- and quantizes animation sweeps into visible steps.
uiTime :: Ui :> es => Eff es Double
uiTime = uiIO getMonotonicTime

-- | The theme the view is drawn with where this is called: the context theme
-- as modified by the enclosing 'styled' and 'disabledWhen' scopes.
{-# INLINE uiTheme #-}
uiTheme :: Ui :> es => Eff es Theme
uiTheme = do
  ctx <- askContext
  uiIO (currentTheme ctx)

-- | Draw a part of the view with a modified theme. Widgets declared inside
-- take their colours, borders and corner radii from it, and 'styled' scopes
-- nest, each modifying the theme of the scope around it:
--
-- > styled (buttonStyle (cornerRadius 8)) $ do
-- >   styled primary (button "Save")
-- >   button "Cancel"
--
-- The modifier runs once per scope per frame. The theme only affects how
-- widgets look, never their layout.
{-# INLINE styled #-}
styled :: Ui :> es => (Theme -> Theme) -> Eff es a -> Eff es a
styled f = withPaintScope $ \ctx outer -> do
  raw <- f <$> scopeRawTheme ctx outer
  let !disabled = outer .&. 1
  ti <- pushThemeScope ctx (disabled /= 0) raw (if disabled /= 0 then disabledTheme raw else raw)
  pure ((ti `shiftL` 1) .|. disabled)

-- | Draw a part of the view with another theme, whatever the theme around it.
{-# INLINE themed #-}
themed :: Ui :> es => Theme -> Eff es a -> Eff es a
themed theme = styled (const theme)

-- | Disable every widget declared inside when the condition holds. Disabled
-- widgets keep their place, state and layout, but take no pointer or
-- keyboard input, cannot be focused, and are drawn with 'disabledTheme'.
--
-- > disabledWhen (T.null name) $ whenM (button "Save") save
{-# INLINE disabledWhen #-}
disabledWhen :: Ui :> es => Bool -> Eff es a -> Eff es a
disabledWhen False m = m
disabledWhen True m =
  -- The view inside sees no presses, keys or wheel, so no widget's own input
  -- handling can fire; the frame's focus and click passes check the scope.
  localStaticRep
    (\(UiRep ctx inp l) -> UiRep ctx (stripInteractionInput inp) {inputMouseDown = False, inputMouseRightDown = False} l)
    (withPaintScope enter m)
  where
    enter ctx outer
      | outer .&. 1 /= 0 = pure outer
      | otherwise = do
          raw <- scopeRawTheme ctx outer
          ti <- pushThemeScope ctx True raw (disabledTheme raw)
          pure ((ti `shiftL` 1) .|. 1)

-- Run @m@ with the arena scope @enter@ picks, then restore the scope around it
-- (also on exceptions).
{-# INLINE withPaintScope #-}
withPaintScope :: Ui :> es => (Context -> Int -> IO Int) -> Eff es a -> Eff es a
withPaintScope enter m = do
  ctx <- askContext
  let na = ctxNodeArena ctx
  unsafeEff $ \es ->
    bracket
      (do
        old <- getArenaScope na
        setArenaScope na =<< enter ctx old
        pure old)
      (setArenaScope na)
      (\_ -> unEff m es)

{-# INLINE setUiTheme #-}
setUiTheme :: Ui :> es => Theme -> Eff es ()
setUiTheme th = do
  ctx <- askContext
  uiIO (setTheme ctx th)

{-# INLINE uiMousePos #-}
uiMousePos :: Ui :> es => Eff es V2
uiMousePos = fmap inputMousePos askInput

{-# INLINE askInput #-}
askInput :: Ui :> es => Eff es Input
askInput = do
  UiRep _ inp _ <- getStaticRep
  pure inp

{-# INLINE windowSize #-}
windowSize :: Ui :> es => Eff es Size
windowSize = fmap inputWindowSize askInput

{-# INLINE windowWidth #-}
windowWidth :: Ui :> es => Eff es Float
windowWidth = fmap (sizeW . inputWindowSize) askInput

{-# INLINE windowHeight #-}
windowHeight :: Ui :> es => Eff es Float
windowHeight = fmap (sizeH . inputWindowSize) askInput

{-# INLINE askHost #-}
askHost :: (Typeable a, Ui :> es) => Eff es (Maybe a)
askHost = do
  ctx <- askContext
  uiIO (askHostIO ctx)

{-# INLINE damageWidgetNow #-}
damageWidgetNow :: (Ui :> es) => WidgetId -> DamageBounds -> Eff es ()
damageWidgetNow wid bounds = do
  ctx <- askContext
  uiIO (damageWidget ctx wid bounds)

{-# INLINE damageKeyNow #-}
damageKeyNow :: (Ui :> es) => Int -> DamageBounds -> Eff es ()
damageKeyNow k bounds = do
  ctx <- askContext
  uiIO (damageKey ctx k bounds)

{-# INLINE damageRectNow #-}
damageRectNow :: (Ui :> es) => Rect -> Eff es ()
damageRectNow r = do
  ctx <- askContext
  uiIO (damageRect ctx r)

{-# INLINE damageGroupNow #-}
damageGroupNow :: (Ui :> es) => [WidgetId] -> DamageBounds -> Eff es ()
damageGroupNow wids bounds = do
  ctx <- askContext
  uiIO (damagePeers ctx wids bounds)

{-# INLINE damageFullNow #-}
damageFullNow :: (Ui :> es) => Eff es ()
damageFullNow = do
  ctx <- askContext
  uiIO (damageFull ctx)

-- | Monadic variant of 'when'. Runs the second action if the first returns 'True'.
--
-- Example:
--
-- @
-- whenM (button "Save") saveDocument
-- @
{-# INLINE whenM #-}
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = mb >>= \b -> when b ma

-- | Monadic variant of 'unless'. Runs the second action if the first returns 'False'.
{-# INLINE unlessM #-}
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb ma = mb >>= \b -> unless b ma

-- | Monadic conditional selection.
{-# INLINE ifM #-}
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = mb >>= \b -> if b then t else f
