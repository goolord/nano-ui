{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'Ui' effect and the 'NanoUI' view type: running a view, widget id
-- scopes and keys, theme scopes, and damage requests from inside a view.
module NanoUI.Monad
  ( NanoUI
  , Ui
  , runNanoUI
  , runUi
  , uiIO
  , withContext
  , emit
  , withKey
  , keyed
  , keyedTag
  , scope
  , withIdFrame
  , nextId
  , burstNextIds
  , currentId
  , askContext
  , askInput
  , askFrameInput
  , localInput
  , askDefaultLayout
  , withDefaultLayout
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
  , (<&&>)
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
  , routedInput
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

-- | A view with UI operations and IO. Backend runners execute it as frames
-- are needed; local-state changes can trigger a second pass within a frame.
type NanoUI = Eff '[Ui, IOE]

-- | Access to the current context, routed input, layout defaults, and widget ids.
data Ui :: Effect

type instance DispatchOf Ui = Static WithSideEffects

-- The input is held twice: as routed to the view being declared, which is
-- what widgets read, and as the frame received it ('askFrameInput'). Few
-- things read the second, so it is lazy: a 'disabledWhen' scope strips it
-- only if something inside asks.
data instance StaticRep Ui = UiRep !Context !Input Input !Layout

-- | Interpret UI operations using a context and input. This runs the view only;
-- use a backend or @runFrame@ to reset arenas, solve layout, and paint.
{-# INLINE runUi #-}
runUi :: IOE :> es => Context -> Input -> Eff (Ui : es) a -> Eff es a
runUi ctx inp ui = do
  lay <- unsafeEff_ (readIORef (ctxDefaultLayout ctx))
  -- The page is layer 0; floating panels route their own bodies.
  page <- unsafeEff_ (routedInput ctx 0 inp)
  evalStaticRep (UiRep ctx page inp lay) ui

-- | Run 'runUi' in IO for the standard 'NanoUI' effect stack.
{-# INLINE runNanoUI #-}
runNanoUI :: Context -> Input -> NanoUI a -> IO a
runNanoUI ctx inp = runEff . runUi ctx inp

-- | Perform IO while building a view. The action runs on every frame that
-- reaches it; guard one-shot effects with a button or another event.
{-# INLINE uiIO #-}
uiIO :: Ui :> es => IO a -> Eff es a
uiIO m = do
  UiRep {} <- getStaticRep
  unsafeEff_ m

-- | Run an action on the view's 'Context'.
{-# INLINE withContext #-}
withContext :: Ui :> es => (Context -> IO a) -> Eff es a
withContext f = do
  UiRep ctx _ _ _ <- getStaticRep
  unsafeEff_ (f ctx)

-- | Queue a typed message for the frame's reducer, in emission order.
{-# INLINE emit #-}
emit :: (Typeable msg, Ui :> es) => msg -> Eff es ()
emit msg = withContext (\ctx -> pushMessage ctx (FrameMsg msg))

-- | The id 'nextId' would issue, without consuming it.
{-# INLINE currentId #-}
currentId :: Ui :> es => Eff es WidgetId
currentId = do
  ctx <- askContext
  ic <- uiIO (readIORef (ctxIdContext ctx))
  pure (idContextWidgetId ic)

-- | Consume the next sibling id. Widgets and state hooks share this sequence,
-- so conditional calls need their own 'scope'.
{-# INLINE nextId #-}
nextId :: Ui :> es => Eff es WidgetId
nextId = do
  ctx <- askContext
  uiIO $ do
    ic <- readIORef (ctxIdContext ctx)
    writeIORef (ctxIdContext ctx) $! ic {siblingId = siblingId ic + 1}
    pure (idContextWidgetId ic)

-- | Reserve @n@ sibling ids without returning them. Non-positive counts do nothing.
{-# INLINE burstNextIds #-}
burstNextIds :: Ui :> es => Int -> Eff es ()
burstNextIds n
  | n <= 0 = pure ()
  | otherwise = do
      ctx <- askContext
      uiIO $ modifyIORef' (ctxIdContext ctx) $ \ic ->
        let !sid = siblingId ic + fromIntegral n
         in ic {siblingId = sid}

-- | Run in the child context returned by @enter@, then restore its advanced
-- parent context, including when the action throws an exception.
{-# INLINE withIdFrame #-}
withIdFrame ::
  Ui :> es => (IdContext -> (IdContext, IdContext)) -> Eff es a -> Eff es a
withIdFrame enter m = do
  ctx <- askContext
  unsafeEff $ \es ->
    bracket
      (do
        old <- readIORef (ctxIdContext ctx)
        let !(!p, !c) = enter old
        writeIORef (ctxIdContext ctx) c
        pure p)
      (\parent' -> writeIORef (ctxIdContext ctx) parent')
      (\_ -> unEff m es)

-- | Give the action a child id sequence while consuming one parent id.
-- Put conditional content inside this scope to keep later siblings stable.
{-# INLINE scope #-}
scope :: Ui :> es => Eff es a -> Eff es a
scope = withIdFrame (enterScope scopeTag)

{-# INLINE keyed #-}

-- | Stable child path from @tag@. Keys must be unique among siblings in the same scope.
keyed :: (Hashable k, Ui :> es) => k -> Eff es a -> Eff es a
keyed k = keyedTag (fromIntegral (hash k))

-- | A keyed child scope using a precomputed 64-bit tag. Tags must be unique
-- among siblings; use 'withKey' to hash an application key.
{-# INLINE keyedTag #-}
keyedTag :: Ui :> es => Word64 -> Eff es a -> Eff es a
keyedTag tag = withIdFrame (enterKeyed tag)

-- | Alias for 'keyed'. Use a stable item key when a list can be reordered.
{-# INLINE withKey #-}
withKey :: (Hashable k, Ui :> es) => k -> Eff es a -> Eff es a
withKey = keyed

-- | The mutable context for this view. It belongs to the current UI session.
{-# INLINE askContext #-}
askContext :: Ui :> es => Eff es Context
askContext = do
  UiRep ctx _ _ _ <- getStaticRep
  pure ctx

-- | Layout defaults in the current 'withDefaultLayout' scope.
{-# INLINE askDefaultLayout #-}
askDefaultLayout :: Ui :> es => Eff es Layout
askDefaultLayout = do
  UiRep _ _ _ l <- getStaticRep
  pure l

-- | Modify layout defaults for the enclosed action, restoring them on exit.
{-# INLINE withDefaultLayout #-}
withDefaultLayout :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
withDefaultLayout f = localStaticRep (\(UiRep ctx inp frame l) -> UiRep ctx inp frame (f l))

-- | The context's base font metrics, before per-widget font overrides.
{-# INLINE uiFontMetrics #-}
uiFontMetrics :: Ui :> es => Eff es FontMetrics
uiFontMetrics = fmap ctxFontMetrics askContext

{-# INLINE uiTime #-}
-- | Monotonic seconds from an unspecified epoch. Subtract two readings to
-- measure elapsed time; this is not a wall-clock timestamp. Keep absolute
-- readings as 'Double' to retain precision during long sessions.
uiTime :: Ui :> es => Eff es Double
uiTime = uiIO getMonotonicTime

-- | The theme the view is drawn with where this is called: the context theme
-- as modified by the enclosing 'styled' and 'disabledWhen' scopes.
{-# INLINE uiTheme #-}
uiTheme :: Ui :> es => Eff es Theme
uiTheme = withContext currentTheme

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
    (\(UiRep ctx inp frame l) -> UiRep ctx (inert inp) (inert frame) l)
    (withPaintScope enter m)
  where
    inert i = (stripInteractionInput i) {inputMouseDown = False, inputMouseRightDown = False}
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

-- | Set the session's base theme and request a repaint. Use 'styled' for a
-- temporary change limited to part of the view.
{-# INLINE setUiTheme #-}
setUiTheme :: Ui :> es => Theme -> Eff es ()
setUiTheme th = withContext (\ctx -> setTheme ctx th)

-- | Where the pointer is, as the view being declared sees it: far off every
-- widget while something drawn in front has the pointer.
{-# INLINE uiMousePos #-}
uiMousePos :: Ui :> es => Eff es V2
uiMousePos = fmap inputMousePos askInput

-- | Input routed to the current layer. Covered layers receive no pointer;
-- disabled scopes also remove keyboard and other interaction events.
{-# INLINE askInput #-}
askInput :: Ui :> es => Eff es Input
askInput = do
  UiRep _ inp _ _ <- getStaticRep
  pure inp

-- | The frame's input before routing, pointer included whoever it belongs
-- to. For what watches the whole window rather than reacting to its own
-- events: a click anywhere outside dismissing a popup, a floating panel
-- working out its body's input. A widget that read its presses from this
-- would react through whatever is drawn over it, so widgets use 'askInput'.
{-# INLINE askFrameInput #-}
askFrameInput :: Ui :> es => Eff es Input
askFrameInput = do
  UiRep _ _ frame _ <- getStaticRep
  pure frame

-- | Run a part of the view with another routed input.
{-# INLINE localInput #-}
localInput :: Ui :> es => Input -> Eff es a -> Eff es a
localInput inp = localStaticRep (\(UiRep ctx _ frame l) -> UiRep ctx inp frame l)

-- | The application window's content size in logical pixels.
{-# INLINE windowSize #-}
windowSize :: Ui :> es => Eff es Size
windowSize = fmap inputWindowSize askInput

-- | Width component of 'windowSize', in logical pixels.
{-# INLINE windowWidth #-}
windowWidth :: Ui :> es => Eff es Float
windowWidth = fmap (sizeW . inputWindowSize) askInput

-- | Height component of 'windowSize', in logical pixels.
{-# INLINE windowHeight #-}
windowHeight :: Ui :> es => Eff es Float
windowHeight = fmap (sizeH . inputWindowSize) askInput

-- | Retrieve the host value installed in the context. 'Nothing' means no
-- value was installed or its runtime type differs from the requested type.
{-# INLINE askHost #-}
askHost :: (Typeable a, Ui :> es) => Eff es (Maybe a)
askHost = withContext askHostIO

-- | Request repaint bounds relative to a widget's rectangle.
{-# INLINE damageWidgetNow #-}
damageWidgetNow :: (Ui :> es) => WidgetId -> DamageBounds -> Eff es ()
damageWidgetNow wid bounds = withContext (\ctx -> damageWidget ctx wid bounds)

-- | 'damageWidgetNow' using the integer store key of a widget.
{-# INLINE damageKeyNow #-}
damageKeyNow :: (Ui :> es) => Int -> DamageBounds -> Eff es ()
damageKeyNow k bounds = withContext (\ctx -> damageKey ctx k bounds)

-- | Request repaint of a rectangle in logical window coordinates.
{-# INLINE damageRectNow #-}
damageRectNow :: (Ui :> es) => Rect -> Eff es ()
damageRectNow r = withContext (\ctx -> damageRect ctx r)

-- | Request repaint bounds for each widget in a group.
{-# INLINE damageGroupNow #-}
damageGroupNow :: (Ui :> es) => [WidgetId] -> DamageBounds -> Eff es ()
damageGroupNow wids bounds = withContext (\ctx -> damagePeers ctx wids bounds)

-- | Request repaint of the entire window, for changes without widget bounds.
{-# INLINE damageFullNow #-}
damageFullNow :: (Ui :> es) => Eff es ()
damageFullNow = withContext damageFull

-- | Monadic variant of 'when'. Runs the second action if the first returns 'True'.
--
-- Example:
--
-- > whenM (button "Save") saveDocument
{-# INLINE whenM #-}
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = mb >>= \b -> when b ma

-- | Monadic variant of 'unless'. Runs the second action if the first returns 'False'.
{-# INLINE unlessM #-}
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb ma = mb >>= \b -> unless b ma

-- | '&&' over effectful tests: the second runs only when the first holds.
infixr 3 <&&>

{-# INLINE (<&&>) #-}
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
a <&&> b = a >>= \ok -> if ok then b else pure False

-- | Monadic conditional selection.
{-# INLINE ifM #-}
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = mb >>= \b -> if b then t else f
