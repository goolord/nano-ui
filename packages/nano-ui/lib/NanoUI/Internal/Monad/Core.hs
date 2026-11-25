{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The view monad itself: the effect, running a view, the context, input,
-- id scopes, themes, fonts, and the damage requests backends make. The rest
-- of "NanoUI.Internal.Monad" builds on this.
module NanoUI.Internal.Monad.Core
  ( NanoUI (..)
  , NanoUIEs
  , Ui
  , runUi
  , runNanoUI
  , embedNanoUI
  , withRunInNanoUI
  , withContext
  , withUiResource
  , withBuildScope
  , catchView
  , currentId
  , nextId
  , freshWidget
  , burstNextIds
  , withIdFrame
  , scope
  , withKey
  , keyedTag
  , askContext
  , askDefaultLayout
  , withDefaultLayout
  , uiFontMetrics
  , uiFontSize
  , resolveFontUi
  , lineWidthUi
  , truncateTextUi
  , truncateTextWithUi
  , uiTime
  , uiTheme
  , styled
  , themed
  , disabledWhen
  , setTheme
  , followSystemTheme
  , systemAppearance
  , uiMousePos
  , mousePressed
  , mouseReleased
  , mouseHeld
  , askInput
  , askFrameInput
  , localInput
  , inertView
  , windowSize
  , windowWidth
  , windowHeight
  , askBackendSession
  , askHost
  , damageWidgetNow
  , damageKeyNow
  , damageRectNow
  , damageGroupNow
  , damageFullNow
  , whenM
  , unlessM
  , (<&&>)
  , ifM
  )
where

import Control.Exception (Exception, bracket)
import Control.Monad (unless, when, (<$!>))
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Catch qualified as Catch (MonadCatch (catch))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bits ((.&.), (.|.))
import Data.Hashable (Hashable, hash)
import Data.IORef (readIORef, writeIORef)
import Data.Monoid (Ap (..))
import Data.Text (Text)
import Data.Word (Word64)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, runEff, type (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep, getStaticRep, localStaticRep, unEff, unsafeEff, unsafeEff_)
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.BackendSession (BackendSession, SBackend)
import NanoUI.Internal.Context (askHostIO, Host, Context (ctxNodeArena, ctxContainerStack, ctxIdCursor, ctxFontMetrics, ctxFontSize), currentTheme, damageFull, damageKey, damagePeers, damageRect, damageWidget, pushThemeScope, scopeRawTheme, routedInput, backendSession, getSystemAppearance, setThemeInView)
import NanoUI.Internal.Context qualified as Ctx
import NanoUI.Internal.Draw.Types (TextFont (..))
import NanoUI.Internal.Font (FontMetrics, lineWidthIO, truncateTextIO, truncateTextWithIO)
import NanoUI.Internal.Frame.Node (resolveTextFont)
import NanoUI.Internal.Id (enterKeyed, enterScope, scopeTag, IdContext, WidgetId)
import NanoUI.Internal.Id.Cursor (peekWidgetId, readIdCursor, skipSiblingIds, takeWidgetId, writeIdCursor)
import NanoUI.Internal.Input (noButtons, stripInteractionInput, withoutPointer, Input (inputComposition, inputMousePos, inputWindowSize, inputButtonsHeld, inputKeysHeld), MouseButton, Pressable (heldIn, pressedIn, releasedIn))
import NanoUI.Internal.Layout.Arena (getArenaScope, setArenaScope)
import NanoUI.Internal.Style (Appearance, FontStyle, FontVariant, FontWeight, Layout, TextDecoration (DecorationNone), TextEllipsis, Theme, defaultLayout)
import NanoUI.Internal.Types (DamageBounds, Rect, Size (..), V2)


-- | A view: widgets, layout, local state and IO. Backend runners execute it
-- as frames are needed; local-state changes can trigger a second pass within
-- a frame. Run IO in it with 'liftIO'; the action runs on every frame that
-- reaches it, so guard one-shot effects with a button or another event.
--
-- "NanoUI.Effectful" exports the constructor and functions for mixing views
-- with other @effectful@ effects in 'NanoUIEs'.
newtype NanoUI a = NanoUI {unNanoUI :: Eff NanoUIEs a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadIO
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadBase IO
    , MonadBaseControl IO
    )
  deriving (Semigroup, Monoid) via Ap NanoUI a

-- | The effect row a 'NanoUI' view runs in.
type NanoUIEs = '[Ui, IOE]

-- | Access to the current context, routed input, layout defaults, and widget ids.
data Ui :: Effect

type instance DispatchOf Ui = Static WithSideEffects

-- The input is held twice: as routed to the view being declared, which is
-- what widgets read, and as the frame received it ('askFrameInput'). Few
-- things read the second, so it is lazy: a 'disabledWhen' scope strips it
-- only if something inside asks.
data instance StaticRep Ui = UiRep
  { repContext :: !Context
  , repInput :: !Input
  , repFrame :: Input
  , repLayout :: !Layout
  }

-- | Interpret UI operations using a context and input. This runs the view only;
-- use a backend or @runFrame@ to reset arenas, solve layout, and paint.
{-# INLINE runUi #-}
runUi :: IOE :> es => Context -> Input -> Eff (Ui : es) a -> Eff es a
runUi ctx inp ui = do
  -- The page is layer 0; floating panels route their own bodies.
  page <- unsafeEff_ (routedInput ctx 0 inp)
  evalStaticRep (UiRep ctx page inp defaultLayout) ui

-- | Run a view against a context and input, without laying it out or
-- painting it.
{-# INLINE runNanoUI #-}
runNanoUI :: Context -> Input -> NanoUI a -> IO a
runNanoUI ctx inp = runEff . runUi ctx inp . unNanoUI

-- | Run a 'NanoUI' view as part of a view in any effect row with 'Ui', where
-- it declares its widgets as if written there: same context, input, layout
-- defaults and widget ids.
embedNanoUI :: Ui :> es => NanoUI a -> Eff es a
embedNanoUI view = withRunInNanoUI (\_ -> view)

-- | Run a 'NanoUI' view from any effect row with 'Ui', with a function that
-- runs that row's actions inside it: how a view written in a larger row
-- passes a body using its other effects to a container widget.
--
-- > counter :: (Ui :> es, State Int :> es) => Eff es ()
-- > counter = withRunInNanoUI $ \run -> row $ do
-- >   n <- run get
-- >   whenM (button "+") (run (put (n + 1)))
--
-- An action run this way sees the scope it is run in (layout defaults,
-- routed input, 'disabledWhen'), not the one 'withRunInNanoUI' was called
-- in. Call the function only on the thread running the view.
withRunInNanoUI :: forall es a. Ui :> es => ((forall r. Eff es r -> NanoUI r) -> NanoUI a) -> Eff es a
withRunInNanoUI k = unsafeEff $ \es -> do
  rep <- unEff (getStaticRep @Ui) es
  let run :: Eff es r -> NanoUI r
      run m = NanoUI $ do
        inner <- getStaticRep @Ui
        unsafeEff_ (unEff (localStaticRep (const inner) m) es)
  runEff (evalStaticRep rep (unNanoUI (k run)))

-- | Run an action on the view's 'Context'.
{-# INLINE withContext #-}
withContext :: (Context -> IO a) -> NanoUI a
withContext f = NanoUI (getStaticRep >>= \r -> unsafeEff_ (f $! repContext r))

-- | Acquire UI-thread state, run an action, and restore it even on exceptions.
-- Acquisition and release are masked, as in 'bracket'; the view inherits the
-- caller's masking state.
{-# INLINE withUiResource #-}
withUiResource :: IO a -> (a -> IO ()) -> NanoUI b -> NanoUI b
withUiResource acquire release (NanoUI action) =
  NanoUI (unsafeEff $ \es -> bracket acquire release (\_ -> unEff action es))

-- | Enter a view build scope (an id scope, a container, a paint scope), run
-- the action, and leave it. No exception handler is installed: an exception
-- that escapes the action leaves the scope entered for the rest of the pass,
-- which 'catchView' undoes for a view that recovers, and the next pass starts
-- from a clean build (@resetUiBuild@). Every row,
-- column, 'scope', 'withKey' row and 'styled' call goes through here, where a
-- handler would cost an allocation each.
{-# INLINE withBuildScope #-}
withBuildScope :: IO a -> (a -> IO ()) -> NanoUI b -> NanoUI b
withBuildScope acquire release m = do
  a <- liftIO acquire
  r <- m
  liftIO (release a)
  pure r

-- | Run part of a view, and run the handler after it if it throws. The view's
-- build state (open containers, the id scope, the paint scope) is put back
-- as it was before the part ran, so the handler and the rest of the view are
-- laid out where they are declared. What the part declared before it threw
-- stays in the frame:
--
-- > catchView (column (riskyPanel model)) $ \(e :: SomeException) ->
-- >   label ("Could not show the panel: " <> T.pack (displayException e))
--
-- Catching with 'Control.Monad.Catch.catch' or 'Control.Monad.Catch.try'
-- instead works, but leaves whatever containers the part had open around
-- the rest of the view until the next frame.
catchView :: Exception e => NanoUI a -> (e -> NanoUI a) -> NanoUI a
catchView m handler = do
  ctx <- askContext
  let na = ctxNodeArena ctx
  saved <- liftIO $ (,,) <$> readIORef (ctxContainerStack ctx) <*> readIdCursor (ctxIdCursor ctx) <*> getArenaScope na
  m `Catch.catch` \e -> do
    liftIO $ case saved of
      (stack, ids, sc) -> do
        writeIORef (ctxContainerStack ctx) stack
        writeIdCursor (ctxIdCursor ctx) ids
        setArenaScope na sc
    handler e

-- | The id 'nextId' would issue, without consuming it.
{-# INLINE currentId #-}
currentId :: NanoUI WidgetId
currentId = withContext (peekWidgetId . ctxIdCursor)

-- | Consume the next sibling id. Widgets share this sequence (positional
-- Vars count apart), so conditional widgets need their own 'scope'.
{-# INLINE nextId #-}
nextId :: NanoUI WidgetId
nextId = withContext (takeWidgetId . ctxIdCursor)

-- | The next sibling id ('nextId') and the view's 'Context', which a widget's body starts from.
{-# INLINE freshWidget #-}
freshWidget :: NanoUI (WidgetId, Context)
freshWidget = do
  wid <- nextId
  ctx <- askContext
  pure (wid, ctx)

-- | Reserve @n@ sibling ids without returning them. Non-positive counts do nothing.
{-# INLINE burstNextIds #-}
burstNextIds :: Int -> NanoUI ()
burstNextIds n
  | n <= 0 = pure ()
  | otherwise = withContext $ \ctx -> skipSiblingIds (ctxIdCursor ctx) n

-- | Run in the child context returned by @enter@, then restore its advanced
-- parent context, including when the action throws an exception.
{-# INLINE withIdFrame #-}
withIdFrame ::
  (IdContext -> (IdContext, IdContext)) -> NanoUI a -> NanoUI a
withIdFrame enter m = do
  ctx <- askContext
  withBuildScope
    ( do
        old <- readIdCursor (ctxIdCursor ctx)
        let
          !(!p, !c) = enter old
        writeIdCursor (ctxIdCursor ctx) c
        pure p
    )
    (writeIdCursor (ctxIdCursor ctx))
    m

-- | Give the action a child id sequence while consuming one parent id.
-- Put conditional content inside this scope to keep later siblings stable.
{-# INLINE scope #-}
scope :: NanoUI a -> NanoUI a
scope = withIdFrame (enterScope scopeTag)

-- | Run the action under a key so its widgets keep their ids and state when
-- earlier siblings change. Keys must be unique among siblings in the same
-- scope; use a stable item key for a list that can be reordered. A keyed
-- scope takes no sibling position, so widgets declared after a keyed list
-- keep their ids as the list grows and shrinks.
--
-- The state kept under a key, by widgets such as a text field and by
-- 'NanoUI.Var.useVar', lasts as long as the context, also once the key is no
-- longer shown ('NanoUI.Var.useVar' says what to use for a growing feed).
{-# INLINE withKey #-}
withKey :: Hashable k => k -> NanoUI a -> NanoUI a
withKey k = keyedTag (fromIntegral (hash k))

-- | A keyed child scope using a precomputed 64-bit tag. Tags must be unique
-- among siblings; use 'withKey' to hash an application key.
{-# INLINE keyedTag #-}
keyedTag :: Word64 -> NanoUI a -> NanoUI a
keyedTag tag = withIdFrame (enterKeyed tag)

-- | The mutable context for this view. It belongs to the current UI session.
{-# INLINE askContext #-}
askContext :: NanoUI Context
askContext = NanoUI (repContext <$!> getStaticRep)

-- | Layout defaults in the current 'withDefaultLayout' scope.
{-# INLINE askDefaultLayout #-}
askDefaultLayout :: NanoUI Layout
askDefaultLayout = NanoUI (repLayout <$!> getStaticRep)

-- | Modify layout defaults for the enclosed action, restoring them on exit.
{-# INLINE withDefaultLayout #-}
withDefaultLayout :: (Layout -> Layout) -> NanoUI a -> NanoUI a
withDefaultLayout f (NanoUI m) = NanoUI (localStaticRep (\r -> r {repLayout = f (repLayout r)}) m)

-- | The context's base font metrics, before per-widget font overrides.
{-# INLINE uiFontMetrics #-}
uiFontMetrics :: NanoUI FontMetrics
uiFontMetrics = fmap ctxFontMetrics askContext

-- | The backend's default font size, used when a layout sets none.
-- 'NanoUI.fontSizeScale' scales relative to this size unless an explicit
-- 'NanoUI.fontSize' was already set.
{-# INLINE uiFontSize #-}
uiFontSize :: NanoUI Float
uiFontSize = fmap ctxFontSize askContext

-- | Metrics for text at a size, weight, style and variant, resolved through
-- the backend's fonts: what a custom widget measures and places its text by
-- when it draws in a font other than the context's.
--
-- It resolves the font as a 'DrawTextStyled' op naming the same size,
-- weight, style and variant is painted, so text measured with these metrics
-- is drawn at the width it was measured at.
resolveFontUi :: Float -> FontWeight -> FontStyle -> FontVariant -> NanoUI FontMetrics
resolveFontUi size weight style variant =
  withContext (\ctx -> fst <$> resolveTextFont ctx (TextFont size variant weight style DecorationNone False))

-- | The advance of one line of text in these metrics, in logical pixels. Unlike
-- the pure 'NanoUI.lineWidth' it first loads the glyphs the text needs,
-- so it is right for metrics that have not drawn this text yet.
lineWidthUi :: FontMetrics -> Text -> NanoUI Float
lineWidthUi fm txt = liftIO (lineWidthIO fm txt)

-- | One line of text cut to fit @maxW@ logical pixels in these metrics,
-- ending in @...@ when it was cut; the text as it is when it fits. Too
-- narrow for the dots, it is cut without them. Buttons and selects cut
-- their labels this way themselves, and so does a single-line label with
-- 'fillW' or 'maxW' in a row.
--
-- > fm <- uiFontMetrics
-- > name <- truncateTextUi fm 120 (trackName t)
truncateTextUi :: FontMetrics -> Float -> Text -> NanoUI Text
truncateTextUi fm maxW txt = liftIO (truncateTextIO (lineWidthIO fm) maxW txt)

-- | Truncate one line of text to @maxW@ using an explicit 'TextEllipsis' mode.
truncateTextWithUi :: TextEllipsis -> FontMetrics -> Float -> Text -> NanoUI Text
truncateTextWithUi ell fm maxW txt = liftIO (truncateTextWithIO ell (lineWidthIO fm) maxW txt)

{-# INLINE uiTime #-}
-- | Monotonic seconds from an unspecified epoch. Subtract two readings to
-- measure elapsed time; this is not a wall-clock timestamp. Keep absolute
-- readings as 'Double' to retain precision during long sessions.
uiTime :: NanoUI Double
uiTime = liftIO getMonotonicTime

-- | The theme the view is drawn with where this is called: the context theme
-- as modified by the enclosing 'styled' and 'disabledWhen' scopes.
{-# INLINE uiTheme #-}
uiTheme :: NanoUI Theme
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
styled :: (Theme -> Theme) -> NanoUI a -> NanoUI a
styled f = withPaintScope $ \ctx outer ->
  (.|. (outer .&. 2)) <$> (pushThemeScope ctx (outer .&. 1 /= 0) . f =<< scopeRawTheme ctx outer)

-- | Draw a part of the view with another theme, whatever the theme around it.
{-# INLINE themed #-}
themed :: Theme -> NanoUI a -> NanoUI a
themed theme = styled (const theme)

-- | Disable every widget declared inside when the condition holds. Disabled
-- widgets keep their place, state and layout, but take no pointer or
-- keyboard input, cannot be focused, and are drawn with 'disabledTheme'.
--
-- > disabledWhen (T.null name) $ whenM (button "Save") save
{-# INLINE disabledWhen #-}
disabledWhen :: Bool -> NanoUI a -> NanoUI a
disabledWhen False m = m
disabledWhen True m =
  -- The view inside sees no presses, keys or wheel, so no widget's own input
  -- handling can fire; the frame's focus and click passes check the scope.
  NanoUI . localStaticRep
    (\r -> r {repInput = inert (repInput r), repFrame = inert (repFrame r)})
    $ unNanoUI (withPaintScope enter m)
  where
    inert i =
      (stripInteractionInput i) {inputButtonsHeld = noButtons, inputKeysHeld = mempty}
    enter ctx outer
      | outer .&. 1 /= 0 = pure outer
      | otherwise = (.|. (outer .&. 2)) <$> (pushThemeScope ctx True =<< scopeRawTheme ctx outer)

-- Run @m@ with the arena scope @enter@ picks, then restore the scope around it
-- (also on exceptions).
{-# INLINE withPaintScope #-}
withPaintScope :: (Context -> Int -> IO Int) -> NanoUI a -> NanoUI a
withPaintScope enter m = do
  ctx <- askContext
  let
    na = ctxNodeArena ctx
  withBuildScope
    ( do
        old <- getArenaScope na
        setArenaScope na =<< enter ctx old
        pure old
    )
    (setArenaScope na)
    m

-- | Set the session's base theme, repainting if it changed. Use 'styled' for
-- a change limited to part of the view. Setting the same theme again is a
-- no-op, so a view can pick its theme every frame:
--
-- > setTheme . lightDark defaultLightTheme defaultTheme =<< systemAppearance
--
-- This replaces a theme set by 'followSystemTheme'.
{-# INLINE setTheme #-}
setTheme :: Theme -> NanoUI ()
setTheme th = withContext (`setThemeInView` th)

-- | Switch the session's theme automatically whenever the OS light/dark
-- preference changes. Calling 'setTheme' returns to a fixed theme.
{-# INLINE followSystemTheme #-}
followSystemTheme :: (Maybe Appearance -> Theme) -> NanoUI ()
followSystemTheme pick = withContext (`Ctx.followSystemTheme` pick)

-- | System light or dark preference, or 'Nothing' when the backend does not
-- report it (RGFW never does). A change repaints the whole window.
{-# INLINE systemAppearance #-}
systemAppearance :: NanoUI (Maybe Appearance)
systemAppearance = withContext getSystemAppearance

-- | Pointer position in the current layer's routed input (moved off-screen
-- when occluded by a layer or overlay in front).
{-# INLINE uiMousePos #-}
uiMousePos :: NanoUI V2
uiMousePos = fmap inputMousePos askInput

-- | Whether the button went down this frame with the pointer over the part of
-- the view being declared, on any widget. 'False' behind an open modal, under
-- a menu or panel drawn in front, and inside 'disabledWhen'. For a press on a
-- particular widget, use its 'NanoUI.respClickedWith' or 'NanoUI.respHeldWith'.
--
-- > whenM (mousePressed MouseBack) goBack
{-# INLINE mousePressed #-}
mousePressed :: MouseButton -> NanoUI Bool
mousePressed b = pressedIn b <$> askInput

-- | Like 'mousePressed', for a button that came up this frame.
{-# INLINE mouseReleased #-}
mouseReleased :: MouseButton -> NanoUI Bool
mouseReleased b = releasedIn b <$> askInput

-- | Like 'mousePressed', for a button that is down.
{-# INLINE mouseHeld #-}
mouseHeld :: MouseButton -> NanoUI Bool
mouseHeld b = heldIn b <$> askInput

-- | Input routed to the current layer. Covered layers receive no pointer;
-- disabled scopes also remove keyboard and other interaction events.
{-# INLINE askInput #-}
askInput :: NanoUI Input
askInput = NanoUI (repInput <$!> getStaticRep)

-- | Unrouted frame input, including the pointer regardless of which layer owns
-- it. Used for window-wide checks (such as outside-click dismissal or routing
-- input to floating panels). Widgets should read 'askInput' instead so they do
-- not react through overlays.
{-# INLINE askFrameInput #-}
askFrameInput :: NanoUI Input
askFrameInput = do
  UiRep {repFrame = frame} <- NanoUI getStaticRep
  pure frame

-- | Run a part of the view with another routed input.
{-# INLINE localInput #-}
localInput :: Input -> NanoUI a -> NanoUI a
localInput inp (NanoUI m) = NanoUI (localStaticRep (\r -> r {repInput = inp}) m)

-- | Display-only content sees neither routed nor raw interaction input.
-- Its container flag independently excludes its widgets from focus and IME.
inertView :: NanoUI a -> NanoUI a
inertView m = withPaintScope (\_ outer -> pure (outer .|. 2)) $ NanoUI $
  localStaticRep (\r -> r {repInput = inert (repInput r), repFrame = inert (repFrame r)}) (unNanoUI m)
  where
    inert i = (withoutPointer (stripInteractionInput i)) {inputKeysHeld = mempty, inputComposition = Nothing}

-- | The window's content size in logical pixels ('NanoUI.winSize' of
-- 'NanoUI.askWindow').
{-# INLINE windowSize #-}
windowSize :: NanoUI Size
windowSize = fmap inputWindowSize askInput

-- | Width component of 'windowSize', in logical pixels.
{-# INLINE windowWidth #-}
windowWidth :: NanoUI Float
windowWidth = fmap (sizeW . inputWindowSize) askInput

-- | Height component of 'windowSize', in logical pixels.
{-# INLINE windowHeight #-}
windowHeight :: NanoUI Float
windowHeight = fmap (sizeH . inputWindowSize) askInput

-- | The session value of the backend driving this view, if it is the
-- backend asked for ('NanoUI.Backend.withBackendSession'). For a
-- backend's own view-side operations.
{-# INLINE askBackendSession #-}
askBackendSession :: SBackend b -> NanoUI (Maybe (BackendSession b))
askBackendSession backend = withContext (backendSession backend)

-- | Read an explicit typed host slot. 'Nothing' means it is uninstalled.
{-# INLINE askHost #-}
askHost :: Host a -> NanoUI (Maybe a)
askHost = liftIO . askHostIO

-- | Request repaint bounds relative to a widget's rectangle.
{-# INLINE damageWidgetNow #-}
damageWidgetNow :: WidgetId -> DamageBounds -> NanoUI ()
damageWidgetNow wid bounds = withContext (\ctx -> damageWidget ctx wid bounds)

-- | 'damageWidgetNow' using the integer store key of a widget.
{-# INLINE damageKeyNow #-}
damageKeyNow :: Int -> DamageBounds -> NanoUI ()
damageKeyNow k bounds = withContext (\ctx -> damageKey ctx k bounds)

-- | Request repaint of a rectangle in logical window coordinates.
{-# INLINE damageRectNow #-}
damageRectNow :: Rect -> NanoUI ()
damageRectNow r = withContext (\ctx -> damageRect ctx r)

-- | Request repaint bounds for each widget in a group.
{-# INLINE damageGroupNow #-}
damageGroupNow :: [WidgetId] -> DamageBounds -> NanoUI ()
damageGroupNow wids bounds = withContext (\ctx -> damagePeers ctx wids bounds)

-- | Request repaint of the entire window, for changes without widget bounds.
{-# INLINE damageFullNow #-}
damageFullNow :: NanoUI ()
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

