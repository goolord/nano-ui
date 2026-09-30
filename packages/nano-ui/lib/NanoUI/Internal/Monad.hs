{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of "NanoUI.Monad", plus running a view against a context,
-- reading and swapping the context, id-frame plumbing, and the frame's
-- message queue.
module NanoUI.Internal.Monad
  ( NanoUI (..)
  , NanoUIEs
  , Ui
  , runNanoUI
  , runUi
  , embedNanoUI
  , withRunInNanoUI
  , liftIO
  , withContext
  , withUiResource
  , emit
  , withKey
  , keyedTag
  , scope
  , withIdFrame
  , nextId
  , freshWidget
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
  , uiFontSize
  , resolveFontUi
  , lineWidthUi
  , truncateTextUi
  , uiTime
  , uiTheme
  , setUiTheme
  , systemAppearance
  , styled
  , themed
  , disabledWhen
  , uiMousePos
  , mousePressed
  , mouseReleased
  , mouseHeld
  , windowSize
  , windowWidth
  , windowHeight
  , lastRect
  , holdFocus
  , releaseFocus
  , focusedWidget
  , requestFocus
  , focusNext
  , focusPrevious
  , clearFocus
  , isFocused
  , getClipboard
  , setClipboard
  , requestFrame
  , takeEscape
  , explainLayout
  , explainingLayout
  , explainedNode
  , explainScope
  , withArenaRange
  , inArenaRange
  , getScrollMetricsUi
  , setScrollOffsetUi
  , scrollToUi
  , scrollByUi
  , scrollPagesUi
  , scrollRectIntoViewUi
  , setScrollStepUi
  , damageWidgetNow
  , damageKeyNow
  , damageRectNow
  , damageGroupNow
  , damageFullNow
  , whenM
  , unlessM
  , ifM
  , (<&&>)
  )
where

import Control.Exception (bracket)
import Control.Monad (unless, when, (<$!>))
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bits ((.&.))
import Data.Hashable (Hashable, hash)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Monoid (Ap (..))
import Data.Text (Text)
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
import NanoUI.Internal.Context
import NanoUI.Internal.Draw.Types (TextFont (..))
import NanoUI.Internal.Font (FontMetrics, lineWidthIO, truncateTextIO)
import NanoUI.Internal.Frame.Node (resolveTextFont)
import NanoUI.Internal.Id hiding (currentId)
import NanoUI.Internal.Layout.Arena (arenaCount, getArenaScope, setArenaScope)
import NanoUI.Internal.Style (Appearance, FontStyle, FontVariant, FontWeight, Layout, TextDecoration (DecorationNone), Theme, defaultLayout)
import NanoUI.Internal.Input (Input (..), Key (KeyEscape), MouseButton, Pressable (..), inputMousePos, inputWindowSize, noButtons, stripInteractionInput)
import NanoUI.Internal.Types (DamageBounds, Rect, Size (..), V2)

-- | A view: widgets, layout, local state and IO. Backend runners execute it
-- as frames are needed; local-state changes can trigger a second pass within
-- a frame. Run IO in it with 'liftIO'; the action runs on every frame that
-- reaches it, so guard one-shot effects with a button or another event.
--
-- It is an @effectful@ computation in the row 'NanoUIEs'. A view never needs
-- to know that; "NanoUI.Effectful" has the constructor and the functions for
-- mixing views with other effects.
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

-- | Queue a typed message for the frame's reducer, in emission order.
{-# INLINE emit #-}
emit :: Typeable msg => msg -> NanoUI ()
emit msg = withContext (\ctx -> pushMessage ctx (FrameMsg msg))

-- | The id 'nextId' would issue, without consuming it.
{-# INLINE currentId #-}
currentId :: NanoUI WidgetId
currentId = withContext (fmap idContextWidgetId . readIORef . ctxIdContext)

-- | Consume the next sibling id. Widgets and positional hooks share this sequence,
-- so conditional calls need their own 'scope'.
{-# INLINE nextId #-}
nextId :: NanoUI WidgetId
nextId = withContext $ \ctx -> do
  ic <- readIORef (ctxIdContext ctx)
  writeIORef (ctxIdContext ctx) $! ic {siblingId = siblingId ic + 1}
  pure (idContextWidgetId ic)

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
  | otherwise = withContext $ \ctx ->
      modifyIORef' (ctxIdContext ctx) $ \ic ->
        let !sid = siblingId ic + fromIntegral n
         in ic {siblingId = sid}

-- | Run in the child context returned by @enter@, then restore its advanced
-- parent context, including when the action throws an exception.
{-# INLINE withIdFrame #-}
withIdFrame ::
  (IdContext -> (IdContext, IdContext)) -> NanoUI a -> NanoUI a
withIdFrame enter m = do
  ctx <- askContext
  withUiResource
    ( do
        old <- readIORef (ctxIdContext ctx)
        let
          !(!p, !c) = enter old
        writeIORef (ctxIdContext ctx) c
        pure p
    )
    (writeIORef (ctxIdContext ctx))
    m

-- | Give the action a child id sequence while consuming one parent id.
-- Put conditional content inside this scope to keep later siblings stable.
{-# INLINE scope #-}
scope :: NanoUI a -> NanoUI a
scope = withIdFrame (enterScope scopeTag)

-- | Run the action under a key so its widgets keep their ids and state when
-- earlier siblings change. Keys must be unique among siblings in the same
-- scope; use a stable item key for a list that can be reordered.
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

-- | The backend's default font size, used when a layout sets none. To scale
-- text relative to it, pass a multiple to 'NanoUI.Internal.Style.fontSize';
-- 'NanoUI.Internal.Style.fontSizeScale' alone scales from 16.
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
  withContext (\ctx -> fst <$> resolveTextFont ctx (TextFont size variant weight style DecorationNone))

-- | The advance of one line of text in these metrics, in logical pixels. Unlike
-- the pure 'NanoUI.Internal.Font.lineWidth' it first loads the glyphs the text needs,
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
  pushThemeScope ctx (outer .&. 1 /= 0) . f =<< scopeRawTheme ctx outer

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
      | otherwise = pushThemeScope ctx True =<< scopeRawTheme ctx outer

-- Run @m@ with the arena scope @enter@ picks, then restore the scope around it
-- (also on exceptions).
{-# INLINE withPaintScope #-}
withPaintScope :: (Context -> Int -> IO Int) -> NanoUI a -> NanoUI a
withPaintScope enter m = do
  ctx <- askContext
  let
    na = ctxNodeArena ctx
  withUiResource
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
-- > setUiTheme . lightDark defaultLightTheme defaultTheme =<< systemAppearance
--
-- This replaces a theme set by 'NanoUI.Internal.Context.followSystemTheme'.
{-# INLINE setUiTheme #-}
setUiTheme :: Theme -> NanoUI ()
setUiTheme th = withContext (\ctx -> setThemeInView ctx th)

-- | Whether the system prefers light or dark colours, or 'Nothing' when the
-- backend cannot tell (RGFW never can). A change repaints the whole window.
{-# INLINE systemAppearance #-}
systemAppearance :: NanoUI (Maybe Appearance)
systemAppearance = withContext getSystemAppearance

-- | Where the pointer is, as the view being declared sees it: far off every
-- widget while something drawn in front has the pointer.
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

-- | The frame's input before routing, pointer included whoever it belongs
-- to. For what watches the whole window rather than reacting to its own
-- events: a click anywhere outside dismissing a popup, a floating panel
-- working out its body's input. A widget that read its presses from this
-- would react through whatever is drawn over it, so widgets use 'askInput'.
{-# INLINE askFrameInput #-}
askFrameInput :: NanoUI Input
askFrameInput = do
  UiRep {repFrame = frame} <- NanoUI getStaticRep
  pure frame

-- | Run a part of the view with another routed input.
{-# INLINE localInput #-}
localInput :: Input -> NanoUI a -> NanoUI a
localInput inp (NanoUI m) = NanoUI (localStaticRep (\r -> r {repInput = inp}) m)

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

-- | Where a widget was laid out last frame, or 'Nothing' before its first.
-- A widget that works out its own input -- a list that scrolls itself, a
-- text view hit-testing a click -- reads the rect it will be given from here,
-- since this frame's is solved only after the view has run:
--
-- > wid <- nextId
-- > rect <- fromMaybe (Rect 0 0 320 240) <$> lastRect wid
-- > ... work out this frame from rect and the input ...
-- > customWidgetWithId wid spec
--
-- When this frame's layout moves or resizes the widget, another frame
-- follows, so the view gets to read the new rect.
lastRect :: WidgetId -> NanoUI (Maybe Rect)
lastRect wid = withContext $ \ctx -> do
  r <- getPrevRect ctx wid
  recordLayoutRead ctx (intKey wid) ((/= r) <$> getPrevRect ctx wid)
  pure r

-- | Give a widget the keyboard, without the focus ring Tab would draw round
-- it. A widget that should keep the keyboard while some condition holds calls
-- this every frame the condition does; nothing happens if it already has it.
--
-- Tab pressed that frame is the widget's: focus stays where it is, and an
-- editor reads the Tab from the input to indent. A click elsewhere still moves
-- focus off, until the next frame's call takes it back.
--
-- An open 'NanoUI.Internal.Widgets.Overlay.modal' keeps the keyboard inside it:
-- called outside one while it is up, this does nothing, so a widget behind
-- the modal cannot take the keys typed into it.
holdFocus :: WidgetId -> NanoUI ()
holdFocus wid = withContext $ \ctx -> do
  focus <- getFocusId ctx
  unlessM (pointerBlockedByModal ctx) $ do
    markTabConsumed ctx
    when (focus /= wid) $ do
      writeIORef (ctxFocusId ctx) wid
      writeIORef (ctxFocusVisible ctx) False

-- | Take the keyboard off a widget, if it has it; nothing then has focus.
-- This takes effect immediately, so widgets declared after the call see no
-- focus. A text field keeps its selection and menu. To drop focus the way a
-- click elsewhere does, at the end of the frame, use 'clearFocus'.
releaseFocus :: WidgetId -> NanoUI ()
releaseFocus wid = withContext $ \ctx -> do
  focus <- getFocusId ctx
  when (focus == wid) (writeIORef (ctxFocusId ctx) (WidgetId 0))

-- | The widget that has the keyboard, or @'WidgetId' 0@ for none.
focusedWidget :: NanoUI WidgetId
focusedWidget = withContext getFocusId

-- | Whether the widget with this id has the keyboard.
isFocused :: WidgetId -> NanoUI Bool
isFocused wid = (\focus -> hashWidgetId wid /= 0 && focus == wid) <$> focusedWidget

-- | Move the keyboard to the widget with this id, as Tab would. A text field
-- resumes with its caret where it was (at the end if never edited), the
-- widget shows the focus ring, and Tab continues from it. The previously
-- focused field drops its selection and menu, an open dropdown closes, and on
-- its next frame the field sees it lost focus (a combo box or search field
-- commits then).
--
-- > (resp, query') <- searchInput' "Find" query
-- > findPressed <- shortcut (ctrl <> key 'f')
-- > when findPressed (requestFocus (respId resp))
--
-- The request is applied at the end of the frame, against that frame's
-- layout, so it can name a widget declared later, such as the next one via
-- 'currentId':
--
-- > whenM (shortcut (ctrl <> key 'l')) (requestFocus =<< currentId)
-- > address' <- textInput address
--
-- Focus moves on the next frame, which the request schedules. The last
-- request in a frame wins ('focusNext', 'focusPrevious' and 'clearFocus' are
-- requests too). The request is ignored for a widget Tab would not stop at
-- this frame: one that is disabled, behind an open
-- 'NanoUI.Internal.Widgets.Overlay.modal', or not declared this frame. A radio
-- group's response names its last option, which is not a Tab stop.
-- Requesting the widget that already has focus is a no-op, so a view can
-- request it every frame. @'WidgetId' 0@ means 'clearFocus'.
requestFocus :: WidgetId -> NanoUI ()
requestFocus wid = askFocus (if hashWidgetId wid == 0 then FocusNowhere else FocusOn wid)

-- | Move focus to the next Tab stop at the end of the frame ('requestFocus').
focusNext :: NanoUI ()
focusNext = askFocus FocusNext

-- | Move focus to the previous Tab stop at the end of the frame
-- ('requestFocus').
focusPrevious :: NanoUI ()
focusPrevious = askFocus FocusPrevious

-- | Drop focus at the end of the frame ('requestFocus'), as a click outside
-- any text field or select does. The focused field drops its selection and
-- menu. 'releaseFocus' instead acts at once on one widget.
clearFocus :: NanoUI ()
clearFocus = askFocus FocusNowhere

askFocus :: FocusRequest -> NanoUI ()
askFocus req = withContext (\ctx -> writeIORef (ctxFocusRequest ctx) (Just req))

-- | The clipboard's text, through whatever clipboard the backend installed.
-- 'Nothing' for an empty clipboard or none at all.
getClipboard :: NanoUI (Maybe Text)
getClipboard = withContext ctxClipboardGet

-- | Put text on the clipboard. 'False' when the backend could not.
setClipboard :: Text -> NanoUI Bool
setClipboard txt = withContext (\ctx -> ctxClipboardSet ctx txt)

-- | Ask for another frame after this one. For a view whose state lives
-- outside nano-ui and changed after the part showing it was declared: the
-- frame that shows the change has to be asked for, since nothing nano-ui
-- keeps says it is due.
--
-- Ask only when something did change. The frame asked for repaints the whole
-- window, since nothing nano-ui keeps says which pixels the change touched,
-- and a view that asks every frame keeps the loop from ever sleeping;
-- 'NanoUI.Internal.Widgets.Animate.wakeAfter' asks for a frame at a later time.
requestFrame :: NanoUI ()
requestFrame = withContext markDirty

-- | Whether Escape was pressed this frame and is the view's to act on, and
-- if so, take it: nothing after this sees it either. 'False' when something
-- earlier in the frame took it, and while a text field's right-click menu or
-- a dropdown is open, since that Escape is for closing it. A dialog that
-- Escape puts away reads it here rather than from the input, so the Escape
-- that closes a menu inside it does not close the dialog as well.
takeEscape :: NanoUI Bool
takeEscape = do
  inp <- askInput
  if not (pressedOnceIn KeyEscape inp)
    then pure False
    else withContext $ \ctx -> do
      taken <- overlayConsumesQuit ctx inp
      menu <- getsInteraction ctx isTextInputMenu
      dropdown <- anySelectOpen <$> getStore ctx
      let ours = not taken && null menu && not dropdown
      when ours (markEscapeConsumed ctx)
      pure ours

-- | Turn the layout overlay on or off. It outlines every layout node one pixel
-- inside its edge, coloured by depth and drawn in the node's layer, and tints
-- the node under the pointer with its content box outlined. It does not
-- affect layout or input. A change repaints the whole window; setting the
-- current value is a no-op, so a view can call this every frame:
--
-- > explainLayout =<< checkbox "Outline layout nodes" =<< explainingLayout
--
-- Backend options can enable it at startup (@sdlExplainLayout@,
-- @optExplainLayout@); on your own context use 'setExplainLayout' from
-- "NanoUI.Backend".
explainLayout :: Bool -> NanoUI ()
explainLayout on = withContext (\ctx -> setExplainLayout ctx on)

-- | Whether the layout overlay is on ('explainLayout').
explainingLayout :: NanoUI Bool
explainingLayout = withContext getExplainLayout

-- | The node under the pointer while the layout overlay is on, as laid out
-- last frame: its kind, rect and padding. 'Nothing' when the overlay is off
-- or the pointer is over no node. A change requests a frame, so a debug panel
-- showing it keeps up with the pointer.
explainedNode :: NanoUI (Maybe ExplainedNode)
explainedNode = withContext getExplainedNode

-- | Limit the layout overlay ('explainLayout') to the nodes the body adds and
-- their descendants. With several scopes, all of them are shown; with none,
-- the whole view is. With the overlay off this just runs the body, so it can
-- stay in a view:
--
-- > explainScope (settingsPanel model)
explainScope :: NanoUI a -> NanoUI a
explainScope body = do
  ctx <- askContext
  on <- liftIO (getExplainLayout ctx)
  if not on
    then body
    else withArenaRange (\from below -> modifyIORef' (ctxExplain ctx) (\es -> es {esScopes = (from, below) : esScopes es})) body

-- | Run @body@, then pass @record@ the range of arena indices it added,
-- @from@ up to but not including @below@. The arena appends nodes in
-- declaration order and a subtree follows its root, so the range covers
-- exactly the nodes the body declared and their descendants.
withArenaRange :: (Int -> Int -> IO ()) -> NanoUI a -> NanoUI a
withArenaRange record body = do
  ctx <- askContext
  let count = liftIO (arenaCount (ctxNodeArena ctx))
  !from <- count
  a <- body
  !below <- count
  liftIO (record from below)
  pure a

-- | Whether arena index @idx@ is in a range 'withArenaRange' recorded.
{-# INLINE inArenaRange #-}
inArenaRange :: Int -> Int -> Int -> Bool
inArenaRange from below idx = from <= idx && idx < below

-- | The scroller's geometry as its last layout left it (its viewport, range
-- and offset), or 'Nothing' before it has been laid out. The id is the one a
-- 'NanoUI.scrollArea' hands back; to run a command before the scroller is
-- declared, take its id with 'currentId' first:
--
-- > sid <- currentId
-- > metrics <- getScrollMetricsUi sid
-- > (_, rows) <- scrollArea (fillW . fillH) (visibleRows metrics)
--
-- When this frame's layout changes the metrics, another frame follows, so
-- the view gets to read them again.
getScrollMetricsUi :: WidgetId -> NanoUI (Maybe ScrollMetrics)
getScrollMetricsUi wid = withContext (`getScrollMetrics` wid)

-- | Put a scroller at an offset in window axes, cancelling a glide; a 1D
-- scroller ignores the axis it does not scroll on. Unlike
-- 'scrollToUi' the offset is not held to the range the last layout found,
-- so it can place content this frame is about to lay out; the layout holds
-- it to the content's real range.
setScrollOffsetUi :: WidgetId -> V2 -> NanoUI ()
setScrollOffsetUi wid off = withContext $ \ctx ->
  readScrollMetrics ctx wid >>= \case
    Just m -> setScrollOffsetIn ctx wid (scrollAxes m) off
    Nothing -> setScrollOffset2D ctx wid off

-- | Scroll to an offset, held to the scroller's range.
scrollToUi :: WidgetId -> V2 -> ScrollBehavior -> NanoUI ()
scrollToUi wid off behavior = withContext (\ctx -> scrollTo ctx wid off behavior)

-- | Scroll by a delta in pixels.
scrollByUi :: WidgetId -> V2 -> ScrollBehavior -> NanoUI ()
scrollByUi wid delta behavior = withContext (\ctx -> scrollBy ctx wid delta behavior)

-- | Scroll by whole viewports: @V2 0 0.5@ is half a page down.
scrollPagesUi :: WidgetId -> V2 -> ScrollBehavior -> NanoUI ()
scrollPagesUi wid pages behavior = withContext (\ctx -> scrollPages ctx wid pages behavior)

-- | Scroll a rectangle of the content, in content coordinates, into view:
-- the row a keyboard selection moved to in a list that builds only the rows
-- it shows.
scrollRectIntoViewUi :: WidgetId -> Rect -> ScrollAlign -> ScrollBehavior -> NanoUI ()
scrollRectIntoViewUi wid r align behavior = withContext (\ctx -> scrollRectIntoView ctx wid r align behavior)

-- | Give a scroller its own wheel step, in pixels a notch; @0@ puts it back on
-- the app's.
setScrollStepUi :: WidgetId -> Float -> NanoUI ()
setScrollStepUi wid px = withContext (\ctx -> setScrollStep ctx wid px)

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
