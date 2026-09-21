{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'Ui' effect and the 'NanoUI' view type: running a view, widget id
-- scopes and keys, theme scopes, and damage requests from inside a view.
module NanoUI.Monad
  ( NanoUI
  , NanoUIEs
  , Ui
  , runNanoUI
  , runUi
  , uiIO
  , withContext
  , withUiResource
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
  , resolveFontUi
  , lineWidthUi
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
  , lastRect
  , holdFocus
  , releaseFocus
  , focusedWidget
  , getClipboard
  , setClipboard
  , requestFrame
  , takeEscape
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
  , getFocusId
  , getPrevRect
  , getScrollMetrics
  , getStore
  , getTextInputMenu
  , anySelectOpen
  , markDirty
  , markEscapeConsumed
  , markTabConsumed
  , overlayConsumesQuit
  , scrollBy
  , scrollPages
  , scrollRectIntoView
  , scrollTo
  , setScrollOffset2D
  , setScrollOffsetIn
  , setScrollStep
  , ScrollAlign
  , ScrollBehavior
  , ScrollMetrics (..)
  , pointerBlockedByModal
  , routedInput
  , currentTheme
  , pushMessage
  , pushThemeScope
  , scopeRawTheme
  , setTheme
  , reduceMessages
  , reduceUpdates
  )
import NanoUI.Draw.Types (TextFont (..))
import NanoUI.Font (FontMetrics, lineWidthIO)
import NanoUI.Frame.Node (resolveTextFont)
import NanoUI.Id
  ( IdContext (siblingId)
  , WidgetId (..)
  , enterKeyed
  , enterScope
  , idContextWidgetId
  , scopeTag
  )
import NanoUI.Layout.Arena (getArenaScope, setArenaScope)
import NanoUI.Style (FontStyle, FontVariant, FontWeight, Layout, TextDecoration (DecorationNone), Theme, disabledTheme)
import NanoUI.Input (Input (..), Key (KeyEscape), inputKeysElem, inputMousePos, inputWindowSize, stripInteractionInput)
import NanoUI.Types (DamageBounds, Rect, Size (..), V2)

-- | A view with UI operations and IO. Backend runners execute it as frames
-- are needed; local-state changes can trigger a second pass within a frame.
type NanoUI = Eff NanoUIEs

-- | The effect row a 'NanoUI' view runs in. Name it where a widget's
-- configuration carries its caller's row, as @PaneGridConfig NanoUIEs@ does.
type NanoUIEs = '[Ui, IOE]

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

-- | Acquire UI-thread state, run an action, and restore it even on exceptions.
-- Acquisition and release are masked, as in 'bracket'; the view inherits the
-- caller's masking state.
{-# INLINE withUiResource #-}
withUiResource :: Ui :> es => IO a -> (a -> IO ()) -> Eff es b -> Eff es b
withUiResource acquire release action = do
  UiRep {} <- getStaticRep
  unsafeEff $ \es -> bracket acquire release (\_ -> unEff action es)

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

-- | Metrics for text at a size, weight, style and variant, resolved through
-- the backend's fonts: what a custom widget measures and places its text by
-- when it draws in a font other than the context's.
--
-- It resolves the font as a 'DrawTextStyled' op naming the same size,
-- weight, style and variant is painted, so text measured with these metrics
-- is drawn at the width it was measured at.
resolveFontUi :: Ui :> es => Float -> FontWeight -> FontStyle -> FontVariant -> Eff es FontMetrics
resolveFontUi size weight style variant =
  withContext (\ctx -> fst <$> resolveTextFont ctx (TextFont size variant weight style DecorationNone))

-- | The advance of one line of text in these metrics, in logical pixels. Unlike
-- the pure 'NanoUI.Font.lineWidth' it first loads the glyphs the text needs,
-- so it is right for metrics that have not drawn this text yet.
lineWidthUi :: Ui :> es => FontMetrics -> Text -> Eff es Float
lineWidthUi fm txt = uiIO (lineWidthIO fm txt)

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

-- | Where a widget was laid out last frame, or 'Nothing' before its first.
-- A widget that works out its own input -- a list that scrolls itself, a
-- text view hit-testing a click -- reads the rect it will be given from here,
-- since this frame's is solved only after the view has run:
--
-- > wid <- nextId
-- > rect <- fromMaybe (Rect 0 0 320 240) <$> lastRect wid
-- > ... work out this frame from rect and the input ...
-- > customWidgetWithId wid spec
lastRect :: Ui :> es => WidgetId -> Eff es (Maybe Rect)
lastRect wid = withContext (\ctx -> getPrevRect ctx wid)

-- | Give a widget the keyboard, without the focus ring Tab would draw round
-- it. A widget that should keep the keyboard while some condition holds calls
-- this every frame the condition does; nothing happens if it already has it.
--
-- Tab pressed that frame is the widget's: focus stays where it is, and an
-- editor reads the Tab from the input to indent. A click elsewhere still moves
-- focus off, until the next frame's call takes it back.
--
-- An open 'NanoUI.Widgets.Overlay.modal' keeps the keyboard inside it:
-- called outside one while it is up, this does nothing, so a widget behind
-- the modal cannot take the keys typed into it.
holdFocus :: Ui :> es => WidgetId -> Eff es ()
holdFocus wid = withContext $ \ctx -> do
  focus <- getFocusId ctx
  behindModal <- pointerBlockedByModal ctx
  unless behindModal $ do
    markTabConsumed ctx
    when (focus /= wid) $ do
      writeIORef (ctxFocusId ctx) wid
      writeIORef (ctxFocusVisible ctx) False

-- | Take the keyboard off a widget, if it has it; nothing then has focus.
releaseFocus :: Ui :> es => WidgetId -> Eff es ()
releaseFocus wid = withContext $ \ctx -> do
  focus <- getFocusId ctx
  when (focus == wid) (writeIORef (ctxFocusId ctx) (WidgetId 0))

-- | The widget that has the keyboard, or @'WidgetId' 0@ for none.
focusedWidget :: Ui :> es => Eff es WidgetId
focusedWidget = withContext getFocusId

-- | The clipboard's text, through whatever clipboard the backend installed.
-- 'Nothing' for an empty clipboard or none at all.
getClipboard :: Ui :> es => Eff es (Maybe Text)
getClipboard = withContext ctxClipboardGet

-- | Put text on the clipboard. 'False' when the backend could not.
setClipboard :: Ui :> es => Text -> Eff es Bool
setClipboard txt = withContext (\ctx -> ctxClipboardSet ctx txt)

-- | Ask for another frame after this one. For a view whose state lives
-- outside nano-ui and changed after the part showing it was declared: the
-- frame that shows the change has to be asked for, since nothing nano-ui
-- keeps says it is due.
--
-- Ask only when something did change. The frame asked for repaints the whole
-- window, and a view that asks every frame keeps the loop from ever sleeping;
-- 'NanoUI.Widgets.Animate.wakeAfter' asks for a frame at a later time.
requestFrame :: Ui :> es => Eff es ()
requestFrame = withContext markDirty

-- | Whether Escape was pressed this frame and is the view's to act on, and
-- if so, take it: nothing after this sees it either. 'False' when something
-- earlier in the frame took it, and while a text field's right-click menu or
-- a dropdown is open, since that Escape is for closing it. A dialog that
-- Escape puts away reads it here rather than from the input, so the Escape
-- that closes a menu inside it does not close the dialog as well.
takeEscape :: Ui :> es => Eff es Bool
takeEscape = do
  inp <- askInput
  if not (inputKeysElem KeyEscape (inputKeys inp))
    then pure False
    else withContext $ \ctx -> do
      taken <- overlayConsumesQuit ctx inp
      menu <- getTextInputMenu ctx
      dropdown <- anySelectOpen <$> getStore ctx
      let ours = not taken && null menu && not dropdown
      when ours (markEscapeConsumed ctx)
      pure ours

-- | The scroller's geometry as its last layout left it (its viewport, range
-- and offset), or 'Nothing' before it has been laid out. The id is the one a
-- 'NanoUI.scrollArea' hands back; to run a command before the scroller is
-- declared, take its id with 'currentId' first:
--
-- > sid <- currentId
-- > metrics <- getScrollMetricsUi sid
-- > (_, rows) <- scrollArea (fillW . fillH) (visibleRows metrics)
getScrollMetricsUi :: Ui :> es => WidgetId -> Eff es (Maybe ScrollMetrics)
getScrollMetricsUi wid = withContext (\ctx -> getScrollMetrics ctx wid)

-- | Put a scroller at an offset in window axes, cancelling a glide; a 1D
-- scroller ignores the axis it does not scroll on. Unlike
-- 'scrollToUi' the offset is not held to the range the last layout found,
-- so it can place content this frame is about to lay out; the layout holds
-- it to the content's real range.
setScrollOffsetUi :: Ui :> es => WidgetId -> V2 -> Eff es ()
setScrollOffsetUi wid off = withContext $ \ctx ->
  getScrollMetrics ctx wid >>= \case
    Just m -> setScrollOffsetIn ctx wid (scrollAxes m) off
    Nothing -> setScrollOffset2D ctx wid off

-- | Scroll to an offset, held to the scroller's range.
scrollToUi :: Ui :> es => WidgetId -> V2 -> ScrollBehavior -> Eff es ()
scrollToUi wid off behavior = withContext (\ctx -> scrollTo ctx wid off behavior)

-- | Scroll by a delta in pixels.
scrollByUi :: Ui :> es => WidgetId -> V2 -> ScrollBehavior -> Eff es ()
scrollByUi wid delta behavior = withContext (\ctx -> scrollBy ctx wid delta behavior)

-- | Scroll by whole viewports: @V2 0 0.5@ is half a page down.
scrollPagesUi :: Ui :> es => WidgetId -> V2 -> ScrollBehavior -> Eff es ()
scrollPagesUi wid pages behavior = withContext (\ctx -> scrollPages ctx wid pages behavior)

-- | Scroll a rectangle of the content, in content coordinates, into view:
-- the row a keyboard selection moved to in a list that builds only the rows
-- it shows.
scrollRectIntoViewUi :: Ui :> es => WidgetId -> Rect -> ScrollAlign -> ScrollBehavior -> Eff es ()
scrollRectIntoViewUi wid r align behavior = withContext (\ctx -> scrollRectIntoView ctx wid r align behavior)

-- | Give a scroller its own wheel step, in pixels a notch; @0@ puts it back on
-- the app's.
setScrollStepUi :: Ui :> es => WidgetId -> Float -> Eff es ()
setScrollStepUi wid px = withContext (\ctx -> setScrollStep ctx wid px)

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
