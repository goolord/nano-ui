{-# LANGUAGE TypeFamilies #-}

-- | Implementation of "NanoUI.Monad", plus running a view against a context,
-- reading and swapping the context, id-frame plumbing, and the frame's
-- message queue.
module NanoUI.Internal.Monad
  ( NanoUI
  , NanoUIEs
  , Ui
  , runNanoUI
  , runUi
  , embedNanoUI
  , uiIO
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
  , requestFocus
  , getClipboard
  , setClipboard
  , requestFrame
  , takeEscape
  , explainLayout
  , explainingLayout
  , explainedNode
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
import Data.Bits ((.&.))
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
import NanoUI.Internal.Context
import NanoUI.Internal.Draw.Types (TextFont (..))
import NanoUI.Internal.Font (FontMetrics, lineWidthIO)
import NanoUI.Internal.Frame.Node (resolveTextFont)
import NanoUI.Internal.Id hiding (currentId)
import NanoUI.Internal.Layout.Arena (getArenaScope, setArenaScope)
import NanoUI.Internal.Style (FontStyle, FontVariant, FontWeight, Layout, TextDecoration (DecorationNone), Theme, defaultLayout)
import NanoUI.Internal.Input (Input (..), Key (KeyEscape), inputKeysElem, inputMousePos, inputWindowSize, stripInteractionInput)
import NanoUI.Internal.Types (DamageBounds, Rect, Size (..), V2)

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

-- | Run 'runUi' in IO for the standard 'NanoUI' effect stack.
{-# INLINE runNanoUI #-}
runNanoUI :: Context -> Input -> NanoUI a -> IO a
runNanoUI ctx inp = runEff . runUi ctx inp

-- | Run a 'NanoUI' view as part of a view in any effect row with 'Ui', where
-- it declares its widgets as if written there: same context, input, layout
-- defaults and widget ids.
embedNanoUI :: Ui :> es => NanoUI a -> Eff es a
embedNanoUI view = do
  rep <- getStaticRep
  unsafeEff_ (runEff (evalStaticRep rep view))

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
withContext f = getStaticRep >>= \r -> unsafeEff_ (f $! repContext r)

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
currentId = withContext (fmap idContextWidgetId . readIORef . ctxIdContext)

-- | Consume the next sibling id. Widgets and state hooks share this sequence,
-- so conditional calls need their own 'scope'.
{-# INLINE nextId #-}
nextId :: Ui :> es => Eff es WidgetId
nextId = withContext $ \ctx -> do
  ic <- readIORef (ctxIdContext ctx)
  writeIORef (ctxIdContext ctx) $! ic {siblingId = siblingId ic + 1}
  pure (idContextWidgetId ic)

-- | The next sibling id ('nextId') and the view's 'Context', which a widget's body starts from.
{-# INLINE freshWidget #-}
freshWidget :: Ui :> es => Eff es (WidgetId, Context)
freshWidget = do
  wid <- nextId
  ctx <- askContext
  pure (wid, ctx)

-- | Reserve @n@ sibling ids without returning them. Non-positive counts do nothing.
{-# INLINE burstNextIds #-}
burstNextIds :: Ui :> es => Int -> Eff es ()
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

-- | Run the action under a key, so its widgets keep their ids and state
-- whatever comes before them. Keys must be unique among siblings in the same
-- scope; use a stable item key when a list can be reordered.
{-# INLINE withKey #-}
withKey :: (Hashable k, Ui :> es) => k -> Eff es a -> Eff es a
withKey k = keyedTag (fromIntegral (hash k))

-- | A keyed child scope using a precomputed 64-bit tag. Tags must be unique
-- among siblings; use 'withKey' to hash an application key.
{-# INLINE keyedTag #-}
keyedTag :: Ui :> es => Word64 -> Eff es a -> Eff es a
keyedTag tag = withIdFrame (enterKeyed tag)

-- | The mutable context for this view. It belongs to the current UI session.
{-# INLINE askContext #-}
askContext :: Ui :> es => Eff es Context
askContext = repContext <$!> getStaticRep

-- | Layout defaults in the current 'withDefaultLayout' scope.
{-# INLINE askDefaultLayout #-}
askDefaultLayout :: Ui :> es => Eff es Layout
askDefaultLayout = repLayout <$!> getStaticRep

-- | Modify layout defaults for the enclosed action, restoring them on exit.
{-# INLINE withDefaultLayout #-}
withDefaultLayout :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
withDefaultLayout f = localStaticRep (\r -> r {repLayout = f (repLayout r)})

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
-- the pure 'NanoUI.Internal.Font.lineWidth' it first loads the glyphs the text needs,
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
styled f = withPaintScope $ \ctx outer ->
  pushThemeScope ctx (outer .&. 1 /= 0) . f =<< scopeRawTheme ctx outer

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
    (\r -> r {repInput = inert (repInput r), repFrame = inert (repFrame r)})
    (withPaintScope enter m)
  where
    inert i =
      (stripInteractionInput i)
        { inputMouseDown = False
        , inputMouseRightDown = False
        , inputMouseMiddleDown = False
        }
    enter ctx outer
      | outer .&. 1 /= 0 = pure outer
      | otherwise = pushThemeScope ctx True =<< scopeRawTheme ctx outer

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
askInput = repInput <$!> getStaticRep

-- | The frame's input before routing, pointer included whoever it belongs
-- to. For what watches the whole window rather than reacting to its own
-- events: a click anywhere outside dismissing a popup, a floating panel
-- working out its body's input. A widget that read its presses from this
-- would react through whatever is drawn over it, so widgets use 'askInput'.
{-# INLINE askFrameInput #-}
askFrameInput :: Ui :> es => Eff es Input
askFrameInput = do
  UiRep {repFrame = frame} <- getStaticRep
  pure frame

-- | Run a part of the view with another routed input.
{-# INLINE localInput #-}
localInput :: Ui :> es => Input -> Eff es a -> Eff es a
localInput inp = localStaticRep (\r -> r {repInput = inp})

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
-- An open 'NanoUI.Internal.Widgets.Overlay.modal' keeps the keyboard inside it:
-- called outside one while it is up, this does nothing, so a widget behind
-- the modal cannot take the keys typed into it.
holdFocus :: Ui :> es => WidgetId -> Eff es ()
holdFocus wid = withContext $ \ctx -> do
  focus <- getFocusId ctx
  unlessM (pointerBlockedByModal ctx) $ do
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

-- | Move the keyboard to the widget with this id, as Tab moving onto it
-- would: a text field starts taking keys with its caret where it last left
-- it (at the end of one not yet edited), the widget shows the focus ring,
-- and Tab goes on from it. The field that had the keyboard drops its
-- selection and menu, and commits on its next frame, as it does when a click
-- lands elsewhere. @'WidgetId' 0@, no widget, takes the keyboard off
-- whatever has it. For a search box that Ctrl+F sends the keys to:
--
-- > (resp, query') <- searchInput' "Find" query
-- > when findPressed (requestFocus (respId resp))
--
-- The request is carried out at the end of the frame, against the frame's
-- layout, so a widget declared after the call can be named too; the widget
-- has the keyboard from the next frame, which the request asks for. The
-- last request of a frame wins. A widget that Tab would not stop at this
-- frame refuses it and focus stays where it was: one that is disabled, one
-- behind an open 'NanoUI.Internal.Widgets.Overlay.modal', one not declared
-- this frame, or a radio group's last option, which its response names.
-- Asking for the widget that already has the keyboard changes nothing, so a
-- view can ask on every frame a condition holds.
requestFocus :: Ui :> es => WidgetId -> Eff es ()
requestFocus wid = withContext (\ctx -> writeIORef (ctxFocusRequest ctx) (Just wid))

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
-- window, since nothing nano-ui keeps says which pixels the change touched,
-- and a view that asks every frame keeps the loop from ever sleeping;
-- 'NanoUI.Internal.Widgets.Animate.wakeAfter' asks for a frame at a later time.
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
      menu <- getsInteraction ctx isTextInputMenu
      dropdown <- anySelectOpen <$> getStore ctx
      let ours = not taken && null menu && not dropdown
      when ours (markEscapeConsumed ctx)
      pure ours

-- | Turn the layout overlay on or off: a one-pixel outline just inside every
-- layout node, coloured by its depth, over the layer the node is in, and a
-- tint on the node under the pointer with its content box outlined. It is
-- for seeing how a view was laid out; layout and input never see it. Turning
-- it on or off repaints the whole window, and calling this with what is
-- already set does nothing, so a view can call it every frame:
--
-- > explainLayout =<< checkbox "Outline layout nodes" =<< explainingLayout
--
-- A backend's options can turn it on from the start (@sdlExplainLayout@,
-- @optExplainLayout@), and 'setExplainLayout' in "NanoUI.Backend" on a
-- context of your own.
explainLayout :: Ui :> es => Bool -> Eff es ()
explainLayout on = withContext (\ctx -> setExplainLayout ctx on)

-- | Whether the layout overlay is on ('explainLayout').
explainingLayout :: Ui :> es => Eff es Bool
explainingLayout = withContext getExplainLayout

-- | The node under the pointer while the layout overlay is on, as the last
-- frame laid it out: what it is, where, and its padding. 'Nothing' with the
-- overlay off or the pointer over no node. A debug panel shows it; a change
-- asks for a frame of its own, so the panel keeps up with the pointer.
explainedNode :: Ui :> es => Eff es (Maybe ExplainedNode)
explainedNode = withContext getExplainedNode

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
