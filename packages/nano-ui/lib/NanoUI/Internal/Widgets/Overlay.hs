-- | Modal dialogs and panels, and floating in-app windows.
module NanoUI.Internal.Widgets.Overlay
  ( modal
  , modalWith
  , modalPanel
  , modalPanelWith
  , window
  , windowTitleBarH
  , windowChromeSepH
  )
where

import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Context
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (NodeType (..), addNodeFromLayout)
import NanoUI.Internal.Monad
import NanoUI.Internal.Store (fieldPoint, lookupSlot)
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Rect (..), Size (..), clamp, rectNonEmpty)
import NanoUI.Internal.WidgetText (buttonCloseTrailing, buttonFlagClose)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx)
import NanoUI.Internal.Widgets.Popup (floatingOverlay)
import NanoUI.Internal.Widgets.Layout hiding (panel)
import NanoUI.Internal.Widgets.Node

-- | A window's chrome above its body: the title bar, the 10px above it and
-- the rule under it, which the frame paints ('windowChromeSepH').
windowTitleBarH :: Float
windowTitleBarH = 28 + 10 + windowChromeSepH

windowChromeSepH :: Float
windowChromeSepH = 1

-- | Show a modal while the first argument is true, blocking interaction with
-- content behind it. Returns a close-request response and the body's result;
-- the result is 'Nothing' while closed. The caller owns the open flag.
modal :: Bool -> Text -> NanoUI a -> NanoUI (Response, Maybe a)
modal = overlay True True id

-- | 'modal' with a layout modifier for its panel, which by default fits its
-- body. The title bar, the rule under it and the padding are the panel's
-- own. A panel given a fixed height holds its body rather than scrolling it,
-- and the body fills what the title bar leaves, so a body laid out with
-- @fillW . fillH@ takes exactly the panel's inside:
--
-- > winW <- windowWidth
-- > winH <- windowHeight
-- > modalWith (fixedWH (winW - 40) (winH - 40)) open "Find" $
-- >   columnWith (fillW . fillH) body
--
-- A panel is never larger than the window, less the margin every floating
-- panel keeps from its edge.
modalWith :: (Layout -> Layout) -> Bool -> Text -> NanoUI a -> NanoUI (Response, Maybe a)
modalWith = overlay True True

-- | A modal panel without a title bar, close button or surrounding padding.
-- The panel surface and modal backdrop remain; Escape or a click outside
-- reports a close request. The caller owns the open flag.
modalPanel :: Bool -> NanoUI a -> NanoUI (Response, Maybe a)
modalPanel = modalPanelWith id

-- | 'modalPanel' with a layout modifier for its panel. A fixed-size panel's
-- body fills the panel, so a body laid out with @fillW . fillH@ takes exactly
-- the panel's inside. Use a small amount of padding only when the body should
-- sit clear of the panel's border.
modalPanelWith :: (Layout -> Layout) -> Bool -> NanoUI a -> NanoUI (Response, Maybe a)
modalPanelWith shape open = overlay True False shape open ""

-- | Show a draggable, resizable in-app window with a scrolling body. Like
-- 'modal', the response reports a close request and the caller updates the
-- open flag. Other windows and the page remain interactive outside its bounds.
window :: Bool -> Text -> NanoUI a -> NanoUI (Response, Maybe a)
window = overlay False True id

-- | A modal (@isModal@) or a window.
overlay ::
  Bool -> Bool -> (Layout -> Layout) -> Bool -> Text -> NanoUI a -> NanoUI (Response, Maybe a)
overlay isModal hasChrome shape open title child = do
  ctx <- askContext
  inp <- askInput
  let
    Size winW winH = inputWindowSize inp
    margin = windowMargin
    availW = max 1 (winW - 2 * margin)
    availH = max 1 (winH - 2 * margin)
    -- Chrome-bearing modals share the window's side padding. The body's
    -- scrollbar sits out in it just inside the panel's edge, that padding
    -- from the content. A bare modal panel has no padding of its own.
    padding
      | not hasChrome = Padding 0 0 0 0
      | isModal = windowPad {padB = 12}
      | otherwise = windowPad
    barH
      | not hasChrome = 0
      | isModal = 40
      | otherwise = windowTitleBarH
    -- Windows keep a side-pad between the chrome and the body. Standard
    -- modals keep their larger gap; a bare modal panel has none.
    bodyGap
      | not hasChrome = 0
      | isModal = 8
      | otherwise = 10
    minWidth = clamp 1 availW (if isModal then 260 else 280)
    minHeight =
      if isModal
        then 0
        else
          min availH (padT padding + windowTitleBarH + bodyGap + padB padding)
    -- The panel's own layout, as the caller shapes it. Its sizes are held
    -- inside the window, whatever the caller asked for.
    panel =
      shape
        defaultLayout
          { layoutDirection = Column
          , layoutWidth = Fit
          , layoutHeight = Fit
          , layoutPadding = padding
          , layoutGap = bodyGap
          , layoutMinW = minWidth
          , layoutMinH = minHeight
          , layoutMaxW = availW
          , layoutMaxH = availH
          , layoutAlignX = AlignStart
          , layoutAlignY = AlignTop
          }
    within avail = \case
      Fixed v -> Fixed (min avail v)
      sz -> sz
    panelW = within availW (layoutWidth panel)
    panelH = within availH (layoutHeight panel)
    -- A panel of a fixed size is seeded at that size, so its first frame is
    -- already where it stays.
    seedW = case panelW of Fixed v -> v; _ -> minWidth
    seedH = case panelH of Fixed v -> v; _ -> minHeight
    addOverlayNode _ parent =
      addNodeFromLayout
        (ctxNodeArena ctx)
        (if isModal then NodeModal else NodeWindow)
        parent
        panel
          { layoutWidth = panelW
          , layoutHeight = panelH
          , layoutMinW = min availW (layoutMinW panel)
          , layoutMinH = min availH (layoutMinH panel)
          , layoutMaxW = min availW (layoutMaxW panel)
          , layoutMaxH = min availH (layoutMaxH panel)
          }
    enter wid = do
      when isModal (beginModal ctx)
      seedFloatingPanel ctx wid =<< seedRect wid
    -- Last frame's rect, else the stored position and size, else centred
    -- (modal) or at the window's top-right corner.
    seedRect wid = do
      mPrev <- getPrevRect ctx wid
      case mPrev of
        Just r | rectNonEmpty r -> pure r
        _ -> do
          store <- getStore ctx
          let
            k = intKey wid
            pos = lookupSlot fieldPoint k store
            sz = lookupSlot fieldPoint (slotKey SlotWinSize k) store
            h1 = max seedH 1
          pure $
            case (pos, sz) of
              (Just (x, y), Just (w, h)) | w > 0 && h > 0 -> Rect x y w h
              (Just (x, y), _) -> Rect x y seedW h1
              _
                | isModal -> Rect ((winW - seedW) / 2) ((winH - h1) / 2) seedW h1
                | otherwise -> Rect (max 0 (winW - seedW - margin)) margin seedW h1
    titleLayout =
      (fixedH barH . alignMid . tight) defaultLayout {layoutMinH = barH, layoutMaxH = barH}
  floatingOverlay open isModal addOverlayNode enter $ do
    closed <-
      if hasChrome
        then do
          close <-
            row' (tight . gap 6 . alignMid . fixedH barH . fillW $ defaultLayout) $ do
              unless (T.null title) $
                (if isModal then id else withKey title) (void (labelEx titleLayout title))
              flex
              withKey ("close" :: Text) $
                buttonStyledEx True "" 0 (tight . fixedWH 24 24 . alignMid $ defaultLayout) $
                  buttonFlagClose .|. buttonCloseTrailing
          when (isModal && not (T.null title)) separator
          pure (respClicked close)
        else pure False
    -- A panel of a fixed height holds its body, which fills what the title
    -- bar leaves when there is one; any other panel scrolls a body taller
    -- than the window.
    r <- case panelH of
      Fixed _ -> columnWith (tight . grow . fillW) child
      _ -> scrollWith (tight . grow) child
    when isModal (liftIO (endModal ctx))
    pure (closed, r)
