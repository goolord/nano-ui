-- | Modal dialogs and floating in-app windows with title bars and scrolling bodies.
module NanoUI.Internal.Widgets.Overlay
  ( modal
  , modalWith
  , window
  )
where

import Control.Monad (unless, void, when)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( Context (..)
  , beginModal
  , endModal
  , getPrevRect
  , getStore
  , intKey
  , seedFloatingPanel
  )
import NanoUI.Internal.Input
  ( inputWindowSize
  )
import NanoUI.Internal.Layout.Arena (NodeType (..), addNode)
import NanoUI.Internal.Monad
  ( Ui
  , askContext
  , askInput
  , uiIO
  , withKey
  )
import NanoUI.Internal.Store (Slot (..), fieldPoint, lookupSlot, slotKey)
import NanoUI.Internal.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , defaultLayout
  , fillW
  , grow
  , padB
  , padT
  , tight
  , windowMargin
  , windowPad
  )
import NanoUI.Internal.Types (Rect (..), Size (..), clamp, rectNonEmpty)
import NanoUI.Internal.Widgets.Chrome
  ( closeButton
  , modalTitleBarH
  , titleBarChromeHFor
  , titleBarLayoutFor
  , titleLabelLayoutFor
  )
import NanoUI.Internal.Widgets.Popup (floatingOverlay)
import NanoUI.Internal.Widgets.Layout
  ( columnWith
  , flex
  , labelEx
  , row'
  , scrollWith
  , separator
  )
import NanoUI.Internal.Widgets.Node
  ( Response (..)
  , respClicked
  )

data OverlayKind
  = ModalOverlay
  | WindowOverlay
  deriving Eq

-- | Show a modal while the first argument is true, blocking interaction with
-- content behind it. Returns a close-request response and the body's result;
-- the result is 'Nothing' while closed. The caller owns the open flag.
modal :: Ui :> es => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
modal = overlay ModalOverlay id

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
modalWith :: Ui :> es => (Layout -> Layout) -> Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
modalWith = overlay ModalOverlay

-- | Show a draggable, resizable in-app window with a scrolling body. Like
-- 'modal', the response reports a close request and the caller updates the
-- open flag. Other windows and the page remain interactive outside its bounds.
window :: Ui :> es => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
window = overlay WindowOverlay id

overlay ::
  Ui :> es =>
  OverlayKind -> (Layout -> Layout) -> Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
overlay kind shape open title child = do
  ctx <- askContext
  inp <- askInput
  let
    Size winW winH = inputWindowSize inp
    margin = windowMargin
    availW = max 1 (winW - 2 * margin)
    availH = max 1 (winH - 2 * margin)
    isModal = kind == ModalOverlay
    -- Modals share the window's side padding. The body's scrollbar sits
    -- out in it just inside the panel's edge, that padding from the
    -- content.
    padding = if isModal then windowPad {padB = 12} else windowPad
    barH = if isModal then modalTitleBarH else titleBarChromeHFor
    -- Window body breathing room: one side-pad between the chrome and
    -- the body, matching the window's left/right padding. Modals keep
    -- their own larger gap.
    bodyGap = if isModal then 8 else 10
    minWidth = clamp 1 availW (if isModal then 260 else 280)
    minHeight =
      if isModal
        then 0
        else
          min availH (padT padding + titleBarChromeHFor + bodyGap + padB padding)
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
      addNode
        (ctxNodeArena ctx)
        (if isModal then NodeModal else NodeWindow)
        parent
        (layoutDirection panel)
        panelW
        panelH
        (layoutPadding panel)
        (layoutGap panel)
        (min availW (layoutMinW panel))
        (min availH (layoutMinH panel))
        (min availW (layoutMaxW panel))
        (min availH (layoutMaxH panel))
        0
        (layoutAlignX panel)
        (layoutAlignY panel)
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
    titleLabel = void (labelEx (titleLabelLayoutFor barH) title)
  floatingOverlay open isModal addOverlayNode enter $ do
    close <-
      row' (titleBarLayoutFor barH) $ do
        unless (T.null title) $
          case kind of
            ModalOverlay -> titleLabel
            WindowOverlay -> withKey title titleLabel
        flex
        withKey ("close" :: Text) closeButton
    when (isModal && not (T.null title)) separator
    -- A panel of a fixed height holds its body, which fills what the
    -- title bar leaves; any other panel scrolls a body taller than the
    -- window.
    r <- case panelH of
      Fixed _ -> columnWith (tight . grow . fillW) child
      _ -> scrollWith (tight . grow) child
    when isModal (uiIO (endModal ctx))
    pure (respClicked close, r)
