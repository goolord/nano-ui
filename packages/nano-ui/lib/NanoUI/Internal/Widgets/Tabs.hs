-- | Controlled tab selection, header styles, close requests, and selected-body rendering.
module NanoUI.Internal.Widgets.Tabs
  ( Tab (..), TabStyle (..), TabOrientation (..), TabResponse (..)
  , TabsConfig (..), defaultTabsConfig
  , tab, closableTab
  , tabs, tabs', tabsConfigured, tabsConfigured'
  , tabBar, tabBar', tabBarConfigured, tabBarConfigured'
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, when, zipWithM)
import Data.Foldable (toList)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( Context (..)
  , getScrollOffset
  , getStore
  , intKey
  , modifyStore
  , resolveScrollStep
  , setScrollOffset
  )
import NanoUI.Internal.Frame.Hit (withWidgetNode)
import NanoUI.Internal.Frame.Scroll.Geometry (scrollAxisRange, scrollBare, scrollHorizontalHidden)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input (inputMousePos, inputScroll)
import NanoUI.Internal.Layout.Arena (setNodeValue)
import NanoUI.Internal.Monad (Ui, askContext, askInput, lastRect, nextId, requestFrame, uiIO, uiTheme, withKey)
import NanoUI.Internal.Store (Slot (..), fieldFloat, findSlot, insertSlot, slotKey)
import NanoUI.Internal.Style
  ( Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , alignCenter
  , alignMid
  , defaultLayout
  , fillH
  , fillW
  , fixedH
  , gap
  , grow
  , padAll
  , padTop
  , padXY
  , themeMuted
  , tight
  )
import NanoUI.Internal.Types (Rect (..), clamp, rectContains, rectW, v2Y)
import NanoUI.Internal.WidgetText (buttonFlagClose, tabEncodeStyle)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx)
import NanoUI.Internal.Widgets.Layout (column', columnWith, row', rowWith, scrollAreaIdConfigured)
import NanoUI.Internal.Widgets.Node
  ( HasResponse (..)
  , Response (..)
  , respClicked
  , respId
  , respRect
  , setChanged
  , setClicked
  , tagContainer
  )

-- | Visual treatment of the tab headers; does not change tab identity.
data TabStyle = TabUnderline | TabPill | TabSegmented | TabContained
  deriving (Eq, Show, Enum, Bounded)

-- | Header strip position relative to the selected tab's body.
data TabOrientation = TabTop | TabBottom | TabLeft | TabRight
  deriving (Eq, Show, Enum, Bounded)

-- | Header look and placement for 'tabsConfigured' and 'tabBarConfigured'.
data TabsConfig = TabsConfig
  { tabsStyle :: !TabStyle
  , tabsOrientation :: !TabOrientation
  }
  deriving (Eq, Show)

-- | Underlined headers along the top.
defaultTabsConfig :: TabsConfig
defaultTabsConfig = TabsConfig TabUnderline TabTop

-- | Tab key, header options, and body. Keys must be distinct within the bar.
-- Closing is a request to the caller; the widget does not remove the tab.
data Tab a body = Tab
  { tabKey :: !a
  , tabTitle :: !Text
  , tabClosable :: !Bool
  , tabDisabled :: !Bool
  , tabBadge :: !(Maybe Text)
  , tabBody :: !body
  }

-- | Header response, optional close request, and selected key. Store 'tabActive'
-- and remove a tab yourself when 'tabClosed' names it.
data TabResponse a = TabResponse
  { tabResponse :: !Response
  , tabClosed :: !(Maybe a)
  , tabActive :: !a
  }
  deriving (Eq, Show)

instance HasResponse (TabResponse a) where
  {-# INLINE toResponse #-}
  toResponse = tabResponse

-- | Enabled tab without a close button or badge.
tab :: a -> Text -> body -> Tab a body
tab key title body = Tab key title False False Nothing body

-- | Enabled tab with a close button, reported through 'tabClosed'.
closableTab :: a -> Text -> body -> Tab a body
closableTab key title body = Tab key title True False Nothing body

-- | Header chrome height: one source for the strip bar, the scroller, and
-- the paging arrows so they cannot drift apart.
tabHeaderH :: Float
tabHeaderH = 28

-- | A header button's layout, the paging arrows' too.
tabHeaderLay :: Layout
tabHeaderLay = padXY 8 4 . fixedH tabHeaderH . alignCenter . alignMid . gap 4 $ defaultLayout

tabStrip ::
  (Eq a, Ui :> es) =>
  TabsConfig ->
  a ->
  [Tab a body] ->
  Maybe (a -> Eff es ()) ->
  Eff es (TabResponse a)
tabStrip (TabsConfig style orient) cur tabList mRenderBody = do
  ctx <- askContext
  groupId <- nextId
  let vertical = orient == TabLeft || orient == TabRight
      headers = renderHeaders ctx (tabEncodeStyle (fromEnum style)) cur tabList
      barGap = if style == TabSegmented then 0 else 4
      contained = if style == TabContained then padTop 2 else id
      headerBar
        | vertical = column' (padAll 2 . gap 2 . fillH $ defaultLayout) $ do
            tagContainer groupId
            fst <$> headers
        | otherwise =
            row' (contained . tight . fillW . fixedH (tabHeaderH + 4) . gap barGap $ defaultLayout) $ do
              tagContainer groupId
              scrollableHeaders ctx groupId barGap cur headers
  case mRenderBody of
    Nothing -> headerBar
    Just bodyRender ->
      (if vertical then rowWith (tight . fillW . grow) else columnWith (tight . fillW)) $ do
        tabResp <- headerBar
        bodyRender (tabActive tabResp)
        pure tabResp

-- | Horizontal headers that page with chevron buttons when they overflow.
-- While the labels fit, the headers sit in the bar with no scroll container.
-- Once they overflow, they move into a hidden, bare 'scrollHorizontalHidden'
-- container, so the framework owns the clip, the offset store, the damage and
-- the left+right wheel, while painting no well and no scrollbar; up/down
-- wheel notches page the same offset here, because a tab bar is horizontal.
-- The scroller grows between the two arrow buttons, so the right arrow sits
-- on the bar's far edge instead of trailing the last tab.
scrollableHeaders ::
  (Eq a, Ui :> es) =>
  Context ->
  WidgetId ->
  Float ->
  a ->
  Eff es (TabResponse a, [(a, Response)]) ->
  Eff es (TabResponse a)
scrollableHeaders ctx groupId barGap cur headers = do
  scrollWid <- withKey ("tab-scroller" :: Text) nextId
  let rangeKey = slotKey SlotScrollContent (intKey scrollWid)
      renderInner =
        withKey ("tab-strip" :: Text) $
          row' (tight . fixedH tabHeaderH . gap barGap $ defaultLayout) headers
  -- The reachable range cached last frame decides whether the strip needs the
  -- scroller at all. Cached as a float so a pure scroll frame keeps its clip
  -- damage (see `onlyScrollFloatsChanged` in NanoUI.Internal.Damage).
  maxOffPrev <- max 0 . findSlot fieldFloat 0 rangeKey <$> uiIO (getStore ctx)
  let overflow = maxOffPrev > 0.5
  off <- uiIO (getScrollOffset ctx scrollWid)
  wheelStep <- uiIO (resolveScrollStep ctx scrollWid)
  mBar <- lastRect groupId
  mScr <- lastRect scrollWid
  inp <- askInput
  let overBar = maybe False (\r -> rectContains r (inputMousePos inp)) mBar
      notches = if overBar then round (v2Y (inputScroll inp)) else 0 :: Int
      canLeft = overflow && off > 0.5
      -- A paging arrow, only while the bar overflows. A disabled end paints
      -- its glyph muted instead of dropping the button, so the row keeps its
      -- width as you page to either end.
      arrow k enabled glyph
        | overflow = withKey (k :: Text) $ do
            theme <- uiTheme
            let muted = if enabled then Nothing else Just (themeMuted theme)
                lay = tabHeaderLay {layoutWidth = Fixed 26, layoutFontColor = muted}
            respClicked <$> buttonStyledEx enabled glyph 0 lay 0
        | otherwise = pure False
  leftClicked <- arrow "tab-arrow-left" canLeft "\8249"
  (tabResp, hdrs) <-
    if overflow
      then
        scrollAreaIdConfigured
          scrollWid
          (tight . fillW . fixedH tabHeaderH $ defaultLayout {layoutDirection = Row})
          scrollHorizontalHidden {scrollBare = True}
          renderInner
      else renderInner
  let
    (viewX, viewW) = maybe (0, 0) (\r -> (rectX r, rectW r)) (if overflow then mScr else mBar)
    maxRight = maximum (0 : [rectX r + rectW r | (_, resp) <- hdrs, let r = respRect resp])
    contentW = maxRight - viewX + (if overflow then off else 0)
    -- The first overflow frame has no scroller rect yet; keep the last cached
    -- range instead of measuring against a phantom viewport, so nothing pages
    -- or clamps wildly and the cache never flip-flops the scroller away.
    maxOff
      | overflow, Nothing <- mScr = maxOffPrev
      | otherwise = scrollAxisRange contentW viewW 0
    page = max 1 (viewW * 0.9)
    canRight = overflow && off < maxOff - 0.5
  rightClicked <- arrow "tab-arrow-right" canRight "\8250"
  -- Sub-pixel churn is ignored so a parked strip never dirties.
  when (abs (maxOff - maxOffPrev) > 0.5) $
    uiIO (modifyStore ctx (insertSlot fieldFloat rangeKey maxOff))
  -- One final offset per frame. The paged result folds the arrow pages, the
  -- wheel notches, and the end clamp (a stale offset that outlived a wider
  -- bar); the active-follow wins over it so a programmatically changed tab
  -- always lands in view.
  let pagedOff
        | leftClicked, canLeft = max 0 (off - page)
        | rightClicked, canRight = min maxOff (off + page)
        | overflow, notches /= 0, maxOff > 0 =
            clamp 0 maxOff (off + fromIntegral notches * wheelStep)
        | overflow, off > maxOff + 0.5 = maxOff
        | otherwise = off
      follow hr
        | rectX hr < viewX = max 0 (off - (viewX - rectX hr))
        | rectX hr + rectW hr > viewX + viewW = min maxOff (off + (rectX hr + rectW hr - viewX - viewW))
        | otherwise = pagedOff
      finalOff
        | overflow, tabActive tabResp /= cur =
            maybe pagedOff (follow . respRect) (lookup (tabActive tabResp) hdrs)
        | otherwise = pagedOff
  when (finalOff /= off) $
    uiIO (setScrollOffset ctx scrollWid finalOff)
  pure tabResp

-- | The headers and the selection after this frame's clicks, with each
-- header's key and response for the scrolling strip.
renderHeaders ::
  (Eq a, Ui :> es) =>
  Context ->
  Int ->
  a ->
  [Tab a body] ->
  Eff es (TabResponse a, [(a, Response)])
renderHeaders ctx tabStyle cur tabList = do
  hdrs <- zipWithM (\i t -> withKey i (renderHeader tabStyle cur t)) [0 :: Int ..] tabList
  let clickedKeys = [k | (k, r, False) <- hdrs, respClicked r]
      closedKey = listToMaybe [k | (k, _, True) <- hdrs]
      keyed = [(k, r) | (k, r, _) <- hdrs]
      nextTab = fromMaybe cur (listToMaybe clickedKeys)
      hasChanged = nextTab /= cur
      resp = setChanged hasChanged (setClicked (not (null clickedKeys)) (foldMap snd keyed))
  when (hasChanged || isJust closedKey) requestFrame
  -- The headers were built with the old selection: move it to the new one.
  when hasChanged $ uiIO $ forM_ keyed $ \(k, r) ->
    withWidgetNode ctx (respId r) () $ \i ->
      setNodeValue (ctxNodeArena ctx) i (if k == nextTab then 1 else 0)
  pure (TabResponse resp closedKey nextTab, keyed)

-- | One header: its key, its response, and whether its close button was clicked.
renderHeader :: (Eq a, Ui :> es) => Int -> a -> Tab a body -> Eff es (a, Response, Bool)
renderHeader tabStyle cur t = do
  let headerText = maybe (tabTitle t) (\b -> mconcat [tabTitle t, " (", b, ")"]) (tabBadge t)
      headerButton = buttonStyledEx (not (tabDisabled t))
      mainButton = headerButton headerText (if tabKey t == cur then 1 else 0) tabHeaderLay tabStyle
  if tabClosable t
    then rowWith tight $ do
      resp <- mainButton
      closeResp <- headerButton "\215" 0 (tabHeaderLay {layoutPadding = Padding 2 4 4 4}) buttonFlagClose
      pure (tabKey t, resp, respClicked closeResp)
    else (tabKey t,,False) <$> mainButton

-- | Tab headers and the active tab's body. Pass the active key; the result is
-- the active key after this frame's clicks, or Enter or Space on a focused
-- header. Only the active tab's body runs.
{-# INLINE tabs #-}
tabs :: (Foldable f, Eq a, Ui :> es) => a -> f (Tab a (Eff es ())) -> Eff es a
tabs = tabsConfigured defaultTabsConfig

-- | 'tabs' returning the 'TabResponse', which also reports a closed tab.
{-# INLINE tabs' #-}
tabs' :: (Foldable f, Eq a, Ui :> es) => a -> f (Tab a (Eff es ())) -> Eff es (TabResponse a)
tabs' = tabsConfigured' defaultTabsConfig

-- | 'tabs' with a header style and placement.
tabsConfigured :: (Foldable f, Eq a, Ui :> es) => TabsConfig -> a -> f (Tab a (Eff es ())) -> Eff es a
tabsConfigured cfg active = fmap tabActive . tabsConfigured' cfg active

-- | 'tabsConfigured' with selection, close requests, and header interaction details.
tabsConfigured' :: (Foldable f, Eq a, Ui :> es) => TabsConfig -> a -> f (Tab a (Eff es ())) -> Eff es (TabResponse a)
tabsConfigured' cfg active inputTabs =
  let ts = toList inputTabs
   in tabStrip cfg active ts (Just (renderBody ts))

-- | Tab headers only; the caller renders the body.
{-# INLINE tabBar #-}
tabBar :: (Foldable f, Eq a, Ui :> es) => a -> f (Tab a body) -> Eff es a
tabBar = tabBarConfigured defaultTabsConfig

{-# INLINE tabBar' #-}
-- | Header-only 'tabBar' with selection and close requests. Does not run tab bodies.
tabBar' :: (Foldable f, Eq a, Ui :> es) => a -> f (Tab a body) -> Eff es (TabResponse a)
tabBar' = tabBarConfigured' defaultTabsConfig

-- | Header-only bar with explicit style/orientation. Returns the selected key
-- without running tab bodies.
tabBarConfigured :: (Foldable f, Eq a, Ui :> es) => TabsConfig -> a -> f (Tab a body) -> Eff es a
tabBarConfigured cfg active = fmap tabActive . tabBarConfigured' cfg active

-- | 'tabBarConfigured' with interaction details and optional close request.
tabBarConfigured' :: (Foldable f, Eq a, Ui :> es) => TabsConfig -> a -> f (Tab a body) -> Eff es (TabResponse a)
tabBarConfigured' cfg active ts = tabStrip cfg active (toList ts) Nothing

renderBody :: (Eq a, Ui :> es) => [Tab a (Eff es ())] -> a -> Eff es ()
renderBody ts activeKey =
  columnWith (tight . fillW) $
    mapM_ tabBody (find ((== activeKey) . tabKey) ts <|> listToMaybe ts)
