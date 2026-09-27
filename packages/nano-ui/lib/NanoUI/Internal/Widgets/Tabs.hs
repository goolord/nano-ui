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
import Control.Monad (when, zipWithM)
import Data.Foldable (toList)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import NanoUI.Internal.Context
import NanoUI.Internal.Frame.Scroll.Geometry (scrollAxisRange, scrollBare, scrollHorizontalHidden)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input (MouseButton (..), inputMousePos, inputScroll)
import NanoUI.Internal.Monad (NanoUI, askInput, freshWidget, nextId, requestFrame, liftIO, uiTheme, withKey)
import NanoUI.Internal.Store (fieldFloat, findSlot, slotWrite)
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Rect (..), clamp, rectContains, rectW, v2Y)
import NanoUI.Internal.WidgetText (buttonFlagClose, tabEncodeStyle)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx)
import NanoUI.Internal.Widgets.Layout (column', columnWith, row', rowWith, scrollAreaIdConfigured)
import NanoUI.Internal.Widgets.Node

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

-- | Enabled tab with a close button. A click on it, or a middle click on the
-- header, is reported through 'tabClosed'.
closableTab :: a -> Text -> body -> Tab a body
closableTab key title body = Tab key title True False Nothing body

-- | Header height, shared by the strip, its scroller and the paging arrows.
tabHeaderH :: Float
tabHeaderH = 28

-- | A header button's layout, the paging arrows' too.
tabHeaderLay :: Layout
tabHeaderLay = padXY 8 4 . fixedH tabHeaderH . alignCenter . alignMid . gap 4 $ defaultLayout

tabStrip ::
  Eq a =>
  TabsConfig ->
  a ->
  [Tab a body] ->
  Maybe (a -> NanoUI ()) ->
  NanoUI (TabResponse a)
tabStrip (TabsConfig style orient) cur tabList mRenderBody = do
  (groupId, ctx) <- freshWidget
  let vertical = orient == TabLeft || orient == TabRight
      headers = renderHeaders ctx (tabEncodeStyle (fromEnum style)) cur tabList
      barGap = if style == TabSegmented then 0 else 4
      contained = if style == TabContained then padTop 2 else id
      barLay = contained . tight . fillW . fixedH (tabHeaderH + 4) . gap barGap $ defaultLayout
      headerBar
        | vertical = column' (padAll 2 . gap 2 . fillH $ defaultLayout) $ do
            tagContainer groupId
            fst <$> headers
        | otherwise = row' barLay $ do
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
-- Overflowing headers move into a bare horizontal scroller that grows between
-- the two arrows; the vertical wheel pages it too.
scrollableHeaders ::
  Eq a =>
  Context ->
  WidgetId ->
  Float ->
  a ->
  NanoUI (TabResponse a, [(a, Response)]) ->
  NanoUI (TabResponse a)
scrollableHeaders ctx groupId barGap cur headers = do
  scrollWid <- withKey ("tab-scroller" :: Text) nextId
  let rangeKey = slotKey SlotScrollContent (intKey scrollWid)
      renderInner =
        withKey ("tab-strip" :: Text) $
          row' (tight . fixedH tabHeaderH . gap barGap $ defaultLayout) headers
      -- Whether the strip overflows and can page left and right, at reachable
      -- range @r@ and offset @o@.
      paging r o = (r > 0.5, r > 0.5 && o > 0.5, o < r - 0.5)
  -- The reachable range measured after last frame's layout decides whether
  -- the strip needs the scroller. It is a float slot so a pure scroll frame
  -- keeps its clip damage (`onlyScrollFloatsChanged` in NanoUI.Internal.Damage).
  maxOff <- max 0 . findSlot fieldFloat 0 rangeKey <$> liftIO (getStore ctx)
  off <- liftIO (getScrollOffset ctx scrollWid)
  let (overflow, canLeft, canRight) = paging maxOff off
  wheelStep <- liftIO (resolveScrollStep ctx scrollWid)
  -- Not 'lastRect': the strip checks its own layout below, and the bar
  -- moving alone needs no second frame.
  mBar <- liftIO (getPrevRect ctx groupId)
  mScr <- liftIO (getPrevRect ctx scrollWid)
  inp <- askInput
  let overBar = maybe False (\r -> rectContains r (inputMousePos inp)) mBar
      notches = if overBar then round (v2Y (inputScroll inp)) else 0 :: Int
      -- A paging arrow while the bar overflows, muted at its end.
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
  rightClicked <- arrow "tab-arrow-right" canRight "\8250"
  let
    (viewX, viewW) = maybe (0, 0) (\r -> (rectX r, rectW r)) (if overflow then mScr else mBar)
    page = max 1 (viewW * 0.9)
    -- The range this frame's layout leaves: the headers' right edge past the
    -- start of what shows them, less its width. The first overflow frame has
    -- no scroller rect yet, and keeps the range it has. Headers that would
    -- fit the whole bar without the arrows leave no range, so the arrows go
    -- once the bar is wide enough, however narrow the scroller between them.
    measure = do
      mView <- getPrevRect ctx (if overflow then scrollWid else groupId)
      mBarNow <- if overflow then getPrevRect ctx groupId else pure Nothing
      rights <- mapM (fmap (maybe 0 (\r -> rectX r + rectW r)) . getPrevRect ctx . respId . snd) hdrs
      o <- getScrollOffset ctx scrollWid
      let extent vx = maximum (0 : rights) - vx + (if overflow then o else 0)
          range (Rect vx _ vw _)
            | Just bar <- mBarNow, scrollAxisRange (extent vx) (rectW bar) 0 == 0 = 0
            | otherwise = scrollAxisRange (extent vx) vw 0
      pure (maybe maxOff range mView, o)
  -- After layout the strip measures itself for the next frame, which it only
  -- asks for when the overflow or an arrow flips: a width change that keeps
  -- them settles in the frame it happens in. Sub-pixel churn is ignored so a
  -- parked strip never writes.
  liftIO . recordLayoutRead ctx rangeKey $ do
    (maxOff', o) <- measure
    when (abs (maxOff' - maxOff) > 0.5) $
      publishLayoutSlots ctx (slotWrite fieldFloat rangeKey maxOff')
    pure (paging maxOff' o /= paging maxOff off)
  -- One offset per frame: arrow pages, wheel notches and the end clamp, or,
  -- when the active tab changed, whatever brings it into view.
  let pagedOff
        | leftClicked, canLeft = max 0 (off - page)
        | rightClicked, canRight = min maxOff (off + page)
        | overflow, notches /= 0, maxOff > 0 =
            clamp 0 maxOff (off + fromIntegral notches * wheelStep)
        | overflow, off > maxOff + 0.5 = maxOff
        | otherwise = off
      follow hr
        | rectX hr < viewX = max 0 (off - (viewX - rectX hr))
        | rectX hr + rectW hr > viewX + viewW =
            min maxOff (off + (rectX hr + rectW hr - viewX - viewW))
        | otherwise = pagedOff
      finalOff
        | overflow, tabActive tabResp /= cur =
            maybe pagedOff (follow . respRect) (lookup (tabActive tabResp) hdrs)
        | otherwise = pagedOff
  when (finalOff /= off) $
    liftIO (setScrollOffset ctx scrollWid finalOff)
  pure tabResp

-- | The headers and the selection after this frame's clicks, with each
-- header's key and response for the scrolling strip.
renderHeaders ::
  Eq a =>
  Context ->
  Int ->
  a ->
  [Tab a body] ->
  NanoUI (TabResponse a, [(a, Response)])
renderHeaders ctx tabStyle cur tabList = do
  hdrs <- zipWithM (\i t -> withKey i (renderHeader tabStyle cur t)) [0 :: Int ..] tabList
  let clickedKeys = [k | (k, r, False) <- hdrs, respClicked r]
      closedKey = listToMaybe [k | (k, _, True) <- hdrs]
      keyed = [(k, r) | (k, r, _) <- hdrs]
      nextTab = fromMaybe cur (listToMaybe clickedKeys)
      hasChanged = nextTab /= cur
      resp = setChanged hasChanged (setClicked (not (null clickedKeys)) (foldMap snd keyed))
  when (hasChanged || isJust closedKey) requestFrame
  liftIO (moveSelection ctx cur nextTab keyed)
  pure (TabResponse resp closedKey nextTab, keyed)

-- | One header: its key, its response, and whether it was closed (close
-- button clicked, or header or close button middle-clicked, as in a
-- browser).
renderHeader :: Eq a => Int -> a -> Tab a body -> NanoUI (a, Response, Bool)
renderHeader tabStyle cur t = do
  let headerText = maybe (tabTitle t) (\b -> mconcat [tabTitle t, " (", b, ")"]) (tabBadge t)
      headerButton = buttonStyledEx (not (tabDisabled t))
      mainButton = headerButton headerText (if tabKey t == cur then 1 else 0) tabHeaderLay tabStyle
  if tabClosable t
    then rowWith tight $ do
      resp <- mainButton
      closeResp <-
        headerButton "\215" 0 (tabHeaderLay {layoutPadding = Padding 2 4 4 4}) buttonFlagClose
      pure (tabKey t, resp, respClicked closeResp || respClickedWith MouseMiddle resp || respClickedWith MouseMiddle closeResp)
    else (tabKey t,,False) <$> mainButton

-- | Tab headers and the active tab's body. Pass the active key; the result is
-- the active key after this frame's clicks, or Enter or Space on a focused
-- header. Only the active tab's body runs.
{-# INLINE tabs #-}
tabs :: (Foldable f, Eq a) => a -> f (Tab a (NanoUI ())) -> NanoUI a
tabs = tabsConfigured defaultTabsConfig

-- | 'tabs' returning the 'TabResponse', which also reports a closed tab.
{-# INLINE tabs' #-}
tabs' :: (Foldable f, Eq a) => a -> f (Tab a (NanoUI ())) -> NanoUI (TabResponse a)
tabs' = tabsConfigured' defaultTabsConfig

-- | 'tabs' with a header style and placement.
tabsConfigured :: (Foldable f, Eq a) => TabsConfig -> a -> f (Tab a (NanoUI ())) -> NanoUI a
tabsConfigured cfg active = fmap tabActive . tabsConfigured' cfg active

-- | 'tabsConfigured' with selection, close requests, and header interaction details.
tabsConfigured' :: (Foldable f, Eq a) => TabsConfig -> a -> f (Tab a (NanoUI ())) -> NanoUI (TabResponse a)
tabsConfigured' cfg active inputTabs = tabStrip cfg active ts (Just body)
  where
    ts = toList inputTabs
    -- Only the active tab's body runs, or the first tab's when none matches.
    -- Each tab's body is keyed by its place in the list, so bodies at the same
    -- position in different tabs do not share widget ids and state.
    body k =
      columnWith (tight . fillW) $
        mapM_
          (\(i, t) -> withKey (i :: Int) (tabBody t))
          (find ((== k) . tabKey . snd) its <|> listToMaybe its)
    its = zip [0 ..] ts

-- | Tab headers only; the caller renders the body.
{-# INLINE tabBar #-}
tabBar :: (Foldable f, Eq a) => a -> f (Tab a body) -> NanoUI a
tabBar = tabBarConfigured defaultTabsConfig

{-# INLINE tabBar' #-}
-- | Header-only 'tabBar' with selection and close requests. Does not run tab bodies.
tabBar' :: (Foldable f, Eq a) => a -> f (Tab a body) -> NanoUI (TabResponse a)
tabBar' = tabBarConfigured' defaultTabsConfig

-- | Header-only bar with explicit style/orientation. Returns the selected key
-- without running tab bodies.
tabBarConfigured :: (Foldable f, Eq a) => TabsConfig -> a -> f (Tab a body) -> NanoUI a
tabBarConfigured cfg active = fmap tabActive . tabBarConfigured' cfg active

-- | 'tabBarConfigured' with interaction details and optional close request.
tabBarConfigured' :: (Foldable f, Eq a) => TabsConfig -> a -> f (Tab a body) -> NanoUI (TabResponse a)
tabBarConfigured' cfg active ts = tabStrip cfg active (toList ts) Nothing
