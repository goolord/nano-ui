-- | Controlled tab selection, header styles, close requests, and header geometry.
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
import Data.Bits ((.|.))
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import NanoUI.Internal.Context
import NanoUI.Internal.Frame.Chrome (tabHeaderVisualStyle)
import NanoUI.Internal.Frame.Scroll.Geometry (scrollAxisRange, scrollBare, scrollHorizontalHidden)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input (MouseButton (..), inputMousePos, inputScroll)
import NanoUI.Internal.Layout.Arena (setStyleIdx, setWidgetId)
import NanoUI.Internal.Monad (NanoUI, askContext, askInput, disabledWhen, freshWidget, nextId, requestFrame, liftIO, styled, uiTheme, withKey)
import NanoUI.Internal.Store (fieldFloat, findSlot, slotWrite)
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Color, Rect (..), clamp, rectContains, rectIntersect, rectW, v2Y)
import NanoUI.Internal.WidgetText (TabChrome (..), buttonCloseTab, buttonFlagClose, tabChromeEncode, tabEncodeStyle)
import NanoUI.Internal.Widgets.Adornment (Adornments, adornWidget, control, trailing, view)
import NanoUI.Internal.Widgets.Behavior (keyActivated)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx)
import NanoUI.Internal.Widgets.Layout (column', columnWith, labelEx, panel', row', rowWith, scrollAreaIdConfigured)
import NanoUI.Internal.Widgets.Node

-- | Visual treatment of the tab headers; does not change tab identity.
data TabStyle
  = TabUnderline
  -- ^ Headers on a rule, the selected one marked by an accent bar on it.
  | TabPill
  -- ^ The selected header filled with the accent colour.
  | TabSegmented
  -- ^ Headers in a rounded track, the selected one raised out of it, as a
  -- segmented control.
  | TabContained
  -- ^ Folder tabs: the selected header opens onto a bordered body, which
  -- 'tabs' draws round the tab's content.
  deriving (Eq, Show, Enum, Bounded)

-- | Header strip position relative to the selected tab's body.
data TabOrientation = TabTop | TabBottom | TabLeft | TabRight
  deriving (Eq, Show, Enum, Bounded)

-- | Header look and placement for 'tabsConfigured' and 'tabBarConfigured'.
data TabsConfig = TabsConfig
  { tabsStyle :: !TabStyle
  , tabsOrientation :: !TabOrientation
  , tabsTrailing :: NanoUI ()
  -- ^ Drawn after the headers: at the far end of a horizontal strip, below
  -- a vertical strip's headers. For actions on the strip as a whole, such
  -- as a button that opens a new tab. Its widgets take their own presses.
  }

-- | The trailing view shows as @\<view\>@.
instance Show TabsConfig where
  showsPrec d (TabsConfig style orient _) =
    showParen (d > 10) $
      showString "TabsConfig " . showsPrec 11 style . showChar ' ' . showsPrec 11 orient . showString " <view>"

-- | Underlined headers along the top, with nothing after them.
defaultTabsConfig :: TabsConfig
defaultTabsConfig = TabsConfig TabUnderline TabTop (pure ())

-- | Tab key, header options, and body. Keys must be distinct within the bar.
-- Closing is a request to the caller; the widget does not remove the tab.
data Tab a body = Tab
  { tabKey :: !a
  , tabTitle :: !Text
  , tabClosable :: !Bool
  , tabDisabled :: !Bool
  , tabBadge :: !(Maybe Text)
  -- ^ A short count or status drawn in a pill after the title.
  , tabAdornments :: !Adornments
  -- ^ Icons, texts and views drawn in the header beside its title
  -- ("NanoUI.Adornment"), in the header's label colour. A @control@, such
  -- as a pin button, takes its own presses: they neither select nor close
  -- the tab.
  , tabBody :: !body
  }

-- | Controlled selection and close requests, with geometry for optional
-- drag sources and insertion targets.
data TabResponse a = TabResponse
  { tabResponse :: !Response
  , tabClosed :: !(Maybe a)
  , tabActive :: !a
  , tabHeaders :: ![(a, Response)]
    -- ^ Ordered header responses, suitable for 'NanoUI.useDrag'.
  , tabStripRect :: !Rect
    -- ^ Visible header viewport, excluding paging buttons and trailing controls.
  }
  deriving (Eq, Show)

instance HasResponse (TabResponse a) where
  {-# INLINE toResponse #-}
  toResponse = tabResponse

-- | Enabled tab without a close button, badge or adornments.
tab :: a -> Text -> body -> Tab a body
tab key title body = Tab key title False False Nothing mempty body

-- | Enabled tab with a close button. A click on it, or a middle click on the
-- header, is reported through 'tabClosed'.
closableTab :: a -> Text -> body -> Tab a body
closableTab key title body = Tab key title True False Nothing mempty body

-- | Header height, shared by the strip, its scroller and the paging arrows.
tabHeaderH :: Float
tabHeaderH = 30

-- | A header's layout. The headers of a vertical strip share its width, and
-- start their labels at their padding ('tabLabelAtStart').
tabHeaderLay :: Bool -> Layout
tabHeaderLay vertical = (if vertical then fillW else id) . fixedH tabHeaderH . alignMid . gap 6 $ defaultLayout

-- | Name the current container @wid@ and give it @part@ of a strip's chrome,
-- painted under its children ('NanoUI.Internal.Frame.Chrome.paintTabChrome').
-- The id tracks the container's rect, so what it paints repaints as it moves.
tagTabChrome :: WidgetId -> TabChrome -> TabStyle -> TabOrientation -> NanoUI ()
tagTabChrome wid part style orient = do
  ctx <- askContext
  liftIO $ do
    parent <- currentParent ctx
    when (parent >= 0) $ do
      setWidgetId (ctxNodeArena ctx) parent wid
      setStyleIdx (ctxNodeArena ctx) parent (tabChromeEncode part (fromEnum style) (fromEnum orient))

tabStrip ::
  Eq a =>
  TabsConfig ->
  a ->
  [Tab a body] ->
  Maybe (a -> NanoUI ()) ->
  NanoUI (TabResponse a)
tabStrip (TabsConfig style orient trailingView) cur tabList mRenderBody = do
  (groupId, ctx) <- freshWidget
  let vertical = orient == TabLeft || orient == TabRight
      -- Headers ahead of the body take this frame's clicks. A body ahead of
      -- them runs the key passed in, so they show it too until the caller
      -- passes the new one.
      bodyFirst = isJust mRenderBody && (orient == TabBottom || orient == TabRight)
      headers = renderHeaders ctx style orient (not bodyFirst) cur tabList
      barGap = if style == TabPill then 4 else 2
      edgeChrome = case style of
        TabUnderline -> TabChromeRule
        TabContained -> TabChromeRule
        _ -> TabChromeNone
      -- Contained headers stand off the rule's ends and the strip's far side.
      contained
        | style /= TabContained = id
        | otherwise = case orient of
            TabTop -> padLRTB 6 6 4 0
            TabBottom -> padLRTB 6 6 0 4
            TabLeft -> padLRTB 4 0 6 6
            TabRight -> padLRTB 0 4 6 6
      -- Headers sit on the rule, whatever the trailing view's height.
      onEdge = if orient == TabBottom then alignTop else alignBottom
      headerBar
        | vertical = column' (contained . segmentedColumn . tight . gap barGap $ defaultLayout) $ do
            if style == TabSegmented
              then tagTabChrome groupId TabChromeTrack style orient
              else tagTabChrome groupId edgeChrome style orient
            tabResp <- headers
            trailingView
            pure tabResp
        | otherwise = row' (contained . onEdge . tight . fillW . gap 8 $ defaultLayout) $ do
            tagTabChrome groupId edgeChrome style orient
            tabResp <- scrollableHeaders ctx style barGap cur headers
            trailingView
            pure tabResp
      -- A segmented column hugs its headers in its track; the others run the
      -- body's length.
      segmentedColumn = if style == TabSegmented then padAll 3 else fillH
      bodyGap = if style == TabContained then 0 else if vertical then 16 else 12
  result <- case mRenderBody of
    Nothing -> headerBar
    Just bodyRender ->
      (if vertical then rowWith (tight . fillW . grow . gap bodyGap) else columnWith (tight . fillW . gap bodyGap)) $
        if bodyFirst
          then bodyRender cur *> headerBar
          else do
            tabResp <- headerBar
            bodyRender (tabActive tabResp)
            pure tabResp
  clip <- liftIO (getPrevClipRect ctx groupId)
  let bounds = tabStripRect result
      visible = maybe bounds (fromMaybe (Rect 0 0 0 0) . rectIntersect bounds) clip
  pure result {tabStripRect = visible}

-- | Horizontal headers that page with chevron buttons when they overflow,
-- in a row that takes the strip's width less its trailing view. Overflowing
-- headers move into a bare horizontal scroller that grows between the two
-- arrows; the vertical wheel pages it too. Segmented headers sit in their
-- track, which scrolls with them.
scrollableHeaders ::
  Eq a =>
  Context ->
  TabStyle ->
  Float ->
  a ->
  NanoUI (TabResponse a) ->
  NanoUI (TabResponse a)
scrollableHeaders ctx style barGap cur headers = withKey ("tab-headers" :: Text) $ do
  groupId <- nextId
  scrollWid <- nextId
  trackId <- nextId
  let rangeKey = slotKey SlotScrollContent (intKey scrollWid)
      segmented = style == TabSegmented
      stripH = if segmented then tabHeaderH + 6 else tabHeaderH
      renderInner =
        withKey ("tab-strip" :: Text) $
          row' ((if segmented then padAll 3 else id) . tight . fixedH stripH . gap barGap $ defaultLayout) $ do
            when segmented $ tagTabChrome trackId TabChromeTrack style TabTop
            headers
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
                lay = (fixedWH 26 stripH . alignCenter . alignMid $ defaultLayout) {layoutFontColor = muted}
            respClicked <$> styled subtle (buttonStyledEx enabled glyph 0 lay 0)
        | otherwise = pure False
  (leftClicked, tabResp, rightClicked) <-
    row' (tight . grow . alignMid . fixedH stripH $ defaultLayout) $ do
      tagContainer groupId
      (,,)
        <$> arrow "tab-arrow-left" canLeft "\8249"
        <*> ( if overflow
                then
                  scrollAreaIdConfigured
                    scrollWid
                    (tight . fillW . fixedH stripH $ defaultLayout {layoutDirection = Row})
                    scrollHorizontalHidden {scrollBare = True}
                    renderInner
                else renderInner
            )
        <*> arrow "tab-arrow-right" canRight "\8250"
  let
    hdrs = tabHeaders tabResp
    viewport = fromMaybe (Rect 0 0 0 0) (if overflow then mScr else mBar)
    viewX = rectX viewport
    viewW = rectW viewport
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
  pure tabResp {tabStripRect = viewport}

-- | The headers and the selection after this frame's clicks, with each
-- header's key and response for the scrolling strip. With @follow@ the
-- headers show the clicked tab as selected at once; without it they show
-- @cur@, the key passed in.
renderHeaders ::
  Eq a =>
  Context ->
  TabStyle ->
  TabOrientation ->
  Bool ->
  a ->
  [Tab a body] ->
  NanoUI (TabResponse a)
renderHeaders ctx style orient follow cur tabList = do
  hdrs <- zipWithM (\i t -> withKey i (renderHeader style orient cur t)) [0 :: Int ..] tabList
  let clickedKeys = [k | (k, r, False) <- hdrs, respClicked r]
      closedKey = listToMaybe [k | (k, _, True) <- hdrs]
      keyed = [(k, r) | (k, r, _) <- hdrs]
      nextTab = fromMaybe cur (listToMaybe clickedKeys)
      hasChanged = nextTab /= cur
      resp = setChanged hasChanged (setClicked (not (null clickedKeys)) (foldMap snd keyed))
  when (hasChanged || isJust closedKey) requestFrame
  when follow $ liftIO (moveSelection ctx cur nextTab keyed)
  pure (TabResponse resp closedKey nextTab keyed (respRect resp))

-- | One header: its key, its response, and whether it was closed (close
-- button clicked, or header or close button middle-clicked, as in a
-- browser). The header is one button; its adornments, badge and close
-- button are drawn inside it, in its label colour. A press on the close
-- button or on a control among the adornments is theirs, not the header's.
renderHeader :: Eq a => TabStyle -> TabOrientation -> a -> Tab a body -> NanoUI (a, Response, Bool)
renderHeader style orient cur t = disabledWhen (tabDisabled t) $ do
  theme <- uiTheme
  let isActive = tabKey t == cur
      packed = tabEncodeStyle (fromEnum style) (fromEnum orient)
      lay = tabHeaderLay (orient == TabLeft || orient == TabRight)
      labelCol = styleFg (tabHeaderVisualStyle theme packed isActive)
  resp <- buttonStyledEx (not (tabDisabled t)) (tabTitle t) (if isActive then 1 else 0) lay packed
  -- The close button's response, which its control view leaves here.
  closeRef <- if tabClosable t then Just <$> liftIO (newIORef Nothing) else pure Nothing
  let badge = foldMap (trailing . view . tabBadgeView labelCol) (tabBadge t)
      closeButton ref =
        buttonStyledEx True "\215" 0 (tight . fixedWH 18 18 . fontColor labelCol $ defaultLayout) (buttonFlagClose .|. buttonCloseTab)
          >>= liftIO . writeIORef ref . Just
      close = foldMap (trailing . control . closeButton) closeRef
  taken <- adornWidget (respId resp) lay (const labelCol) (tabAdornments t <> badge <> close)
  closeResp <- maybe (pure Nothing) (liftIO . readIORef) closeRef
  -- A control holding the pointer has the press; only the keyboard can
  -- select the tab ('NanoUI.Internal.Widgets.Button.buttonConfigured'').
  header <- if taken then (`setClicked` inertResponse resp) <$> keyActivated (respId resp) else pure resp
  let closed =
        tabClosable t
          && ( maybe False (\c -> respClicked c || respClickedWith MouseMiddle c) closeResp
                 || respClickedWith MouseMiddle header
             )
  pure (tabKey t, header, closed)

-- | A badge: its text, smaller, in a pill of the label colour @col@.
tabBadgeView :: Color -> Text -> NanoUI ()
tabBadgeView col txt =
  styled (panelStyle (\s -> s {styleBg = fadeAlpha col 44, styleBorderWidth = 0, styleCornerRadius = 8})) $
    panel' (padXY 6 1 . tight . minH 17 . alignCenter . alignMid $ defaultLayout) $
      () <$ labelEx (tight . fontSizeScale 0.78 . fontColor col $ defaultLayout) txt

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
    -- A contained body is a bordered surface the selected header opens onto.
    body k = do
      let contained = tabsStyle cfg == TabContained
          vertical = tabsOrientation cfg `elem` [TabLeft, TabRight]
          surface = if contained then padAll 12 . (if vertical then fillH else id) else id
      bodyId <- if contained then withKey ("tab-body" :: Text) (Just <$> nextId) else pure Nothing
      columnWith (surface . tight . fillW) $ do
        mapM_ (\wid -> tagTabChrome wid TabChromeBody (tabsStyle cfg) (tabsOrientation cfg)) bodyId
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
