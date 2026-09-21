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
import Control.Monad (forM_, when)
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
  , resolveScrollStep
  , setScrollOffset
  , setStore
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

-- | One rendered header, shared by selection, close handling, and scrolling.
data Header a = Header
  { headerKey :: !a
  , headerResponse :: !Response
  , headerClosed :: !Bool
  }

tabStrip ::
  (Eq a, Ui :> es) =>
  TabsConfig ->
  a ->
  [Tab a body] ->
  Maybe (a -> Eff es ()) ->
  Eff es (TabResponse a, a)
tabStrip (TabsConfig style orient) cur tabList mRenderBody = do
  ctx <- askContext
  groupId <- nextId
  let vertical = orient == TabLeft || orient == TabRight
      h = tabHeaderH
      styleVal = fromEnum style
      hdrLay = padXY 8 4 . fixedH h . alignCenter . alignMid . gap 4 $ defaultLayout
      barLay
        | vertical = padAll 2 . gap 2 . fillH $ defaultLayout
        | otherwise =
            (if style == TabContained then padTop 2 else id)
              . tight
              . fillW
              . fixedH (h + 4)
              . gap (if style == TabSegmented then 0 else 4)
              $ defaultLayout {layoutDirection = Row}
  let headerBar =
        if vertical
          then column' barLay $ do
            tagContainer groupId
            (tabResp, nextTab, _) <- renderHeaders ctx hdrLay styleVal cur (zip [0 :: Int ..] tabList)
            pure (tabResp, nextTab)
          else row' barLay $ do
            tagContainer groupId
            renderScrollableHeaders ctx style hdrLay barLay groupId cur tabList
  case mRenderBody of
    Nothing -> headerBar
    Just bodyRender ->
      let shell layout = layout $ do
            (tabResp, nextTab) <- headerBar
            bodyRender nextTab
            pure (tabResp, nextTab)
       in if vertical
            then shell (rowWith (tight . fillW . grow))
            else shell (columnWith (tight . fillW))

-- | Horizontal headers that page with chevron buttons when they overflow.
-- While the labels fit, the strip renders exactly as before (no scroll
-- container, no well). Once they overflow, the headers move into a 1D
-- hidden, bare 'scrollHorizontalHidden' container so the framework owns the
-- clip, the offset store, the damage, and the wheel: a bare scroller paints
-- no well, so the headers look exactly as they did before they could
-- scroll, and the hidden policy keeps the scrollbar away while the wheel
-- (both the left+right axis, applied by the framework, and up/down notches,
-- mapped here because a tab bar is horizontal) still pages the same offset.
-- The strip only adds the two buttons. The scroller grows between the
-- buttons, so the right arrow sits on the bar's far edge instead of
-- trailing the last tab.
renderScrollableHeaders ::
  (Eq a, Ui :> es) =>
  Context ->
  TabStyle ->
  Layout ->
  Layout ->
  WidgetId ->
  a ->
  [Tab a body] ->
  Eff es (TabResponse a, a)
renderScrollableHeaders ctx style hdrLay barLay groupId cur tabList = do
  scrollWid <- withKey ("tab-scroller" :: Text) nextId
  let h = tabHeaderH
      styleVal = fromEnum style
      barPad = layoutPadding barLay
      arrowW = 26
      leftGlyph = "\8249"
      rightGlyph = "\8250"
      innerLay = tight . fixedH h . gap (layoutGap barLay) $ defaultLayout {layoutDirection = Row}
      scrollerLay = tight . fillW . fixedH h $ defaultLayout {layoutDirection = Row}
      -- Hidden + bare: the scroller owns the clip, offset, wheel, and damage
      -- but paints nothing (no well, no scrollbar), so the headers look
      -- exactly as they did before the strip could scroll.
      scrollerCfg = scrollHorizontalHidden {scrollBare = True}
      rangeKey = slotKey SlotScrollContent (intKey scrollWid)
      renderInner =
        withKey ("tab-strip" :: Text) $
          row' innerLay (renderHeaders ctx hdrLay styleVal cur (zip [0 :: Int ..] tabList))
  -- The reachable range cached last frame decides whether the strip needs the
  -- scroller at all. Cached as a float so a pure scroll frame keeps its clip
  -- damage (see `onlyScrollFloatsChanged` in NanoUI.Internal.Damage).
  store <- uiIO (getStore ctx)
  let maxOffPrev = max 0 (findSlot fieldFloat 0 rangeKey store)
      overflow = maxOffPrev > 0.5
  off <- uiIO (getScrollOffset ctx scrollWid)
  wheelStep <- uiIO (resolveScrollStep ctx scrollWid)
  mBar <- lastRect groupId
  mScr <- lastRect scrollWid
  inp <- askInput
  let overBar = maybe False (\r -> rectContains r (inputMousePos inp)) mBar
      notches = if overBar then round (v2Y (inputScroll inp)) else 0 :: Int
      canLeft = overflow && off > 0.5
  leftResp <-
    if overflow
      then Just <$> withKey ("tab-arrow-left" :: Text) (arrowButton hdrLay arrowW h (not canLeft) leftGlyph)
      else pure Nothing
  (tabResp, nextTab, resps) <-
    if overflow
      then scrollAreaIdConfigured scrollWid scrollerLay scrollerCfg renderInner
      else renderInner
  let
    (viewX, viewW) =
      if overflow
        then maybe (0, 0) (\r -> (rectX r, rectW r)) mScr
        else
          case mBar of
            Just r ->
              ( rectX r + padL barPad
              , max 0 (rectW r - padL barPad - padR barPad)
              )
            Nothing -> (0, 0)
    maxRight = maximum (0 : [rectX r + rectW r | header <- resps, let r = respRect (headerResponse header)])
    contentW = maxRight - viewX + (if overflow then off else 0)
    -- The first overflow frame has no scroller rect yet (mScr is Nothing);
    -- keep the last cached range instead of measuring against a phantom
    -- viewport, so nothing pages or clamps wildly and the cache never
    -- flip-flops the scroller away.
    maxOff = case (overflow, mScr) of
      (True, Nothing) -> maxOffPrev
      _ -> scrollAxisRange contentW viewW 0
    page = max 1 (viewW * 0.9)
    canRight = overflow && off < maxOff - 0.5
  rightResp <-
    if overflow
      then Just <$> withKey ("tab-arrow-right" :: Text) (arrowButton hdrLay arrowW h (not canRight) rightGlyph)
      else pure Nothing
  uiIO (cacheScrollRange ctx rangeKey maxOff)
  -- One final offset per frame. The paged result folds the arrow pages, the
  -- wheel notches, and the end clamp (a stale offset that outlived a wider
  -- bar); the active-follow wins over it so a programmatically changed tab
  -- always lands in view.
  let pagedOff
        | maybe False respClicked leftResp, canLeft = max 0 (off - page)
        | maybe False respClicked rightResp, canRight = min maxOff (off + page)
        | overflow, notches /= 0, maxOff > 0 =
            clamp 0 maxOff (off + fromIntegral notches * wheelStep)
        | overflow, off > maxOff + 0.5 = maxOff
        | otherwise = off
      finalOff
        | overflow
        , nextTab /= cur
        , Just header <- find ((== nextTab) . headerKey) resps
        , let hr = respRect (headerResponse header) =
            if rectX hr < viewX
              then max 0 (off - (viewX - rectX hr))
              else
                if rectX hr + rectW hr > viewX + viewW
                  then min maxOff (off + (rectX hr + rectW hr - viewX - viewW))
                  else pagedOff
        | otherwise = pagedOff
  when (finalOff /= off) $
    uiIO (setScrollOffset ctx scrollWid finalOff)
  pure (tabResp, nextTab)

-- | Remember the scroller's reachable range for the next frame's arrow
-- visibility. Sub-pixel churn is ignored so a parked strip never dirties.
cacheScrollRange :: Context -> Int -> Float -> IO ()
cacheScrollRange ctx key v = do
  st <- getStore ctx
  when (abs (findSlot fieldFloat 0 key st - v) > 0.5) $
    setStore ctx (insertSlot fieldFloat key v st)

-- | A prettier thin chevron button for the strip. Disabled ends paint the
-- glyph in the muted fg instead of dropping the button, so the row width does
-- not jump as you page to either end.
arrowButton :: (Ui :> es) => Layout -> Float -> Float -> Bool -> Text -> Eff es Response
arrowButton hdrLay arrowW barH muted glyph = do
  theme <- uiTheme
  let lay =
        hdrLay
          { layoutWidth = Fixed arrowW
          , layoutHeight = Fixed barH
          , layoutFontColor = if muted then Just (themeMuted theme) else Nothing
          }
  buttonStyledEx (not muted) glyph 0 lay 0

renderHeaders ::
  (Eq a, Ui :> es) =>
  Context ->
  Layout ->
  Int ->
  a ->
  [(Int, Tab a body)] ->
  Eff es (TabResponse a, a, [Header a])
renderHeaders ctx hdrLay styleVal cur indexed = do
  resps <- mapM (\(i, t) -> withKey i (renderSingleHeader hdrLay (tabEncodeStyle styleVal i) cur t)) indexed
  let clickedKeys = [headerKey h | h <- resps, respClicked (headerResponse h), not (headerClosed h)]
      closedKey = headerKey <$> find headerClosed resps
      nextTab = fromMaybe cur (listToMaybe clickedKeys)
      hasChanged = nextTab /= cur
      hasClicked = not (null clickedKeys)
      overallResp =
        TabResponse
          { tabResponse = setChanged hasChanged (setClicked hasClicked (foldMap headerResponse resps))
          , tabClosed = closedKey
          , tabActive = nextTab
          }
  when (hasChanged || isJust closedKey) requestFrame
  when hasChanged $ uiIO (syncTabHeaderActive ctx nextTab resps)
  pure (overallResp, nextTab, resps)

renderSingleHeader ::
  (Eq a, Ui :> es) =>
  Layout ->
  Int ->
  a ->
  Tab a body ->
  Eff es (Header a)
renderSingleHeader hdrLay tabStyle cur t = do
  let isActive = tabKey t == cur
      headerText = maybe (tabTitle t) (\b -> mconcat [tabTitle t, " (", b, ")"]) (tabBadge t)
      headerButton = buttonStyledEx (not (tabDisabled t))
  if tabClosable t
    then do
      (tabResp, closed) <- rowWith tight $ do
        resp <- headerButton headerText (if isActive then 1 else 0) hdrLay tabStyle
        closeResp <- headerButton "\215" 0 (hdrLay {layoutPadding = Padding 2 4 4 4}) buttonFlagClose
        pure (resp, respClicked closeResp)
      pure (Header (tabKey t) tabResp closed)
    else do
      resp <- headerButton headerText (if isActive then 1 else 0) hdrLay tabStyle
      pure (Header (tabKey t) resp False)

syncTabHeaderActive :: Eq a => Context -> a -> [Header a] -> IO ()
syncTabHeaderActive ctx active resps =
  forM_ resps $ \(Header k r _) -> do
    withWidgetNode ctx (respId r) () $ \i -> setNodeValue (ctxNodeArena ctx) i (if k == active then 1 else 0)

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
tabsConfigured cfg active inputTabs =
  let ts = toList inputTabs
   in snd <$> tabStrip cfg active ts (Just (renderBody ts))

-- | 'tabsConfigured' with selection, close requests, and header interaction details.
tabsConfigured' :: (Foldable f, Eq a, Ui :> es) => TabsConfig -> a -> f (Tab a (Eff es ())) -> Eff es (TabResponse a)
tabsConfigured' cfg active inputTabs =
  let ts = toList inputTabs
   in fst <$> tabStrip cfg active ts (Just (renderBody ts))

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
tabBarConfigured cfg active ts = snd <$> tabStrip cfg active (toList ts) Nothing

-- | 'tabBarConfigured' with interaction details and optional close request.
tabBarConfigured' :: (Foldable f, Eq a, Ui :> es) => TabsConfig -> a -> f (Tab a body) -> Eff es (TabResponse a)
tabBarConfigured' cfg active ts = fst <$> tabStrip cfg active (toList ts) Nothing

renderBody :: (Eq a, Ui :> es) => [Tab a (Eff es ())] -> a -> Eff es ()
renderBody ts activeKey =
  columnWith (tight . fillW) $
    mapM_ tabBody (find ((== activeKey) . tabKey) ts <|> listToMaybe ts)
