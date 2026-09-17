{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Tabs
  ( Tab (..), TabStyle (..), TabOrientation (..), TabResponse (..)
  , TabsConfig (..), defaultTabsConfig
  , tab, closableTab
  , tabs, tabs', tabsConfigured, tabsConfigured'
  , tabBar, tabBar', tabBarConfigured, tabBarConfigured'
  )
where

import Control.Monad (forM_, when)
import Data.Bits ((.|.))
import Data.List (find)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , getPrevRect
  , getScrollOffset
  , getStore
  , intKey
  , markDirty
  , resolveScrollStep
  , setScrollOffset
  , setStore
  , currentTheme
  )
import NanoUI.Font (resolveLayoutPadding)
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Frame.Scroll.Geometry (scrollAxisRange, scrollBare, scrollHorizontalHidden)
import NanoUI.Id (WidgetId)
import NanoUI.Input (inputMousePos, inputScroll)
import NanoUI.Layout.Arena (setNodeValue)
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (storeFloat), slotKey, slotScrollContent)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , defaultLayout
  , fillW
  , grow
  , themeMuted
  , tight
  )
import NanoUI.Types (Rect (..), clamp, rectContains, rectW, v2Y)
import NanoUI.WidgetText (buttonFlagClose, buttonFlagTab)
import NanoUI.Widgets.Combinators (buttonStyledEx)
import NanoUI.Widgets.Layout (column', columnWith, row', rowWith, scrollAreaIdConfigured)
import NanoUI.Widgets.Node
  ( HasResponse (..)
  , Response (..)
  , respClicked
  , respId
  , respRect
  , setChanged
  , setClicked
  , tagContainer
  )

data TabStyle = TabUnderline | TabPill | TabSegmented | TabContained
  deriving (Eq, Show, Enum, Bounded)

data TabOrientation = TabTop | TabBottom | TabLeft | TabRight
  deriving (Eq, Show, Enum, Bounded)

-- | Header look and placement for 'tabsWith' and 'tabBarWith'.
data TabsConfig = TabsConfig
  { tabsStyle :: !TabStyle
  , tabsOrientation :: !TabOrientation
  }
  deriving (Eq, Show)

-- | Underlined headers along the top.
defaultTabsConfig :: TabsConfig
defaultTabsConfig = TabsConfig TabUnderline TabTop

data Tab a body = Tab
  { tabKey :: !a
  , tabTitle :: !Text
  , tabClosable :: !Bool
  , tabDisabled :: !Bool
  , tabBadge :: !(Maybe Text)
  , tabBody :: !body
  }

data TabResponse a = TabResponse
  { tabResponse :: !Response
  , tabClosed :: !(Maybe a)
  , tabActive :: !a
  }
  deriving (Eq, Show)

instance HasResponse (TabResponse a) where
  {-# INLINE toResponse #-}
  toResponse = tabResponse

tab :: a -> Text -> body -> Tab a body
tab key title body = Tab key title False False Nothing body

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
      hdrLay =
        defaultLayout
          { layoutHeight = Fixed h
          , layoutPadding = Padding 8 8 4 4
          , layoutAlignX = AlignCenter
          , layoutAlignY = AlignMiddle
          , layoutGap = 4
          }
      barLay =
        if vertical
          then defaultLayout {layoutDirection = Column, layoutWidth = Fit, layoutHeight = Grow 1, layoutGap = 2, layoutPadding = Padding 2 2 2 2}
          else
            defaultLayout
              { layoutDirection = Row
              , layoutWidth = Grow 1
              , layoutHeight = Fixed (h + 4)
              , layoutGap = if style == TabSegmented then 0 else 4
              , layoutPadding = if style == TabContained then Padding 0 0 2 0 else Padding 0 0 0 0
              }
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
  let fm = ctxFontMetrics ctx
      h = tabHeaderH
      styleVal = fromEnum style
      barPad = resolveLayoutPadding fm (layoutPadding barLay)
      arrowW = 26
      leftGlyph = "\8249"
      rightGlyph = "\8250"
      innerLay =
        defaultLayout
          { layoutDirection = Row
          , layoutWidth = Fit
          , layoutHeight = Fixed h
          , layoutGap = layoutGap barLay
          , layoutPadding = Padding 0 0 0 0
          }
      scrollerLay =
        defaultLayout
          { layoutDirection = Row
          , layoutWidth = Grow 1
          , layoutHeight = Fixed h
          , layoutPadding = Padding 0 0 0 0
          }
      -- Hidden + bare: the scroller owns the clip, offset, wheel, and damage
      -- but paints nothing (no well, no scrollbar), so the headers look
      -- exactly as they did before the strip could scroll.
      scrollerCfg = scrollHorizontalHidden {scrollBare = True}
      rangeKey = slotKey slotScrollContent (intKey scrollWid)
      renderInner =
        withKey ("tab-strip" :: Text) $
          row' innerLay (renderHeaders ctx hdrLay styleVal cur (zip [0 :: Int ..] tabList))
  -- The reachable range cached last frame decides whether the strip needs the
  -- scroller at all. Cached as a float so a pure scroll frame keeps its clip
  -- damage (see `onlyScrollFloatsChanged` in NanoUI.Damage).
  store <- uiIO (getStore ctx)
  let maxOffPrev = max 0 (IM.findWithDefault 0 rangeKey (storeFloat store))
      overflow = maxOffPrev > 0.5
  off <- uiIO (getScrollOffset ctx scrollWid)
  wheelStep <- uiIO (resolveScrollStep ctx scrollWid)
  mBar <- uiIO (getPrevRect ctx groupId)
  mScr <- uiIO (getPrevRect ctx scrollWid)
  inp <- askInput
  let overBar = maybe False (\r -> rectContains r (inputMousePos inp)) mBar
      notches = if overBar then round (v2Y (inputScroll inp)) else 0 :: Int
      canLeft = overflow && off > 0.5
  leftResp <-
    if overflow
      then Just <$> withKey ("tab-arrow-left" :: Text) (arrowButton ctx hdrLay arrowW h (not canLeft) leftGlyph)
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
      then Just <$> withKey ("tab-arrow-right" :: Text) (arrowButton ctx hdrLay arrowW h (not canRight) rightGlyph)
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
  let prev = IM.findWithDefault 0 key (storeFloat st)
  when (abs (prev - v) > 0.5) $
    setStore ctx (st {storeFloat = IM.insert key v (storeFloat st)})

-- | A prettier thin chevron button for the strip. Disabled ends paint the
-- glyph in the muted fg instead of dropping the button, so the row width does
-- not jump as you page to either end.
arrowButton :: (Ui :> es) => Context -> Layout -> Float -> Float -> Bool -> Text -> Eff es Response
arrowButton ctx hdrLay arrowW barH muted glyph = do
  theme <- uiIO (currentTheme ctx)
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
  resps <- mapM (\(i, t) -> withKey i (renderSingleHeader hdrLay (styleVal + 4 * i) cur t)) indexed
  let clickedKeys = [headerKey h | h <- resps, respClicked (headerResponse h), not (headerClosed h)]
      closedKey = headerKey <$> find headerClosed resps
      nextTab = case clickedKeys of
        (k : _) -> k
        [] -> cur
      hasChanged = nextTab /= cur
      hasClicked = not (null clickedKeys)
      overallResp =
        TabResponse
          { tabResponse = setChanged hasChanged (setClicked hasClicked (foldMap headerResponse resps))
          , tabClosed = closedKey
          , tabActive = nextTab
          }
  when (hasChanged || isJust closedKey) $ uiIO (markDirty ctx)
  when hasChanged $ uiIO (syncTabHeaderActive ctx nextTab resps)
  pure (overallResp, nextTab, resps)

renderSingleHeader ::
  (Eq a, Ui :> es) =>
  Layout ->
  Int ->
  a ->
  Tab a body ->
  Eff es (Header a)
renderSingleHeader hdrLay packedStyle cur t = do
  let isActive = tabKey t == cur
      headerText = maybe (tabTitle t) (\b -> mconcat [tabTitle t, " (", b, ")"]) (tabBadge t)
      tabStyle = packedStyle .|. buttonFlagTab
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
    mIdx <- findNodeByWidgetId ctx (respId r)
    case mIdx of
      Just i -> setNodeValue (ctxNodeArena ctx) i (if k == active then 1 else 0)
      Nothing -> pure ()

-- | Tab headers and the active tab's body. Pass the active key; the result is
-- the active key after this frame's clicks or arrow keys. Only the active
-- tab's body runs.
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
  let ts = foldr (:) [] inputTabs
   in snd <$> tabStrip cfg active ts (Just (renderBody ts))

tabsConfigured' :: (Foldable f, Eq a, Ui :> es) => TabsConfig -> a -> f (Tab a (Eff es ())) -> Eff es (TabResponse a)
tabsConfigured' cfg active inputTabs =
  let ts = foldr (:) [] inputTabs
   in fst <$> tabStrip cfg active ts (Just (renderBody ts))

-- | Tab headers only; the caller renders the body.
{-# INLINE tabBar #-}
tabBar :: (Foldable f, Eq a, Ui :> es) => a -> f (Tab a body) -> Eff es a
tabBar = tabBarConfigured defaultTabsConfig

{-# INLINE tabBar' #-}
tabBar' :: (Foldable f, Eq a, Ui :> es) => a -> f (Tab a body) -> Eff es (TabResponse a)
tabBar' = tabBarConfigured' defaultTabsConfig

tabBarConfigured :: (Foldable f, Eq a, Ui :> es) => TabsConfig -> a -> f (Tab a body) -> Eff es a
tabBarConfigured cfg active ts = snd <$> tabStrip cfg active (foldr (:) [] ts) Nothing

tabBarConfigured' :: (Foldable f, Eq a, Ui :> es) => TabsConfig -> a -> f (Tab a body) -> Eff es (TabResponse a)
tabBarConfigured' cfg active ts = fst <$> tabStrip cfg active (foldr (:) [] ts) Nothing

renderBody :: (Eq a, Ui :> es) => [Tab a (Eff es ())] -> a -> Eff es ()
renderBody ts activeKey =
  columnWith (tight . fillW) $
    case find ((== activeKey) . tabKey) ts of
      Just selected -> tabBody selected
      Nothing -> maybe (pure ()) tabBody (listToMaybe ts)
