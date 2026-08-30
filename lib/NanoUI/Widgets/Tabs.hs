{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Tabs
  ( Tab (..)
  , TabStyle (..)
  , TabOrientation (..)
  , TabResponse (..)
  , tabRespClicked
  , tabRespChanged
  , tab
  , closableTab
  , mkTab
  , tabs
  , tabsEx
  , tabBar
  , tabBarEx
  , tabsEmit
  , tabsEmitEx
  , useTab
  , useTabIdx
  , boundedTabs
  )
where

import Control.Monad (forM_, when, zipWithM)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , markDirty
  )
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.Id (WidgetId (..))
import NanoUI.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , getWidgetId
  , setNodeValue
  )
import NanoUI.Monad (Ui, askContext, emit, nextId, uiIO, withKey)
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
  , tight
  )
import NanoUI.Types (Rect (..))
import NanoUI.WidgetMarkers (closeButtonMarker, tabButtonMarker)
import NanoUI.Widgets.Animate (useText)
import NanoUI.Widgets.Layout (column, row)
import NanoUI.Widgets.Node
  ( Clickable (..)
  , Responding (..)
  , Response (..)
  , addWidgetStyled
  , mkResponse
  , setChanged
  , setClicked
  )

-- | Visual style for tab headers.
data TabStyle
  = TabStyleUnderline
  | TabStylePill
  | TabStyleSegmented
  | TabStyleContained
  | TabUnderline
  | TabPill
  | TabSegmented
  | TabContained
  deriving (Eq, Show, Enum, Bounded)

-- | Orientation for the tab list.
data TabOrientation
  = TabTop
  | TabBottom
  | TabLeft
  | TabRight
  | TabHorizontal
  | TabVertical
  deriving (Eq, Show, Enum, Bounded)

-- | Specification for an individual tab.
data Tab a body = Tab
  { tabKey :: !a
  , tabTitle :: !Text
  , tabClosable :: !Bool
  , tabDisabled :: !Bool
  , tabBadge :: !(Maybe Text)
  , tabBody :: !body
  }

-- | Response from the overall tabs container widget.
data TabResponse a = TabResponse
  { tabResponse :: !Response
  , tabClosed :: !(Maybe a)
  , tabActive :: !a
  }
  deriving (Eq, Show)

instance Responding (TabResponse a) where
  respId (TabResponse r _ _) = respId r
  respRect (TabResponse r _ _) = respRect r
  respHovered (TabResponse r _ _) = respHovered r
  respPressed (TabResponse r _ _) = respPressed r
  respClicked (TabResponse r _ _) = respClicked r
  respChanged (TabResponse r _ _) = respChanged r

instance Clickable (TabResponse a) where
  respIsClicked (TabResponse r _ _) = respClicked r

tabRespClicked :: TabResponse a -> Bool
tabRespClicked = respClicked

tabRespChanged :: TabResponse a -> Bool
tabRespChanged = respChanged

tabStyleIndex :: TabStyle -> Int
tabStyleIndex TabPill = 1
tabStyleIndex TabStylePill = 1
tabStyleIndex TabSegmented = 2
tabStyleIndex TabStyleSegmented = 2
tabStyleIndex TabContained = 3
tabStyleIndex TabStyleContained = 3
tabStyleIndex TabUnderline = 0
tabStyleIndex TabStyleUnderline = 0

tabOrientationDirection :: TabOrientation -> Direction
tabOrientationDirection TabHorizontal = Row
tabOrientationDirection TabTop = Row
tabOrientationDirection TabBottom = Row
tabOrientationDirection TabVertical = Column
tabOrientationDirection TabLeft = Column
tabOrientationDirection TabRight = Column

-- | Smart constructor for a standard non-closable tab.
tab :: a -> Text -> body -> Tab a body
tab key title body =
  Tab
    { tabKey = key
    , tabTitle = title
    , tabClosable = False
    , tabDisabled = False
    , tabBadge = Nothing
    , tabBody = body
    }

-- | Smart constructor for a closable tab.
closableTab :: a -> Text -> body -> Tab a body
closableTab key title body =
  Tab
    { tabKey = key
    , tabTitle = title
    , tabClosable = True
    , tabDisabled = False
    , tabBadge = Nothing
    , tabBody = body
    }

-- | Construct a tab with customizable options.
mkTab :: a -> Text -> Bool -> Bool -> Maybe Text -> body -> Tab a body
mkTab = Tab

tabHeaderHeight :: HostProfile -> Float
tabHeaderHeight host
  | isCellHost host = 1
  | otherwise = 28

tabHeaderLayout :: Context -> TabStyle -> Layout
tabHeaderLayout ctx _style =
  let
    h = tabHeaderHeight (ctxHostProfile ctx)
   in
    defaultLayout
      { layoutHeight = Fixed h
      , layoutPadding = Padding 8 8 4 4
      , layoutAlignX = AlignCenter
      , layoutAlignY = AlignMiddle
      , layoutGap = 4
      }

tabListContainerLayout :: Context -> TabStyle -> TabOrientation -> Layout
tabListContainerLayout ctx style orient =
  let
    h = tabHeaderHeight (ctxHostProfile ctx)
    dir = tabOrientationDirection orient
   in
    case orient of
      TabVertical ->
        defaultLayout
          { layoutDirection = dir
          , layoutWidth = Fit
          , layoutHeight = Grow 1
          , layoutGap = 2
          , layoutPadding = Padding 2 2 2 2
          }
      TabLeft ->
        defaultLayout
          { layoutDirection = dir
          , layoutWidth = Fit
          , layoutHeight = Grow 1
          , layoutGap = 2
          , layoutPadding = Padding 2 2 2 2
          }
      TabRight ->
        defaultLayout
          { layoutDirection = dir
          , layoutWidth = Fit
          , layoutHeight = Grow 1
          , layoutGap = 2
          , layoutPadding = Padding 2 2 2 2
          }
      _ ->
        defaultLayout
          { layoutDirection = dir
          , layoutWidth = Grow 1
          , layoutHeight = Fixed (h + 4)
          , layoutGap = if style == TabSegmented || style == TabStyleSegmented then 0 else 4
          , layoutPadding =
              if style == TabContained || style == TabStyleContained
                then Padding 0 0 2 0
                else Padding 0 0 0 0
          }

-- | Render a standard tab bar with panels.
tabs ::
  (Eq a, Ui :> es) =>
  a
  -> [Tab a (Eff es ())]
  -> Eff es (TabResponse a, a)
tabs curTab tabList = tabsEx TabStyleUnderline TabTop curTab tabList

-- | Render tabs with custom styling and orientation.
tabsEx ::
  (Eq a, Ui :> es) =>
  TabStyle
  -> TabOrientation
  -> a
  -> [Tab a (Eff es ())]
  -> Eff es (TabResponse a, a)
tabsEx style orient curTab tabList =
  -- Bar + body must share one parent. Sequential root siblings leave the body unpositioned.
  let
    shell layout = layout $ do
      (tabResp, nextTab) <- tabBarEx style orient curTab tabList
      renderActiveBody nextTab tabList
      pure (tabResp, nextTab)
   in
    case orient of
      TabVertical -> shell (row (tight . fillW . grow $ defaultLayout))
      TabLeft -> shell (row (tight . fillW . grow $ defaultLayout))
      TabRight -> shell (row (tight . fillW . grow $ defaultLayout))
      _ -> shell (column (tight . fillW . grow $ defaultLayout))
 where
  renderActiveBody activeKey ts =
    case filter (\t -> tabKey t == activeKey) ts of
      (selected : _) -> tabBody selected
      [] -> case ts of
        (firstTab : _) -> tabBody firstTab
        [] -> pure ()

-- | Render just the tab headers without rendering the tab bodies.
tabBar ::
  (Eq a, Ui :> es) =>
  a
  -> [Tab a body]
  -> Eff es (TabResponse a, a)
tabBar curTab tabList = tabBarEx TabStyleUnderline TabTop curTab tabList

-- | Render just the tab headers with styling options.
tabBarEx ::
  (Eq a, Ui :> es) =>
  TabStyle
  -> TabOrientation
  -> a
  -> [Tab a body]
  -> Eff es (TabResponse a, a)
tabBarEx style orient curTab tabList = do
  ctx <- askContext
  let
    containerLayout = tabListContainerLayout ctx style orient
    headerLayout = tabHeaderLayout ctx style
    styleVal = tabStyleIndex style

  case orient of
    TabVertical ->
      column containerLayout $ renderHeaders ctx headerLayout styleVal
    TabLeft ->
      column containerLayout $ renderHeaders ctx headerLayout styleVal
    TabRight ->
      column containerLayout $ renderHeaders ctx headerLayout styleVal
    _ ->
      row containerLayout $ renderHeaders ctx headerLayout styleVal
 where
  renderHeaders ctx headerLayout styleVal = do
    resps <-
      zipWithM
        (\i t -> withKey i (renderSingleHeader headerLayout styleVal t))
        [0 :: Int ..]
        tabList
    let
      clickedKeys = [k | (k, clicked, _, _) <- resps, clicked]
      closedKeys = [k | (_, _, Just k, _) <- resps]
      nextTab = case clickedKeys of
        (k : _) -> k
        [] -> curTab
      closedKey = case closedKeys of
        (k : _) -> Just k
        [] -> Nothing
      hasChanged = nextTab /= curTab
      hasClicked = not (null clickedKeys)
      baseResp = case resps of
        ((_, _, _, r) : _) -> r
        [] -> mkResponse (WidgetId 0) (Rect 0 0 0 0) False False False False
      modifiedResp = setChanged hasChanged (setClicked hasClicked baseResp)
      overallResp =
        TabResponse
          { tabResponse = modifiedResp
          , tabClosed = closedKey
          , tabActive = nextTab
          }
    -- Header activeVal is painted from nextTab (click frame), not lagged curTab.
    -- Active underline is node value, not text: damage alone would miss it.
    when (hasChanged || isJust closedKey) $ uiIO (markDirty ctx)
    when hasChanged $ uiIO (syncTabHeaderActive ctx nextTab resps)
    pure (overallResp, nextTab)

  renderSingleHeader headerLayout styleVal t = do
    let
      isActive = tabKey t == curTab
      activeVal = if isActive then 1 else 0
      baseTitle = tabTitle t
      badgeSuffix = case tabBadge t of
        Just b -> " (" <> b <> ")"
        Nothing -> ""
      headerText = tabButtonMarker <> baseTitle <> badgeSuffix

    if tabClosable t
      then do
        (tabResp, closed) <- row (tight defaultLayout) $ do
          wid <- nextId
          resp <-
            addWidgetStyled
              wid
              NodeButton
              headerText
              activeVal
              headerLayout
              styleVal
              Nothing
          closeWid <- nextId
          closeResp <-
            addWidgetStyled
              closeWid
              NodeButton
              (closeButtonMarker <> "×")
              0
              (headerLayout {layoutPadding = Padding 2 4 4 4})
              0
              Nothing
          pure (resp, respClicked closeResp)
        let
          closeKey = if closed then Just (tabKey t) else Nothing
        pure (tabKey t, respClicked tabResp && not closed, closeKey, tabResp)
      else do
        wid <- nextId
        resp <-
          addWidgetStyled
            wid
            NodeButton
            headerText
            activeVal
            headerLayout
            styleVal
            Nothing
        pure (tabKey t, respClicked resp, Nothing, resp)

-- | Patch tab button node values so the active underline matches selection this frame.
syncTabHeaderActive ::
  Eq a => Context -> a -> [(a, Bool, Maybe a, Response)] -> IO ()
syncTabHeaderActive ctx active resps = do
  let
    na = ctxNodeArena ctx
  count <- arenaCount na
  forM_ resps $ \(k, _, _, r) -> do
    let
      wid = respId r
      target = if k == active then 1 else 0
      go i
        | i >= count = pure ()
        | otherwise = do
            w <- getWidgetId na i
            if w == wid
              then setNodeValue na i target
              else go (i + 1)
    go 0

-- | Event-emitting variant of tabs that dispatches actions.
tabsEmit ::
  (Typeable action, Eq a, Ui :> es) =>
  (a -> action)
  -> a
  -> [Tab a (Eff es ())]
  -> Eff es (TabResponse a, a)
tabsEmit toAction curTab tabList = tabsEmitEx TabStyleUnderline TabTop toAction curTab tabList

-- | Event-emitting variant of tabs with full styling control.
tabsEmitEx ::
  (Typeable action, Eq a, Ui :> es) =>
  TabStyle
  -> TabOrientation
  -> (a -> action)
  -> a
  -> [Tab a (Eff es ())]
  -> Eff es (TabResponse a, a)
tabsEmitEx style orient toAction curTab tabList = do
  (tabResp, nextTab) <- tabsEx style orient curTab tabList
  when (tabRespClicked tabResp && nextTab /= curTab) $
    emit (toAction nextTab)
  pure (tabResp, nextTab)

-- | Uncontrolled tab hook storing the active tab in component local store.
useTab :: (Eq a, Show a, Read a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useTab initial = do
  (readTxt, setTxt) <- useText (T.pack (show initial))
  txt <- readTxt
  let
    current = case reads (T.unpack txt) of
      [(v, "")] -> v
      _ -> initial
    set v = when (v /= current) $ setTxt (T.pack (show v))
  pure (current, set)

-- | Uncontrolled tab hook by integer index.
useTabIdx :: Ui :> es => Int -> Eff es (Int, Int -> Eff es ())
useTabIdx = useTab

-- | Render a set of tabs for any bounded enumerable type.
boundedTabs ::
  (Bounded a, Enum a, Eq a, Show a, Read a, Ui :> es) =>
  a
  -> (a -> Text)
  -> (a -> Eff es ())
  -> Eff es ()
boundedTabs initial encodeTab tabf = do
  (curTab, setTab) <- useTab initial
  (tabResp, nextTab) <-
    tabs curTab (fmap (\x -> tab x (encodeTab x) (tabf x)) [minBound .. maxBound])
  when (tabRespChanged tabResp) (setTab nextTab)
