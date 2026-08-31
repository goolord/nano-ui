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
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), markDirty)
import NanoUI.Host (isCellHost)
import NanoUI.Layout.Arena (arenaCount, getWidgetId, setNodeValue)
import NanoUI.Monad (Ui, askContext, emit, uiIO, withKey)
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
import NanoUI.WidgetText (closeButtonMarker, tabButtonMarker)
import NanoUI.Widgets.Behavior (useSelection)
import NanoUI.Widgets.Combinators (buttonStyled)
import NanoUI.Widgets.Layout (column, row)
import NanoUI.Widgets.Node
  ( Clickable (..)
  , Responding (..)
  , Response (..)
  , setChanged
  , setClicked
  )

data TabStyle = TabUnderline | TabPill | TabSegmented | TabContained
  deriving (Eq, Show, Enum, Bounded)

data TabOrientation = TabTop | TabBottom | TabLeft | TabRight
  deriving (Eq, Show, Enum, Bounded)

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
tabStyleIndex TabUnderline = 0
tabStyleIndex TabPill = 1
tabStyleIndex TabSegmented = 2
tabStyleIndex TabContained = 3

isVertical :: TabOrientation -> Bool
isVertical TabLeft = True
isVertical TabRight = True
isVertical _ = False

tab :: a -> Text -> body -> Tab a body
tab key title body = Tab key title False False Nothing body

closableTab :: a -> Text -> body -> Tab a body
closableTab key title body = Tab key title True False Nothing body

mkTab :: a -> Text -> Bool -> Bool -> Maybe Text -> body -> Tab a body
mkTab = Tab

headerLayout :: Context -> Layout
headerLayout ctx =
  let h = if isCellHost (ctxHostProfile ctx) then 1 else 28
   in defaultLayout
        { layoutHeight = Fixed h
        , layoutPadding = Padding 8 8 4 4
        , layoutAlignX = AlignCenter
        , layoutAlignY = AlignMiddle
        , layoutGap = 4
        }

listLayout :: Context -> TabStyle -> TabOrientation -> Layout
listLayout ctx style orient =
  let h = if isCellHost (ctxHostProfile ctx) then 1 else 28
      dir = if isVertical orient then Column else Row
      gapN = if style == TabSegmented then 0 else 4
      pad = if style == TabContained then Padding 0 0 2 0 else Padding 0 0 0 0
   in if isVertical orient
        then defaultLayout {layoutDirection = dir, layoutWidth = Fit, layoutHeight = Grow 1, layoutGap = 2, layoutPadding = Padding 2 2 2 2}
        else defaultLayout {layoutDirection = dir, layoutWidth = Grow 1, layoutHeight = Fixed (h + 4), layoutGap = gapN, layoutPadding = pad}

renderActiveBody :: (Eq a, Ui :> es) => [Tab a (Eff es ())] -> a -> Eff es ()
renderActiveBody ts activeKey =
  column (tight . fillW $ defaultLayout) $
    case filter (\t -> tabKey t == activeKey) ts of
      (selected : _) -> tabBody selected
      [] -> case ts of
        (firstTab : _) -> tabBody firstTab
        [] -> pure ()

renderTabsCore ::
  (Eq a, Ui :> es) =>
  TabStyle ->
  TabOrientation ->
  a ->
  [Tab a body] ->
  Maybe (a -> Eff es ()) ->
  Eff es (TabResponse a, a)
renderTabsCore style orient curTab tabList mRenderBody = do
  ctx <- askContext
  let barLay = listLayout ctx style orient
      hdrLay = headerLayout ctx
      styleVal = tabStyleIndex style
      headerBar =
        (if isVertical orient then column else row) barLay (renderHeaders ctx hdrLay styleVal)
  case mRenderBody of
    Nothing -> headerBar
    Just renderBody ->
      let shell layout = layout $ do
            (tabResp, nextTab) <- headerBar
            renderBody nextTab
            pure (tabResp, nextTab)
       in if isVertical orient
            then shell (row (tight . fillW . grow $ defaultLayout))
            else shell (column (tight . fillW $ defaultLayout))
 where
  renderHeaders ctx hdrLay styleVal = do
    resps <-
      zipWithM
        (\i t -> withKey i (renderSingleHeader hdrLay styleVal t))
        [0 :: Int ..]
        tabList
    let clickedKeys = [k | (k, clicked, _, _) <- resps, clicked]
        closedKeys = [k | (_, _, Just k, _) <- resps]
        nextTab = case clickedKeys of
          (k : _) -> k
          [] -> curTab
        closedKey = case closedKeys of
          (k : _) -> Just k
          [] -> Nothing
        hasChanged = nextTab /= curTab
        hasClicked = not (null clickedKeys)
        overallResp =
          TabResponse
            { tabResponse = setChanged hasChanged (setClicked hasClicked (mconcat [r | (_, _, _, r) <- resps]))
            , tabClosed = closedKey
            , tabActive = nextTab
            }
    when (hasChanged || isJust closedKey) $ uiIO (markDirty ctx)
    when hasChanged $ uiIO (syncTabHeaderActive ctx nextTab resps)
    pure (overallResp, nextTab)

  renderSingleHeader hdrLay styleVal t = do
    let isActive = tabKey t == curTab
        activeVal = if isActive then 1 else 0
        badge = maybe "" (\b -> " (" <> b <> ")") (tabBadge t)
        headerText = tabButtonMarker <> tabTitle t <> badge
    if tabClosable t
      then do
        (tabResp, closed) <- row (tight defaultLayout) $ do
          resp <- buttonStyled headerText activeVal hdrLay styleVal
          closeResp <-
            buttonStyled
              (closeButtonMarker <> "\215")
              0
              (hdrLay {layoutPadding = Padding 2 4 4 4})
              0
          pure (resp, respClicked closeResp)
        let closeKey = if closed then Just (tabKey t) else Nothing
        pure (tabKey t, respClicked tabResp && not closed, closeKey, tabResp)
      else do
        resp <- buttonStyled headerText activeVal hdrLay styleVal
        pure (tabKey t, respClicked resp, Nothing, resp)

tabs :: (Eq a, Ui :> es) => a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabs = tabsEx TabUnderline TabTop

tabsEx :: (Eq a, Ui :> es) => TabStyle -> TabOrientation -> a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabsEx style orient cur tabList =
  renderTabsCore style orient cur tabList (Just (renderActiveBody tabList))

tabBar :: (Eq a, Ui :> es) => a -> [Tab a body] -> Eff es (TabResponse a, a)
tabBar = tabBarEx TabUnderline TabTop

tabBarEx :: (Eq a, Ui :> es) => TabStyle -> TabOrientation -> a -> [Tab a body] -> Eff es (TabResponse a, a)
tabBarEx style orient cur tabList = renderTabsCore style orient cur tabList Nothing

syncTabHeaderActive :: Eq a => Context -> a -> [(a, Bool, Maybe a, Response)] -> IO ()
syncTabHeaderActive ctx active resps = do
  let na = ctxNodeArena ctx
  count <- arenaCount na
  forM_ resps $ \(k, _, _, r) -> do
    let wid = respId r
        target = if k == active then 1 else 0
        go i
          | i >= count = pure ()
          | otherwise = do
              w <- getWidgetId na i
              if w == wid then setNodeValue na i target else go (i + 1)
    go 0

tabsEmit :: (Typeable action, Eq a, Ui :> es) => (a -> action) -> a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabsEmit = tabsEmitEx TabUnderline TabTop

tabsEmitEx ::
  (Typeable action, Eq a, Ui :> es) =>
  TabStyle ->
  TabOrientation ->
  (a -> action) ->
  a ->
  [Tab a (Eff es ())] ->
  Eff es (TabResponse a, a)
tabsEmitEx style orient toAction cur tabList = do
  (tabResp, nextTab) <- renderTabsCore style orient cur tabList (Just (renderActiveBody tabList))
  when (tabRespClicked tabResp && nextTab /= cur) $ emit (toAction nextTab)
  pure (tabResp, nextTab)

useTabIdx :: (Ui :> es) => Int -> Eff es (Int, Int -> Eff es ())
useTabIdx = useSelection

useTab :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useTab initial = do
  (cur, set) <- useSelection (fromEnum initial)
  pure (toEnum cur, set . fromEnum)

boundedTabs ::
  (Bounded a, Enum a, Eq a, Ui :> es) =>
  a ->
  (a -> Text) ->
  (a -> Eff es ()) ->
  Eff es ()
boundedTabs initial encodeTab tabf = do
  (curTab, setTab) <- useTab initial
  (tabResp, nextTab) <-
    tabs curTab (fmap (\x -> tab x (encodeTab x) (tabf x)) [minBound .. maxBound])
  when (tabRespChanged tabResp) (setTab nextTab)
