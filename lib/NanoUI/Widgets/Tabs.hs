{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Tabs
  ( Tab (..), TabStyle (..), TabOrientation (..), TabResponse (..)
  , tabRespClicked, tabRespChanged, tab, closableTab, mkTab
  , tabs, tabsEx, tabBar, tabBarEx, tabsEmit, tabsEmitEx
  , useTab, useTabIdx, boundedTabs
  )
where

import Control.Monad (forM_, when, zipWithM)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), markDirty)
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Layout.Arena (setNodeValue)
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
import NanoUI.Types (isCellHost)
import NanoUI.WidgetText (closeButtonMarker, tabButtonMarker)
import NanoUI.Widgets.Combinators (buttonStyled)
import NanoUI.Widgets.Behavior (useSelection)
import NanoUI.Widgets.Layout (column, row)
import NanoUI.Widgets.Node
  ( Clickable (..)
  , RightClickable (..)
  , Responding (..)
  , Response (..)
  , setChanged
  , setClicked
  , tagContainer
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
  respRightPressed (TabResponse r _ _) = respRightPressed r
  respRightClicked (TabResponse r _ _) = respRightClicked r

instance Clickable (TabResponse a) where
  respIsClicked (TabResponse r _ _) = respClicked r

instance RightClickable (TabResponse a) where
  respIsRightClicked (TabResponse r _ _) = respRightClicked r

tabRespClicked :: TabResponse a -> Bool
tabRespClicked = respClicked

tabRespChanged :: TabResponse a -> Bool
tabRespChanged = respChanged

tab :: a -> Text -> body -> Tab a body
tab key title body = Tab key title False False Nothing body

closableTab :: a -> Text -> body -> Tab a body
closableTab key title body = Tab key title True False Nothing body

mkTab :: a -> Text -> Bool -> Bool -> Maybe Text -> body -> Tab a body
mkTab = Tab

tabStrip ::
  (Eq a, Ui :> es) =>
  TabStyle ->
  TabOrientation ->
  a ->
  [Tab a body] ->
  Maybe (a -> Eff es ()) ->
  Eff es (TabResponse a, a)
tabStrip style orient cur tabList mRenderBody = do
  ctx <- askContext
  groupId <- nextId
  let vertical = orient == TabLeft || orient == TabRight
      h = if isCellHost (ctxHostProfile ctx) then 1 else 28
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
      headerBar =
        (if vertical then column else row) barLay $ do
          tagContainer groupId
          renderHeaders ctx hdrLay styleVal
  case mRenderBody of
    Nothing -> headerBar
    Just bodyRender ->
      let shell layout = layout $ do
            (tabResp, nextTab) <- headerBar
            bodyRender nextTab
            pure (tabResp, nextTab)
       in if vertical
            then shell (row (tight . fillW . grow $ defaultLayout))
            else shell (column (tight . fillW $ defaultLayout))
 where
  renderHeaders ctx hdrLay styleVal = do
    resps <- zipWithM (\i t -> withKey i (renderSingleHeader hdrLay (styleVal + 4 * i) t)) [0 :: Int ..] tabList
    let clickedKeys = [k | (k, clicked, _, _) <- resps, clicked]
        closedKey = listToMaybe [k | (_, _, Just k, _) <- resps]
        nextTab = case clickedKeys of
          (k : _) -> k
          [] -> cur
        hasChanged = nextTab /= cur
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

  renderSingleHeader hdrLay packedStyle t = do
    let isActive = tabKey t == cur
        badge = maybe "" (\b -> " (" <> b <> ")") (tabBadge t)
        headerText = tabButtonMarker <> tabTitle t <> badge
    if tabClosable t
      then do
        (tabResp, closed) <- row (tight defaultLayout) $ do
          resp <- buttonStyled headerText (if isActive then 1 else 0) hdrLay packedStyle
          closeResp <- buttonStyled (closeButtonMarker <> "\215") 0 (hdrLay {layoutPadding = Padding 2 4 4 4}) 0
          pure (resp, respClicked closeResp)
        pure (tabKey t, respClicked tabResp && not closed, if closed then Just (tabKey t) else Nothing, tabResp)
      else do
        resp <- buttonStyled headerText (if isActive then 1 else 0) hdrLay packedStyle
        pure (tabKey t, respClicked resp, Nothing, resp)

syncTabHeaderActive :: Eq a => Context -> a -> [(a, Bool, Maybe a, Response)] -> IO ()
syncTabHeaderActive ctx active resps =
  forM_ resps $ \(k, _, _, r) -> do
    mIdx <- findNodeByWidgetId ctx (respId r)
    case mIdx of
      Just i -> setNodeValue (ctxNodeArena ctx) i (if k == active then 1 else 0)
      Nothing -> pure ()

tabs :: (Eq a, Ui :> es) => a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabs = tabsEx TabUnderline TabTop

tabsEx :: (Eq a, Ui :> es) => TabStyle -> TabOrientation -> a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabsEx style orient cur ts = tabStrip style orient cur ts (Just (renderBody ts))

tabBar :: (Eq a, Ui :> es) => a -> [Tab a body] -> Eff es (TabResponse a, a)
tabBar = tabBarEx TabUnderline TabTop

tabBarEx :: (Eq a, Ui :> es) => TabStyle -> TabOrientation -> a -> [Tab a body] -> Eff es (TabResponse a, a)
tabBarEx style orient cur ts = tabStrip style orient cur ts Nothing

tabsEmit :: (Typeable action, Eq a, Ui :> es) => (a -> action) -> a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabsEmit = tabsEmitEx TabUnderline TabTop

tabsEmitEx :: (Typeable action, Eq a, Ui :> es) => TabStyle -> TabOrientation -> (a -> action) -> a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabsEmitEx style orient toAction cur ts = do
  (tabResp, nextTab) <- tabStrip style orient cur ts (Just (renderBody ts))
  when (tabRespClicked tabResp && nextTab /= cur) $ emit (toAction nextTab)
  pure (tabResp, nextTab)

useTabIdx :: (Ui :> es) => Int -> Eff es (Int, Int -> Eff es ())
useTabIdx = useSelection

useTab :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useTab initial = fmap (\(c, s) -> (toEnum c, s . fromEnum)) (useSelection (fromEnum initial))

boundedTabs :: (Bounded a, Enum a, Eq a, Ui :> es) => a -> (a -> Text) -> (a -> Eff es ()) -> Eff es ()
boundedTabs initial encodeTab tabf = do
  (curTab, setTab) <- useTab initial
  (tabResp, nextTab) <- tabs curTab (fmap (\x -> tab x (encodeTab x) (tabf x)) [minBound .. maxBound])
  when (tabRespChanged tabResp) (setTab nextTab)

renderBody :: (Eq a, Ui :> es) => [Tab a (Eff es ())] -> a -> Eff es ()
renderBody ts activeKey =
  column (tight . fillW $ defaultLayout) $
    case filter (\t -> tabKey t == activeKey) ts of
      (selected : _) -> tabBody selected
      [] -> case ts of { (firstTab : _) -> tabBody firstTab; [] -> pure () }
