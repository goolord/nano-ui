{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Radio (radioFieldset, boundedRadioFieldset, enumRadio, useRadio) where

import Control.Monad (unless, void, when)
import qualified Data.IntMap.Strict as IM
import Data.Hashable (hash, hashWithSalt)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), getPrevRect, getStore, intKey, setStore)
import NanoUI.Icons (radioMark)
import NanoUI.Id (IdContext (..), WidgetId (..), mix64)
import NanoUI.Input (Input, inputMousePos)
import NanoUI.Layout.Arena
  ( NodeType (..)
  , addNodeFromLayout
  , setNodeText
  , setNodeValue
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..), slotKey)
import NanoUI.Style (Layout, defaultLayout, fillW, fontMuted, gap, tight)
import NanoUI.Types (Rect (..), isCellHost, rectContains, rectH, rectW)
import NanoUI.Widgets.Behavior (useSelection)
import NanoUI.Widgets.Combinators (selectableItem)
import NanoUI.Widgets.Layout (column')
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidgetStyled
  , mkResponse
  , parentIdx
  , resolveInteraction
  , setChanged
  , tagContainer
  )

radioLay :: Layout
radioLay = tight (fillW defaultLayout)

radioGroupLay :: Layout
radioGroupLay = tight (gap 4 (fillW defaultLayout))

legendLay :: Layout
legendLay = tight (fillW (fontMuted defaultLayout))

radioSalt :: Int
radioSalt = hash ("radio" :: Text)

radioFieldset :: (Ui :> es) => Text -> [Text] -> Int -> Eff es (Response, Int)
radioFieldset legend options initial =
  withKey (hashWithSalt radioSalt legend) $ do
    gid <- nextId
    ctx <- askContext
    let opts = if null options then [""] else options
        !len = length opts
        !c0 = max 0 (min (len - 1) initial)
        !key = intKey gid
        !keyInit = slotKey 1 key
    st0 <- uiIO (getStore ctx)
    let lastInit = IM.lookup keyInit (storeInt st0)
        storedSel = IM.lookup key (storeInt st0)
        !sel = case (lastInit, storedSel) of
          (Just li, Just s) | li == c0 -> max 0 (min (len - 1) s)
          _                            -> c0
    column' radioGroupLay $ do
      tagContainer gid
      unless (T.null legend) $ void (legendLabel legendLay legend)
      (combinedResp, clickedIdx) <-
        case opts of
          [l] -> do
            r <- bit ctx sel 0 l
            pure (r, if rawRespClicked r then 0 else -1)
          _ -> do
            inp <- askInput
            addRadioOptions ctx inp sel opts
      let !finalSel = if clickedIdx >= 0 then clickedIdx else sel
          !hasClick = clickedIdx >= 0
      uiIO $ do
        when (storedSel /= Just finalSel || lastInit /= Just c0) $ do
          st <- getStore ctx
          setStore ctx st
            { storeInt =
                IM.insert key finalSel $
                  IM.insert keyInit c0 (storeInt st)
            }
      pure (setChanged (finalSel /= sel || hasClick) combinedResp, finalSel)

bit :: Ui :> es => Context -> Int -> Int -> Text -> Eff es Response
bit ctx sel i l = do
  let on = sel == i
  selectableItem NodeRadio (if isCellHost (ctxHostProfile ctx) then radioMark (ctxIcons ctx) on <> l else l) on radioLay i

addRadioOptions :: Ui :> es => Context -> Input -> Int -> [Text] -> Eff es (Response, Int)
addRadioOptions ctx inp sel opts =
  uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    ic@(IdContext cid sid) <- readIORef (ctxIdContext ctx)
    pending <- readIORef (ctxClickedId ctx)
    let parent = parentIdx stack
        terminal = isCellHost (ctxHostProfile ctx)
        icons = ctxIcons ctx
        go !_ !_ [] !acc !clickedIdx =
          pure ((acc, clickedIdx), sid)
        go !i !s (l : ls) !acc !clickedIdx = do
          let raw = mix64 cid s
              wid = if raw == 0 then WidgetId 1 else WidgetId raw
          idx <- addNodeFromLayout (ctxNodeArena ctx) NodeRadio parent radioLay
          let on = sel == i
              txt = if terminal then radioMark icons on <> l else l
          setNodeText (ctxNodeArena ctx) idx txt
          setNodeValue (ctxNodeArena ctx) idx (if on then 1 else 0)
          setStyleIdx (ctxNodeArena ctx) idx i
          setWidgetId (ctxNodeArena ctx) idx wid
          r <- radioResponse ctx inp pending wid
          let !clickedIdx' = if rawRespClicked r && clickedIdx < 0 then i else clickedIdx
          go (i + 1) (s + 1) ls (acc <> r) clickedIdx'
    (result, sid') <- go 0 sid opts mempty (-1)
    writeIORef (ctxIdContext ctx) (ic {siblingId = sid'})
    pure result

radioResponse :: Context -> Input -> WidgetId -> WidgetId -> IO Response
radioResponse ctx inp pending wid = do
  mrect <- getPrevRect ctx wid
  let rect = maybe (Rect 0 0 0 0) id mrect
      mouse = inputMousePos inp
      underMouse = rectW rect > 0 && rectH rect > 0 && rectContains rect mouse
  if not underMouse && pending /= wid
    then pure (mkResponse wid rect False False False False)
    else resolveInteraction ctx inp wid

legendLabel :: (Ui :> es) => Layout -> Text -> Eff es Response
legendLabel layout txt = do
  wid <- nextId
  addWidgetStyled
    wid
    NodeText
    txt
    0
    layout
    0
    (Just (mkResponse wid (Rect 0 0 0 0) False False False False))

boundedRadioFieldset :: (Bounded a, Enum a, Ui :> es) => Text -> a -> (a -> Text) -> Eff es (Response, a)
boundedRadioFieldset legend initial encode =
  let vs = take 256 [minBound .. maxBound]
   in fmap (\(r, i) -> (r, toEnum (max 0 (min (length vs - 1) i)))) (radioFieldset legend (map encode vs) (fromEnum initial))

enumRadio :: (Bounded a, Enum a, Show a, Ui :> es) => Text -> a -> Eff es (Response, a)
enumRadio legend initial = boundedRadioFieldset legend initial (T.pack . show)

useRadio :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useRadio initial = fmap (\(c, s) -> (toEnum c, s . fromEnum)) (useSelection (fromEnum initial))
