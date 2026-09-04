{-# LANGUAGE BangPatterns #-}

module NanoUI.Rgfw.Layout
  ( solveSinglePassLayout
  , solveSinglePassLayoutWith
  , computePopupPosition
  , getContentHeight
  , getContentWidth
  ) where

import Control.Monad (when)
import Data.Primitive.PrimArray
  ( newPrimArray
  , readPrimArray
  , setPrimArray
  , writePrimArray
  )
import qualified Data.Text as T
import NanoUI
  ( Padding (..)
  , PopupAnchor (..)
  , PopupPlacement (..)
  , Rect (..)
  , V2 (..)
  , WidgetId (..)
  )
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeArena
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getClipRect
  , getDirection
  , getGap
  , getHeightSizing
  , getMinMax
  , getNodeType
  , getPadding
  , getParent
  , getRect
  , getText
  , getWidgetId
  , getWidthSizing
  , isContainerNode
  , isFloatingNode
  , setClipRect
  , setRect
  , snapshotLayoutRects
  , withArenaArraysSnap
  )

-- | Compute popup floating position clamped to screen bounds
computePopupPosition ::
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  PopupAnchor ->
  PopupPlacement ->
  Float ->
  (Float, Float)
computePopupPosition !winW !winH !margin !iw !ih !anchor !placement !offset =
  case anchor of
    AnchorPoint (V2 px py) ->
      let !x0 = case placement of
            PlacementLeft -> px - iw - offset
            PlacementRight -> px + offset
            _ -> px
          !y0 = case placement of
            PlacementAbove -> py - ih - offset
            PlacementBelow -> py + offset
            _ -> py
          !x = if x0 + iw > winW - margin && px - iw - margin >= 0
                then px - iw - offset
                else max margin (min (winW - iw - margin) x0)
          !y = if y0 + ih > winH - margin && py - ih - margin >= 0
                then py - ih - offset
                else max margin (min (winH - ih - margin) y0)
       in (x, y)
    AnchorRect (Rect rx ry rw rh) ->
      case placement of
        PlacementBelow ->
          let !x0 = rx
              !y0 = ry + rh + offset
              !y = if y0 + ih > winH - margin && ry - ih - offset >= margin
                    then ry - ih - offset
                    else y0
              !x = max margin (min (winW - iw - margin) x0)
           in (x, max margin (min (winH - ih - margin) y))
        PlacementAbove ->
          let !x0 = rx
              !y0 = ry - ih - offset
              !y = if y0 < margin && ry + rh + offset + ih <= winH - margin
                    then ry + rh + offset
                    else y0
              !x = max margin (min (winW - iw - margin) x0)
           in (x, max margin (min (winH - ih - margin) y))
        PlacementRight ->
          let !x0 = rx + rw + offset
              !y0 = ry
              !x = if x0 + iw > winW - margin && rx - iw - offset >= margin
                    then rx - iw - offset
                    else x0
              !y = max margin (min (winH - ih - margin) y0)
           in (max margin (min (winW - iw - margin) x), y)
        PlacementLeft ->
          let !x0 = rx - iw - offset
              !y0 = ry
              !x = if x0 < margin && rx + rw + offset + iw <= winW - margin
                    then rx + rw + offset
                    else x0
              !y = max margin (min (winH - ih - margin) y0)
           in (max margin (min (winW - iw - margin) x), y)
        PlacementAuto ->
          let !spaceBelow = winH - margin - (ry + rh + offset)
              !spaceAbove = ry - offset - margin
              !y = if spaceBelow >= ih || spaceBelow >= spaceAbove
                    then ry + rh + offset
                    else ry - ih - offset
              !x = max margin (min (winW - iw - margin) rx)
           in (x, max margin (min (winH - ih - margin) y))
        PlacementAtCursor ->
          (max margin (min (winW - iw - margin) rx), max margin (min (winH - ih - margin) (ry + rh + offset)))

-- | Efficient, lean O(N) single-pass layout engine.
solveSinglePassLayout :: NodeArena -> Float -> Float -> IO ()
solveSinglePassLayout na !viewportW !viewportH =
  solveSinglePassLayoutWith na viewportW viewportH (\_ -> pure Nothing) (\_ -> pure Nothing) (\_ -> pure Nothing)

solveSinglePassLayoutWith ::
  NodeArena ->
  Float ->
  Float ->
  (WidgetId -> IO (Maybe (PopupAnchor, PopupPlacement, Float))) ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  IO ()
solveSinglePassLayoutWith na !viewportW !viewportH lookupPopup lookupWindowPos lookupWindowSize = do
  !n <- arenaCount na
  when (n > 0) $ do
    -- 1. Build child linked lists in unboxed arrays: headChildArr and nextSibArr
    headChildArr <- newPrimArray n
    nextSibArr   <- newPrimArray n
    setPrimArray headChildArr 0 n (-1)
    setPrimArray nextSibArr 0 n (-1)

    withArenaArraysSnap na $ do
      let buildLists !i
            | i < 0 = pure ()
            | otherwise = do
                p <- getParent na i
                nt <- getNodeType na i
                when (p >= 0 && p < n && not (isFloatingNode nt)) $ do
                  oldHead <- readPrimArray headChildArr p
                  writePrimArray nextSibArr i oldHead
                  writePrimArray headChildArr p i
                buildLists (i - 1)
      buildLists (n - 1)

      -- Precompute overlay hierarchy (floating windows, modals, popups and their descendants)
      isOverlayArr <- newPrimArray n
      setPrimArray isOverlayArr 0 n (0 :: Int)
      isPopupArr <- newPrimArray n
      setPrimArray isPopupArr 0 n (0 :: Int)
      let prepOverlay !i
            | i >= n = pure ()
            | otherwise = do
                nt <- getNodeType na i
                p <- getParent na i
                isPOverlay <- if p >= 0 && p < n then readPrimArray isOverlayArr p else pure 0
                let !isO = if isFloatingNode nt || isPOverlay == 1 then 1 else 0
                writePrimArray isOverlayArr i isO
                isPPopup <- if p >= 0 && p < n then readPrimArray isPopupArr p else pure 0
                let !isPop = if nt == NodePopup || isPPopup == 1 then 1 else 0
                writePrimArray isPopupArr i isPop
                prepOverlay (i + 1)
      prepOverlay 0

      -- 2. Pass 1: Bottom-up intrinsic / content size computation (i = n-1 down to 0)
      reqWArr <- newPrimArray n
      reqHArr <- newPrimArray n
      remAfterWArr <- newPrimArray n
      remAfterHArr <- newPrimArray n
      setPrimArray remAfterWArr 0 n 0
      setPrimArray remAfterHArr 0 n 0

      let pass1 !i
            | i < 0 = pure ()
            | otherwise = do
                nt <- getNodeType na i
                (wTag, wVal) <- getWidthSizing na i
                (hTag, hVal) <- getHeightSizing na i
                rawPad <- getPadding na i
                rawGap <- getGap na i
                dir <- getDirection na i
                (minW, minH, maxW, maxH) <- getMinMax na i
                txt <- getText na i
                let !dispTxt =
                      if T.isPrefixOf "\x01" txt || T.isPrefixOf "\x02" txt || T.isPrefixOf "\x03" txt
                        then T.drop 1 txt
                        else txt
                    !txtLines = T.splitOn "\n" dispTxt
                    !lineCount = max 1 (length txtLines)
                    !maxLineLen = fromIntegral (maximum (0 : map T.length txtLines))
                    !txtLen = maxLineLen
                    !isClose = T.isPrefixOf "\x01" txt || txt == "[X]" || txt == "X" || txt == "\xd7" || txt == "\xf00d"
                isPop <- readPrimArray isPopupArr i

                -- For context menus / popups: compact 2px padding and zero gap between items
                let !pad = if nt == NodePopup then Padding 2 2 2 2 else rawPad
                    !gap = if isPop == 1 then 0 else rawGap

                if isContainerNode nt
                  then do
                    -- Container content size from direct children
                    let loopChildren !c (!count :: Int) !totMain !maxCross
                          | c < 0 = pure (count, totMain, maxCross)
                          | otherwise = do
                              cw <- readPrimArray reqWArr c
                              ch <- readPrimArray reqHArr c
                              (cWTag, _) <- getWidthSizing na c
                              (cHTag, _) <- getHeightSizing na c
                              next <- readPrimArray nextSibArr c
                              if dir == DirColumn
                                then do
                                  let !mSize = if cHTag == SizingGrow then max 0 ch else ch
                                      !tot = totMain + mSize + (if count > 0 then gap else 0)
                                      !cross = max maxCross cw
                                  loopChildren next (count + 1) tot cross
                                else do
                                  let !mSize = if cWTag == SizingGrow then max 0 cw else cw
                                      !tot = totMain + mSize + (if count > 0 then gap else 0)
                                      !cross = max maxCross ch
                                  loopChildren next (count + 1) tot cross

                    firstChild <- readPrimArray headChildArr i
                    (!cCount, !cTot, !cCross) <- loopChildren firstChild 0 0 0

                    let (!contentW, !contentH) =
                          if dir == DirColumn
                            then (cCross + padL pad + padR pad, (if cCount > 0 then cTot else 0) + padT pad + padB pad)
                            else ((if cCount > 0 then cTot else 0) + padL pad + padR pad, cCross + padT pad + padB pad)

                    -- Precalculate remAfter for children of this container
                    let calcRemAfter !c
                          | c < 0 = pure ()
                          | otherwise = do
                              next <- readPrimArray nextSibArr c
                              let sumAfter !s !acc
                                    | s < 0 = pure acc
                                    | otherwise = do
                                        (sWTag, _) <- getWidthSizing na s
                                        (sHTag, _) <- getHeightSizing na s
                                        sw <- readPrimArray reqWArr s
                                        sh <- readPrimArray reqHArr s
                                        sNext <- readPrimArray nextSibArr s
                                        if dir == DirRow
                                          then do
                                            let !add = if sWTag == SizingGrow then max 0 sw + gap else sw + gap
                                            sumAfter sNext (acc + add)
                                          else do
                                            let !add = if sHTag == SizingGrow then max 0 sh + gap else sh + gap
                                            sumAfter sNext (acc + add)

                              !remAfter <- sumAfter next 0
                              if dir == DirRow
                                then writePrimArray remAfterWArr c remAfter
                                else writePrimArray remAfterHArr c remAfter
                              calcRemAfter next

                    calcRemAfter firstChild

                    if nt == NodeWindow
                      then do
                        wid <- getWidgetId na i
                        mStoredSz <- lookupWindowSize wid
                        let (!rw, !rh) = case mStoredSz of
                              Just (sw, sh) | sw > 0 && sh > 0 -> (sw, sh)
                              _ -> (max 320 contentW, max 200 contentH)
                        writePrimArray reqWArr i (max minW (min maxW rw))
                        writePrimArray reqHArr i (max minH (min maxH rh))
                      else do
                        let !minPopW = 80
                            !rw = if nt == NodePopup
                                    then max minPopW contentW
                                    else if wTag == SizingFixed && wVal > 0 then wVal else contentW
                            !rh = if hTag == SizingFixed && hVal > 0 then hVal else contentH
                        writePrimArray reqWArr i (max minW (min maxW rw))
                        writePrimArray reqHArr i (max minH (min maxH rh))

                  else do
                    -- Leaf node
                    let !leafW =
                          if wTag == SizingFixed && wVal > 0
                            then wVal
                            else case nt of
                              NodeText        -> if isPop == 1 && padL pad == 0
                                                   then maxLineLen * 6 + 10
                                                   else maxLineLen * 6 + padL pad + padR pad
                              NodeButton      -> if isClose
                                                   then if isPop == 1 then 17 else 24
                                                   else if isPop == 1
                                                     then txtLen * 6 + 10
                                                     else txtLen * 6 + 16
                              NodeCheckbox    -> txtLen * 6 + 24
                              NodeRadio       -> txtLen * 6 + 24
                              NodeSlider      -> 120
                              NodeTextInput   -> 160
                              NodeTextArea    -> 200
                              NodeSeparator   -> if isPop == 1 then 20 else 2
                              NodeSpacer      -> if wVal > 0 then wVal else 8
                              NodeBox         -> if wVal > 0 then wVal else 20
                              _               -> 60

                        !leafH =
                          if hTag == SizingFixed && hVal > 0
                            then hVal
                            else case nt of
                              NodeText        -> if isPop == 1 && padT pad == 0 && padB pad == 0
                                                   then 17
                                                   else fromIntegral lineCount * 13 + padT pad + padB pad
                              NodeButton      -> if isClose
                                                   then if isPop == 1 then 17 else 24
                                                   else if isPop == 1 then 17 else 21
                              NodeCheckbox    -> 18
                              NodeRadio       -> 18
                              NodeSlider      -> 20
                              NodeTextInput   -> 21
                              NodeTextArea    -> 80
                              NodeSeparator   -> if isPop == 1 then 5 else 2
                              NodeSpacer      -> if hVal > 0 then hVal else 8
                              NodeBox         -> if hVal > 0 then hVal else 20
                              _               -> 21

                    writePrimArray reqWArr i (max minW (min maxW leafW))
                    writePrimArray reqHArr i (max minH (min maxH leafH))

                pass1 (i - 1)
      pass1 (n - 1)

      -- 3. Pass 2: Top-down positioning and Grow distribution (i = 0 to n-1)
      curXArr   <- newPrimArray n
      curYArr   <- newPrimArray n
      innerXArr <- newPrimArray n
      innerYArr <- newPrimArray n
      innerWArr <- newPrimArray n
      innerHArr <- newPrimArray n

      let pass2 !i
            | i >= n = pure ()
            | otherwise = do
                p <- getParent na i
                nt <- getNodeType na i
                (wTag, wVal) <- getWidthSizing na i
                (hTag, hVal) <- getHeightSizing na i
                pad <- getPadding na i
                (minW, minH, maxW, maxH) <- getMinMax na i
                reqW <- readPrimArray reqWArr i
                reqH <- readPrimArray reqHArr i

                if nt == NodeWindow || nt == NodeModal
                  then do
                    -- Floating Window/Modal: placed at stored pos or top-right, over rest of UI
                    wid <- getWidgetId na i
                    mStoredPos <- lookupWindowPos wid
                    mStoredSz  <- lookupWindowSize wid
                    let !effMinW = if minW > 0 then minW else 160.0
                        !effMinH = if minH > 0 then minH else 60.0
                        !maxAvailW = max effMinW (viewportW - 16.0)
                        !maxAvailH = max effMinH (viewportH - 16.0)
                        (!winW, !winH) = case mStoredSz of
                          Just (sw, sh) | sw > 0 && sh > 0 ->
                            (max effMinW (min maxAvailW sw), max effMinH (min maxAvailH sh))
                          _ ->
                            let !defW = if wTag == SizingFixed && wVal > 0 then wVal else min (viewportW * 0.8) (max 320 reqW)
                                !defH = if hTag == SizingFixed && hVal > 0 then hVal else min (viewportH * 0.8) (max 200 reqH)
                             in (max effMinW (min (if maxW > 0 then maxW else maxAvailW) defW),
                                 max effMinH (min (if maxH > 0 then maxH else maxAvailH) defH))
                        (!winX, !winY) = case mStoredPos of
                          Just (px, py) ->
                            (max 0 (min (viewportW - winW) px), max 0 (min (viewportH - winH) py))
                          Nothing ->
                            -- Default: top right with 16px margin, below top bar (y = 32)
                            (max 10.0 (viewportW - winW - 16.0), 32.0)

                    setRect na i winX winY winW winH
                    setClipRect na i (Rect winX winY winW winH)

                    let !ix = winX + padL pad
                        !iy = winY + padT pad
                        !iw = max 0 (winW - padL pad - padR pad)
                        !ih = max 0 (winH - padT pad - padB pad)
                    writePrimArray innerXArr i ix
                    writePrimArray innerYArr i iy
                    writePrimArray innerWArr i iw
                    writePrimArray innerHArr i ih
                    writePrimArray curXArr i ix
                    writePrimArray curYArr i iy

                else if p < 0
                  then do
                    -- Root node
                    let (!x, !y, !w, !h) = (0, 0, viewportW, viewportH)
                    setRect na i x y w h
                    setClipRect na i (Rect x y 1e9 1e9)

                    let !ix = x + padL pad
                        !iy = y + padT pad
                        !iw = max 0 (w - padL pad - padR pad)
                        !ih = max 0 (h - padT pad - padB pad)
                    writePrimArray innerXArr i ix
                    writePrimArray innerYArr i iy
                    writePrimArray innerWArr i iw
                    writePrimArray innerHArr i ih
                    writePrimArray curXArr i ix
                    writePrimArray curYArr i iy

                  else if nt == NodePopup
                    then do
                      -- Floating Popup: placed at anchor / cursor, unclipped by parent container
                      wid <- getWidgetId na i
                      mcfg <- lookupPopup wid
                      let (anchor, placement, offset) = case mcfg of
                            Just (a, pl, o) -> (a, pl, o)
                            Nothing -> (AnchorPoint (V2 0 0), PlacementAuto, 4)
                          !minPopupW = 80
                          !minPopupH = 18
                          !popupW = max minPopupW reqW
                          !popupH = max minPopupH reqH
                          (!popupX, !popupY) = computePopupPosition viewportW viewportH 4 popupW popupH anchor placement offset
                      setRect na i popupX popupY popupW popupH
                      setClipRect na i (Rect popupX popupY popupW popupH)

                      let !effPad = Padding 2 2 2 2
                          !ix = popupX + padL effPad
                          !iy = popupY + padT effPad
                          !iw = max 0 (popupW - padL effPad - padR effPad)
                          !ih = max 0 (popupH - padT effPad - padB effPad)
                      writePrimArray innerXArr i ix
                      writePrimArray innerYArr i iy
                      writePrimArray innerWArr i iw
                      writePrimArray innerHArr i ih
                      writePrimArray curXArr i ix
                      writePrimArray curYArr i iy

                  else do
                    -- Child node
                    pDir <- getDirection na p
                    rawPGap <- getGap na p
                    pIsPop <- readPrimArray isPopupArr p
                    let !pGap = if pIsPop == 1 then 0 else rawPGap
                    cx <- readPrimArray curXArr p
                    cy <- readPrimArray curYArr p
                    pix <- readPrimArray innerXArr p
                    piy <- readPrimArray innerYArr p
                    piw <- readPrimArray innerWArr p
                    pih <- readPrimArray innerHArr p
                    remAfterW <- readPrimArray remAfterWArr i
                    remAfterH <- readPrimArray remAfterHArr i

                    mParentClip <- getClipRect na p
                    let !parentClip = case mParentClip of
                          Just r -> r
                          Nothing -> Rect 0 0 viewportW viewportH

                    -- Compute Width
                    let !availW = max 0 (pix + piw - cx)
                        !w =
                          if wTag == SizingFixed && wVal > 0
                            then wVal
                            else if pDir == DirRow
                              then if wTag == SizingGrow
                                then max reqW (availW - remAfterW)
                                else reqW
                              else if wTag == SizingGrow
                                then max reqW piw
                                else reqW

                    -- Compute Height
                    let !availH = max 0 (piy + pih - cy)
                        !h =
                          if hTag == SizingFixed && hVal > 0
                            then hVal
                            else if pDir == DirColumn
                              then if hTag == SizingGrow
                                then max reqH (availH - remAfterH)
                                else reqH
                              else if hTag == SizingGrow
                                then max reqH pih
                                else reqH

                    let !finalW = max minW (min maxW w)
                        !finalH = max minH (min maxH h)
                        !x = cx
                        !y = cy

                    setRect na i x y finalW finalH

                    let !cx0 = max (rectX parentClip) x
                        !cy0 = max (rectY parentClip) y
                        !cx1 = min (rectX parentClip + rectW parentClip) (x + finalW)
                        !cy1 = min (rectY parentClip + rectH parentClip) (y + finalH)
                        !childClip = Rect cx0 cy0 (max 0 (cx1 - cx0)) (max 0 (cy1 - cy0))
                    setClipRect na i childClip

                    -- Advance parent cursor
                    if pDir == DirColumn
                      then writePrimArray curYArr p (cy + finalH + pGap)
                      else writePrimArray curXArr p (cx + finalW + pGap)

                    -- If container, initialize its inner bounds and cursor
                    when (isContainerNode nt) $ do
                      let !cix = x + padL pad
                          !ciy = y + padT pad
                          !ciw = max 0 (finalW - padL pad - padR pad)
                          !cih = max 0 (finalH - padT pad - padB pad)
                      writePrimArray innerXArr i cix
                      writePrimArray innerYArr i ciy
                      writePrimArray innerWArr i ciw
                      writePrimArray innerHArr i cih
                      writePrimArray curXArr i cix
                      writePrimArray curYArr i ciy

                pass2 (i + 1)
      pass2 0

    snapshotLayoutRects na

-- | Calculate total content bottom extent across all non-floating children in the arena.
getContentHeight :: NodeArena -> IO Float
getContentHeight na = do
  !n <- arenaCount na
  let isFloating !idx = do
        nt <- getNodeType na idx
        if isFloatingNode nt
          then pure True
          else do
            p <- getParent na idx
            if p < 0 then pure False else isFloating p
      loop !i !acc
        | i >= n = pure acc
        | otherwise = do
            p <- getParent na i
            floating <- isFloating i
            if p >= 0 && not floating
              then do
                (_, ry, _, rh) <- getRect na i
                loop (i + 1) (max acc (ry + rh))
              else loop (i + 1) acc
  loop 0 0

-- | Calculate total content right extent across all non-floating children in the arena.
getContentWidth :: NodeArena -> IO Float
getContentWidth na = do
  !n <- arenaCount na
  let isFloating !idx = do
        nt <- getNodeType na idx
        if isFloatingNode nt
          then pure True
          else do
            p <- getParent na idx
            if p < 0 then pure False else isFloating p
      loop !i !acc
        | i >= n = pure acc
        | otherwise = do
            p <- getParent na i
            floating <- isFloating i
            if p >= 0 && not floating
              then do
                (rx, _, rw, _) <- getRect na i
                loop (i + 1) (max acc (rx + rw))
              else loop (i + 1) acc
  loop 0 0
