{-# LANGUAGE BangPatterns #-}

module NanoUI.Rgfw.Layout
  ( solveSinglePassLayout
  , solveSinglePassLayoutWith
  , computePopupPosition
  , getContentHeight
  , getContentWidth
  ) where

import Control.Exception (finally)
import Control.Monad (forM, forM_, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , newPrimArray
  , readPrimArray
  , setPrimArray
  , writePrimArray
  )
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (RealWorld)
import GHC.IO (unsafePerformIO)
import NanoUI
  ( Padding (..)
  , PopupAnchor (..)
  , PopupPlacement (..)
  , Rect (..)
  , V2 (..)
  , WidgetId (..)
  )
import NanoUI.Testing (computePopupPosition)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeArena
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getClipRect
  , getDirection
  , getGap
  , getGridCols
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

-- | Preallocated unboxed scratch buffers for single-pass layout (zero allocation per frame).
data LayoutScratch = LayoutScratch
  { lsCap          :: {-# UNPACK #-} !Int
  , lsHeadChild    :: !(MutablePrimArray RealWorld Int)
  , lsNextSib      :: !(MutablePrimArray RealWorld Int)
  , lsIsOverlay    :: !(MutablePrimArray RealWorld Int)
  , lsIsPopup      :: !(MutablePrimArray RealWorld Int)
  , lsReqW         :: !(MutablePrimArray RealWorld Float)
  , lsReqH         :: !(MutablePrimArray RealWorld Float)
  , lsRemAfterW    :: !(MutablePrimArray RealWorld Float)
  , lsRemAfterH    :: !(MutablePrimArray RealWorld Float)
  , lsGridCellX    :: !(MutablePrimArray RealWorld Float)
  , lsGridCellY    :: !(MutablePrimArray RealWorld Float)
  , lsGridCellW    :: !(MutablePrimArray RealWorld Float)
  , lsGridCellH    :: !(MutablePrimArray RealWorld Float)
  , lsCurX         :: !(MutablePrimArray RealWorld Float)
  , lsCurY         :: !(MutablePrimArray RealWorld Float)
  , lsInnerX       :: !(MutablePrimArray RealWorld Float)
  , lsInnerY       :: !(MutablePrimArray RealWorld Float)
  , lsInnerW       :: !(MutablePrimArray RealWorld Float)
  , lsInnerH       :: !(MutablePrimArray RealWorld Float)
  }

{-# NOINLINE globalScratchPool #-}
globalScratchPool :: IORef (Maybe LayoutScratch)
globalScratchPool = unsafePerformIO (newIORef Nothing)

allocLayoutScratch :: Int -> IO LayoutScratch
allocLayoutScratch !cap = do
  hChild <- newPrimArray cap
  nSib   <- newPrimArray cap
  isOver <- newPrimArray cap
  isPop  <- newPrimArray cap
  rW     <- newPrimArray cap
  rH     <- newPrimArray cap
  remW   <- newPrimArray cap
  remH   <- newPrimArray cap
  gcX    <- newPrimArray cap
  gcY    <- newPrimArray cap
  gcW    <- newPrimArray cap
  gcH    <- newPrimArray cap
  cX     <- newPrimArray cap
  cY     <- newPrimArray cap
  iX     <- newPrimArray cap
  iY     <- newPrimArray cap
  iW     <- newPrimArray cap
  iH     <- newPrimArray cap
  pure $ LayoutScratch cap hChild nSib isOver isPop rW rH remW remH gcX gcY gcW gcH cX cY iX iY iW iH

withLayoutScratch :: Int -> (LayoutScratch -> IO a) -> IO a
withLayoutScratch !reqN act = do
  mSc <- atomicModifyIORef' globalScratchPool (\m -> (Nothing, m))
  sc <- case mSc of
    Just s | lsCap s >= reqN -> pure s
    _ -> do
      let !cap = max 256 (max reqN (maybe 0 ((* 2) . lsCap) mSc))
      allocLayoutScratch cap
  act sc `finally` atomicModifyIORef' globalScratchPool (\_ -> (Just sc, ()))

-- | Zero-allocation text line and length measurement
measureTextLines :: Text -> (Int, Float)
measureTextLines !txt
  | T.null txt = (1, 0)
  | otherwise =
      let go !n !curLen !maxLen t = case T.uncons t of
            Nothing -> (n, fromIntegral (max maxLen curLen))
            Just ('\n', rest) -> go (n + 1) 0 (max maxLen curLen) rest
            Just ('\r', rest) -> go n curLen maxLen rest
            Just (_, rest) -> go n (curLen + 1) maxLen rest
       in go 1 (0 :: Int) (0 :: Int) txt

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
  when (n > 0) $ withLayoutScratch n $ \scratch -> do
    let headChildArr = lsHeadChild scratch
        nextSibArr   = lsNextSib scratch
        isOverlayArr = lsIsOverlay scratch
        isPopupArr   = lsIsPopup scratch
        reqWArr      = lsReqW scratch
        reqHArr      = lsReqH scratch
        remAfterWArr = lsRemAfterW scratch
        remAfterHArr = lsRemAfterH scratch
        gridCellXArr = lsGridCellX scratch
        gridCellYArr = lsGridCellY scratch
        gridCellWArr = lsGridCellW scratch
        gridCellHArr = lsGridCellH scratch
        curXArr      = lsCurX scratch
        curYArr      = lsCurY scratch
        innerXArr    = lsInnerX scratch
        innerYArr    = lsInnerY scratch
        innerWArr    = lsInnerW scratch
        innerHArr    = lsInnerH scratch

    setPrimArray headChildArr 0 n (-1)
    setPrimArray nextSibArr 0 n (-1)
    setPrimArray isOverlayArr 0 n (0 :: Int)
    setPrimArray isPopupArr 0 n (0 :: Int)
    setPrimArray remAfterWArr 0 n 0
    setPrimArray remAfterHArr 0 n 0
    setPrimArray gridCellXArr 0 n 0
    setPrimArray gridCellYArr 0 n 0
    setPrimArray gridCellWArr 0 n 0
    setPrimArray gridCellHArr 0 n 0

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
      let pass1 !i
            | i < 0 = pure ()
            | otherwise = do
                nt <- getNodeType na i
                (wTag, wVal) <- getWidthSizing na i
                (hTag, hVal) <- getHeightSizing na i
                rawPad <- getPadding na i
                rawGap <- getGap na i
                dir <- getDirection na i
                gCols <- getGridCols na i
                (minW, minH, maxW, maxH) <- getMinMax na i
                isPop <- readPrimArray isPopupArr i

                -- For context menus / popups: compact 2px padding and zero gap between items
                let !pad = if nt == NodePopup then Padding 2 2 2 2 else rawPad
                    !gap = if isPop == 1 then 0 else rawGap

                if isContainerNode nt
                  then do
                    firstChild <- readPrimArray headChildArr i
                    (!contentW, !contentH) <-
                      if gCols > 0
                        then do
                          let getKids !c !acc
                                | c < 0 = pure (reverse acc)
                                | otherwise = do
                                    nxt <- readPrimArray nextSibArr c
                                    getKids nxt (c : acc)
                          kids <- getKids firstChild []
                          let !numKids = length kids
                              !numRows = if numKids == 0 then 0 else ((numKids - 1) `div` gCols) + 1

                          colWidths <- forM [0 .. gCols - 1] $ \col -> do
                            let colKids = [ kid | (kid, k) <- zip kids [0 :: Int ..], k `mod` gCols == col ]
                            ws <- forM colKids $ \kid -> do
                              cw <- readPrimArray reqWArr kid
                              (cWTag, cWVal) <- getWidthSizing na kid
                              pure $ if cWTag == SizingFixed && cWVal > 0
                                then cWVal
                                else if cWTag == SizingGrow
                                  then max 0 cw
                                  else cw
                            pure (maximum (0 : ws))

                          rowHeights <- forM [0 .. numRows - 1] $ \row -> do
                            let rowKids = [ kid | (kid, k) <- zip kids [0 :: Int ..], k `div` gCols == row ]
                            hs <- forM rowKids $ \kid -> do
                              ch <- readPrimArray reqHArr kid
                              (cHTag, cHVal) <- getHeightSizing na kid
                              pure $ if cHTag == SizingFixed && cHVal > 0
                                then cHVal
                                else if cHTag == SizingGrow
                                  then max 0 ch
                                  else ch
                            pure (maximum (0 : hs))

                          let !totGapW = fromIntegral (max 0 (gCols - 1)) * gap
                              !totGapH = fromIntegral (max 0 (numRows - 1)) * gap
                              !cw = sum colWidths + totGapW + padL pad + padR pad
                              !ch = sum rowHeights + totGapH + padT pad + padB pad
                          pure (cw, ch)
                        else do
                          -- Container content size from direct children
                          let loopChildren !c (!count :: Int) !totMain !maxCross !totAdd
                                | c < 0 = pure (count, totMain, maxCross, totAdd)
                                | otherwise = do
                                    cw <- readPrimArray reqWArr c
                                    ch <- readPrimArray reqHArr c
                                    (cWTag, _) <- getWidthSizing na c
                                    (cHTag, _) <- getHeightSizing na c
                                    next <- readPrimArray nextSibArr c
                                    if dir == DirColumn
                                      then do
                                        let !mSize = if cHTag == SizingGrow then max 0 ch else ch
                                            !add = mSize + gap
                                            !tot = totMain + mSize + (if count > 0 then gap else 0)
                                            !cross = max maxCross cw
                                        loopChildren next (count + 1) tot cross (totAdd + add)
                                      else do
                                        let !mSize = if cWTag == SizingGrow then max 0 cw else cw
                                            !add = mSize + gap
                                            !tot = totMain + mSize + (if count > 0 then gap else 0)
                                            !cross = max maxCross ch
                                        loopChildren next (count + 1) tot cross (totAdd + add)

                          (!cCount, !cTot, !cCross, !cTotAdd) <- loopChildren firstChild 0 0 0 0

                          -- Precalculate remAfter for children in a single O(K) pass
                          let fillRemAfter !c !remAcc
                                | c < 0 = pure ()
                                | otherwise = do
                                    (sWTag, _) <- getWidthSizing na c
                                    (sHTag, _) <- getHeightSizing na c
                                    sw <- readPrimArray reqWArr c
                                    sh <- readPrimArray reqHArr c
                                    next <- readPrimArray nextSibArr c
                                    if dir == DirRow
                                      then do
                                        let !add = if sWTag == SizingGrow then max 0 sw + gap else sw + gap
                                            !remAfter = max 0 (remAcc - add)
                                        writePrimArray remAfterWArr c remAfter
                                        fillRemAfter next remAfter
                                      else do
                                        let !add = if sHTag == SizingGrow then max 0 sh + gap else sh + gap
                                            !remAfter = max 0 (remAcc - add)
                                        writePrimArray remAfterHArr c remAfter
                                        fillRemAfter next remAfter

                          fillRemAfter firstChild cTotAdd

                          pure $ if dir == DirColumn
                            then (cCross + padL pad + padR pad, (if cCount > 0 then cTot else 0) + padT pad + padB pad)
                            else ((if cCount > 0 then cTot else 0) + padL pad + padR pad, cCross + padT pad + padB pad)

                    p <- getParent na i
                    pnt <- if p >= 0 && p < n then getNodeType na p else pure NodePanel
                    pFirstChild <- if p >= 0 && p < n then readPrimArray headChildArr p else pure (-1)
                    let !isWinTitleBar = (pnt == NodeWindow || pnt == NodeModal) && i == pFirstChild && nt == NodeContainer && dir == DirRow

                    if nt == NodeWindow
                      then do
                        wid <- getWidgetId na i
                        mStoredSz <- lookupWindowSize wid
                        let (!rw, !rh) = case mStoredSz of
                              Just (sw, sh) | sw > 0 && sh > 0 -> (sw, sh)
                              _ -> (max 320 contentW, max 200 contentH)
                        writePrimArray reqWArr i (max minW (min maxW rw))
                        writePrimArray reqHArr i (max minH (min maxH rh))
                      else if isWinTitleBar
                        then do
                          writePrimArray reqWArr i (max minW (min maxW contentW))
                          writePrimArray reqHArr i 24.0
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
                    txt <- getText na i
                    let !dispTxt =
                          if T.isPrefixOf "\x01" txt || T.isPrefixOf "\x02" txt || T.isPrefixOf "\x03" txt
                            then T.drop 1 txt
                            else txt
                        !(!lineCount, !maxLineLen) = measureTextLines dispTxt
                        !txtLen = maxLineLen
                        !isClose = T.isPrefixOf "\x01" txt || txt == "[X]" || txt == "X" || txt == "\xd7" || txt == "\xf00d"
                    p <- getParent na i
                    pp <- if p >= 0 && p < n then getParent na p else pure (-1)
                    ppnt <- if pp >= 0 && pp < n then getNodeType na pp else pure NodePanel
                    ppFirstChild <- if pp >= 0 && pp < n then readPrimArray headChildArr pp else pure (-1)
                    let !isWinTitleChild = (ppnt == NodeWindow || ppnt == NodeModal) && p == ppFirstChild
                    let !leafW =
                          if isWinTitleChild && isClose
                            then 24
                            else if wTag == SizingFixed && wVal > 0
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
                          if isWinTitleChild
                            then 24
                            else if hTag == SizingFixed && hVal > 0
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
      let setupGridChildren !parentIdx !gCols !pix !piy !piw !pih !pGap = do
            let getKids !c !acc
                  | c < 0 = pure (reverse acc)
                  | otherwise = do
                      nxt <- readPrimArray nextSibArr c
                      getKids nxt (c : acc)
            firstChild <- readPrimArray headChildArr parentIdx
            kids <- getKids firstChild []
            let !numKids = length kids
                !numRows = if numKids == 0 then 0 else ((numKids - 1) `div` gCols) + 1

            when (numKids > 0 && gCols > 0) $ do
              let !totGapW = fromIntegral (max 0 (gCols - 1)) * pGap
                  !netW = max 0 (piw - totGapW)
                  !totGapH = fromIntegral (max 0 (numRows - 1)) * pGap
                  !netH = max 0 (pih - totGapH)

              -- Column sizing: for each col 0..gCols-1, examine children in that col
              colInfos <- forM [0 .. gCols - 1] $ \col -> do
                let colKids = [ kid | (kid, k) <- zip kids [0 :: Int ..], k `mod` gCols == col ]
                sizes <- forM colKids $ \kid -> do
                  (cWTag, cWVal) <- getWidthSizing na kid
                  cw <- readPrimArray reqWArr kid
                  pure (cWTag == SizingGrow, if cWTag == SizingFixed && cWVal > 0 then cWVal else 0, cw)
                let !hasGrow = any (\(g, _, _) -> g) sizes
                    !fixedW = maximum (0 : [ fw | (_, fw, _) <- sizes ])
                    !reqW = maximum (0 : [ rw | (_, _, rw) <- sizes ])
                pure (hasGrow, fixedW, reqW)

              let anyColGrow = any (\(g, _, _) -> g) colInfos
                  fixedCols = [ (col, fw) | (col, (_, fw, _)) <- zip [0 :: Int ..] colInfos, fw > 0 ]
                  numFixedCols = length fixedCols

              colWidths <- if anyColGrow
                then do
                  let nonGrowCols = [ (col, if fw > 0 then fw else rw) | (col, (g, fw, rw)) <- zip [0 :: Int ..] colInfos, not g ]
                      growCols = [ col | (col, (g, _, _)) <- zip [0 :: Int ..] colInfos, g ]
                      usedW = sum (map snd nonGrowCols)
                      remW = max 0 (netW - usedW)
                      growShare = if null growCols then 0 else remW / fromIntegral (length growCols)
                  pure $ flip map (zip [0 :: Int ..] colInfos) $ \(_, (g, fw, rw)) ->
                    if g then max rw growShare else (if fw > 0 then fw else rw)
                else if numFixedCols > 0 && numFixedCols < gCols
                  then do
                    let fixedWSum = sum (map snd fixedCols)
                        remW = max 0 (netW - fixedWSum)
                        numOther = gCols - numFixedCols
                        otherShare = remW / fromIntegral numOther
                    pure $ flip map (zip [0 :: Int ..] colInfos) $ \(_, (_, fw, rw)) ->
                      if fw > 0 then fw else max rw otherShare
                else do
                  let totalReqW = sum [ rw | (_, _, rw) <- colInfos ]
                  if netW > totalReqW && totalReqW > 0
                    then do
                      let extraW = netW - totalReqW
                          share = extraW / fromIntegral gCols
                      pure $ map (\(_, _, rw) -> rw + share) colInfos
                    else if totalReqW > 0
                      then pure $ map (\(_, _, rw) -> rw) colInfos
                      else do
                        let uniformW = netW / fromIntegral gCols
                        pure $ replicate gCols uniformW

              let colXOffsets = scanl (\acc w -> acc + w + pGap) pix colWidths

              -- Row sizing: for each row 0..numRows-1, examine children in that row
              rowInfos <- forM [0 .. numRows - 1] $ \row -> do
                let rowKids = [ kid | (kid, k) <- zip kids [0 :: Int ..], k `div` gCols == row ]
                sizes <- forM rowKids $ \kid -> do
                  (cHTag, cHVal) <- getHeightSizing na kid
                  ch <- readPrimArray reqHArr kid
                  pure (cHTag == SizingGrow, if cHTag == SizingFixed && cHVal > 0 then cHVal else 0, ch)
                let !hasGrowH = any (\(g, _, _) -> g) sizes
                    !fixedH = maximum (0 : [ fh | (_, fh, _) <- sizes ])
                    !reqH = maximum (0 : [ rh | (_, _, rh) <- sizes ])
                pure (hasGrowH, fixedH, reqH)

              let anyRowGrow = any (\(g, _, _) -> g) rowInfos
              rowHeights <- if anyRowGrow
                then do
                  let nonGrowRows = [ (row, if fh > 0 then fh else rh) | (row, (g, fh, rh)) <- zip [0 :: Int ..] rowInfos, not g ]
                      growRows = [ row | (row, (g, _, _)) <- zip [0 :: Int ..] rowInfos, g ]
                      usedH = sum (map snd nonGrowRows)
                      remH = max 0 (netH - usedH)
                      growShare = if null growRows then 0 else remH / fromIntegral (length growRows)
                  pure $ flip map (zip [0 :: Int ..] rowInfos) $ \(_, (g, fh, rh)) ->
                    if g then max rh growShare else (if fh > 0 then fh else rh)
                else do
                  pure $ map (\(_, fh, rh) -> if fh > 0 then fh else rh) rowInfos

              let rowYOffsets = scanl (\acc h -> acc + h + pGap) piy rowHeights

              forM_ (zip kids [0 :: Int ..]) $ \(kid, k) -> do
                let !cIdx = k `mod` gCols
                    !rIdx = k `div` gCols
                    !cellX = colXOffsets !! cIdx
                    !cellY = rowYOffsets !! rIdx
                    !cellW = colWidths !! cIdx
                    !cellH = rowHeights !! rIdx
                writePrimArray gridCellXArr kid cellX
                writePrimArray gridCellYArr kid cellY
                writePrimArray gridCellWArr kid cellW
                writePrimArray gridCellHArr kid cellH

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

                    firstChild <- readPrimArray headChildArr i
                    hasTitleRow <- if firstChild >= 0
                      then do
                        fcNt <- getNodeType na firstChild
                        fcDir <- getDirection na firstChild
                        pure (fcNt == NodeContainer && fcDir == DirRow)
                      else pure False

                    let (!ix, !iy, !iw, !ih) =
                          if hasTitleRow
                            then (winX, winY, winW, winH)
                            else (winX + padL pad, winY + padT pad, max 0 (winW - padL pad - padR pad), max 0 (winH - padT pad - padB pad))
                    writePrimArray innerXArr i ix
                    writePrimArray innerYArr i iy
                    writePrimArray innerWArr i iw
                    writePrimArray innerHArr i ih
                    writePrimArray curXArr i ix
                    writePrimArray curYArr i iy
                    winGCols <- getGridCols na i
                    when (winGCols > 0) $ do
                      rawGap <- getGap na i
                      setupGridChildren i winGCols ix iy iw ih rawGap

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
                    rootGCols <- getGridCols na i
                    when (rootGCols > 0) $ do
                      rawGap <- getGap na i
                      setupGridChildren i rootGCols ix iy iw ih rawGap

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
                      popGCols <- getGridCols na i
                      when (popGCols > 0) $
                        setupGridChildren i popGCols ix iy iw ih 0

                  else do
                    -- Child node
                    pGCols <- getGridCols na p
                    if pGCols > 0
                      then do
                        cellX <- readPrimArray gridCellXArr i
                        cellY <- readPrimArray gridCellYArr i
                        cellW <- readPrimArray gridCellWArr i
                        cellH <- readPrimArray gridCellHArr i

                        mParentClip <- getClipRect na p
                        let !parentClip = case mParentClip of
                              Just r -> r
                              Nothing -> Rect 0 0 viewportW viewportH

                        let !w = if wTag == SizingFixed && wVal > 0 then min cellW wVal else cellW
                            !h = if hTag == SizingFixed && hVal > 0 then min cellH hVal else cellH
                            !finalW = max minW (min maxW w)
                            !finalH = max minH (min maxH h)
                            !x = cellX
                            !y = cellY

                        setRect na i x y finalW finalH

                        let !cx0 = max (rectX parentClip) x
                            !cy0 = max (rectY parentClip) y
                            !cx1 = min (rectX parentClip + rectW parentClip) (x + finalW)
                            !cy1 = min (rectY parentClip + rectH parentClip) (y + finalH)
                            !childClip = Rect cx0 cy0 (max 0 (cx1 - cx0)) (max 0 (cy1 - cy0))
                        setClipRect na i childClip

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
                          cgCols <- getGridCols na i
                          when (cgCols > 0) $ do
                            rawGap <- getGap na i
                            setupGridChildren i cgCols cix ciy ciw cih rawGap

                      else do
                        pDir <- getDirection na p
                        rawPGap <- getGap na p
                        pIsPop <- readPrimArray isPopupArr p
                        let !pGap = if pIsPop == 1 then 0 else rawPGap
                        pnt <- getNodeType na p
                        pFirstChild <- readPrimArray headChildArr p
                        pp <- if p >= 0 then getParent na p else pure (-1)
                        ppnt <- if pp >= 0 then getNodeType na pp else pure NodePanel
                        ppFirstChild <- if pp >= 0 then readPrimArray headChildArr pp else pure (-1)
                        let !isWinTitleBar = (pnt == NodeWindow || pnt == NodeModal) && i == pFirstChild && nt == NodeContainer && pDir == DirColumn
                            !isWinTitleChild = (ppnt == NodeWindow || ppnt == NodeModal) && p == ppFirstChild

                        if isWinTitleBar
                          then do
                            (winX, winY, winW, winH) <- getRect na p
                            winPad <- getPadding na p
                            setRect na i winX winY winW 24.0
                            setClipRect na i (Rect winX winY winW 24.0)
                            let !cix = winX + 8.0
                                !ciy = winY
                                !ciw = max 0 (winW - 8.0)
                                !cih = 24.0
                            writePrimArray innerXArr i cix
                            writePrimArray innerYArr i ciy
                            writePrimArray innerWArr i ciw
                            writePrimArray innerHArr i cih
                            writePrimArray curXArr i cix
                            writePrimArray curYArr i ciy
                            writePrimArray curYArr p (winY + 24.0)
                            writePrimArray innerYArr p (winY + 24.0)
                            writePrimArray innerXArr p (winX + padL winPad)
                            writePrimArray innerWArr p (max 0 (winW - padL winPad - padR winPad))
                            writePrimArray innerHArr p (max 0 (winH - 24.0 - padB winPad))
                          else if isWinTitleChild
                            then do
                              cx <- readPrimArray curXArr p
                              cy <- readPrimArray curYArr p
                              pix <- readPrimArray innerXArr p
                              piw <- readPrimArray innerWArr p
                              remAfterW <- readPrimArray remAfterWArr i
                              case nt of
                                NodeText -> do
                                  setRect na i cx cy reqW 24.0
                                  setClipRect na i (Rect cx cy reqW 24.0)
                                  writePrimArray curXArr p (cx + reqW)
                                NodeSpacer -> do
                                  let !availW = max 0 (pix + piw - cx)
                                      !w = max 0 (availW - remAfterW)
                                  setRect na i cx cy w 24.0
                                  setClipRect na i (Rect cx cy w 24.0)
                                  writePrimArray curXArr p (cx + w)
                                NodeButton -> do
                                  let !btnX = max cx (pix + piw - 24.0)
                                  setRect na i btnX cy 24.0 24.0
                                  setClipRect na i (Rect btnX cy 24.0 24.0)
                                  writePrimArray curXArr p (btnX + 24.0)
                                _ -> do
                                  setRect na i cx cy reqW 24.0
                                  setClipRect na i (Rect cx cy reqW 24.0)
                                  writePrimArray curXArr p (cx + reqW)
                          else do
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
                                        then max 0 (availW - remAfterW)
                                        else reqW
                                      else if wTag == SizingGrow
                                        then piw
                                        else reqW

                            -- Compute Height
                            let !availH = max 0 (piy + pih - cy)
                                !h =
                                  if hTag == SizingFixed && hVal > 0
                                    then hVal
                                    else if pDir == DirColumn
                                      then if hTag == SizingGrow
                                        then max 0 (availH - remAfterH)
                                        else reqH
                                      else if hTag == SizingGrow
                                        then pih
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
                              cgCols <- getGridCols na i
                              when (cgCols > 0) $ do
                                rawGap <- getGap na i
                                setupGridChildren i cgCols cix ciy ciw cih rawGap

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
