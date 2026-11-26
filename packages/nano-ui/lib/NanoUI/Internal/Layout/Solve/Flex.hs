-- | Scratch-buffer flex, grid, wrap-line, chrome-column, and alignment math.
module NanoUI.Internal.Layout.Solve.Flex
  ( FlowAcc (..)
  , containerFlow
  , isChromeColumn
  , pairColumnGap
  , columnGapSumScratch
  , foldChromeColumnScratch
  , gridColumnCount
  , gridColWidth
  , gridRowHeight
  , wrapLines
  , distributeScratch
  , resolveSize
  , alignX
  , alignY
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Primitive.PrimArray (copyMutablePrimArray, newPrimArray, readPrimArray, writePrimArray)
import NanoUI.Internal.Layout.ScrollGeometry (isScrollStyle2D)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Padding (..), alignXFraction, alignYFraction)
import NanoUI.Internal.Types (clamp, foldUpTo, forUpTo_, isBounded)

-- | Strict accumulator for flow-child folds: a child count and two running
-- sums or extents. The strict fields keep the folds unboxed.
data FlowAcc = FlowAcc !Int !Float !Float

-- | Greedily break items into lines no longer than @limit@, with @gap@
-- between neighbours. An item longer than the limit gets a line to itself.
wrapLines :: Float -> Float -> (a -> Float) -> [a] -> [[a]]
wrapLines limit gap len = go
  where
    go [] = []
    go (x : xs) = let (line, rest) = fill (len x) xs in (x : line) : go rest
    fill !used (y : ys)
      | used + gap + len y <= limit + 1.0e-3 =
          let (line, rest) = fill (used + gap + len y) ys in (y : line, rest)
    fill _ ys = ([], ys)

isChromeColumn :: NodeType -> DirTag -> Bool
isChromeColumn nt dir =
  dir == DirColumn && (nt == NodeWindow || nt == NodeModal)

-- | Gap before child @b@ in a column; chrome columns drop it before separators.
pairColumnGap :: NodeArena -> Bool -> NodeIdx -> Float -> IO Float
pairColumnGap _ False _ gap = pure gap
pairColumnGap na True b gap = do
  ntB <- getNodeType na b
  pure (if ntB == NodeSeparator then 0 else gap)

foldChromeColumnScratch :: NodeArena -> Int -> Float -> IO (Float, Float)
foldChromeColumnScratch na n gap = do
  FlexScratch {fsW = wArr, fsH = hArr} <- readIORef (naScratch na)
  gapSum <- columnGapSumScratch na n gap
  maxW <- foldUpTo n (\m i -> max m <$> readPrimArray wArr i) 0
  totalH <- foldUpTo n (\t i -> (t +) <$> readPrimArray hArr i) 0
  pure (maxW, totalH + gapSum)

-- | Grid column count: explicit, else as many @minColW@ columns as fit in a
-- positive @availW@, else one.
{-# INLINE gridColumnCount #-}
gridColumnCount :: Int -> Float -> Float -> Float -> Int
gridColumnCount gCols minColW availW gap
  | gCols > 0 = gCols
  | minColW > 0 && availW > 0 = max 1 (floor ((availW + gap) / (minColW + gap)))
  | otherwise = 1

-- | Height of grid row @r@: its tallest child.
-- | Width of each column in a grid of @cols@ columns across @avail@ with @gap@.
{-# INLINE gridColWidth #-}
gridColWidth :: Float -> Int -> Float -> Float
gridColWidth avail cols gap = max 0 ((avail - gap * fromIntegral (cols - 1)) / fromIntegral cols)

gridRowHeight :: IOArr Float -> Int -> Int -> Int -> IO Float
gridRowHeight hArr n cols r =
  foldUpTo (min cols (n - r * cols)) (\m j -> max m <$> readPrimArray hArr (r * cols + j)) 0

{-# INLINE containerFlow #-}
containerFlow :: NodeArenaArrays -> NodeIdx -> IO (Padding, Float, DirTag)
containerFlow a idx =
  (,,) <$> readPadding a idx <*> readStyle a idx StyleGap <*> readTagEnum a idx TagDirection

-- | The gaps between the first @n@ scratch children of a window's or modal's
-- column, where a separator takes none before it.
columnGapSumScratch :: NodeArena -> Int -> Float -> IO Float
columnGapSumScratch na n gap = do
  FlexScratch {fsIdx = idxArr} <- readIORef (naScratch na)
  let addGap acc i = readPrimArray idxArr (i + 1) >>= \b -> (acc +) <$> pairColumnGap na True b gap
  foldUpTo (n - 1) addGap 0

-- | Share the main axis among the first @n@ scratch children: 'fsOut' gets
-- each child's size along it, starting from its content size ('fsW' or
-- 'fsH') and taking its part of the space the container has spare or lacks.
distributeScratch :: NodeArena -> Int -> Float -> Float -> Bool -> IO ()
distributeScratch na n avail gapSum horizontal = do
  FlexScratch {fsIdx = idxArr, fsW = wArr, fsH = hArr, fsOut = out, fsGrow = gfArr} <- readIORef (naScratch na)
  a <- arenaArrays na
  copyMutablePrimArray out 0 (if horizontal then wArr else hArr) 0 n
  total <- foldUpTo n (\acc i -> (acc +) <$> readPrimArray out i) 0
  let slack = avail - (total + gapSum)
      sizingAt i = readPrimArray idxArr i >>= \ci -> readAxisSizing a ci horizontal
  if slack > 0.001
    then do
      -- Grow children share the free space by factor, but no child is
      -- squeezed below its content size (a min-content floor, like CSS
      -- flex with min-width:auto): two fillW columns come out equal unless
      -- one column's content needs more, and that one then takes exactly
      -- what it needs while the rest re-share what is left.
      --
      -- 'fsGrow' holds each child's factor, 0 once it is not or no longer
      -- growing (-1 once its maximum stops it, 'settleCapped'), and 'fsOut'
      -- keeps the exact content size until the shares are handed out.
      FlowAcc capped growTotal _ <- foldUpTo n (\(FlowAcc c acc _) i -> do
        AxisSizing tag val _ hi <- sizingAt i
        let gf = if tag == SizingGrow && val > 0 then val else 0
        writePrimArray gfArr i gf
        pure (FlowAcc (if gf > 0 && isBounded hi then c + 1 else c) (acc + gf) 0)) (FlowAcc 0 0 0)
      when (growTotal > 0) $ do
        (free, gfSum) <-
          if capped > 0
            then settleCapped a idxArr out gfArr horizontal avail gapSum n
            else settleGrow out gfArr avail gapSum n 0
        forUpTo_ n $ \i -> do
          gf <- readPrimArray gfArr i
          when (gf > 0) $ writePrimArray out i (max 0 (free * gf / gfSum))
    else when (slack < -0.001) $ do
      -- Shrink children by factor to make up the shortfall, none below its
      -- minimum ('shrinkScratch'). 'fsGrow' holds the factors. A scroll
      -- container that fits its content along the axis it scrolls gives
      -- space back like a grow child, and scrolls the rest: a scroll area in
      -- a popup capped by 'maxH' keeps within the cap.
      forUpTo_ n $ \i -> do
        ci <- readPrimArray idxArr i
        ax <- readAxisSizing a ci horizontal
        scrolls <-
          if axTag ax /= SizingFit
            then pure False
            else do
              nt <- readTagEnum a ci TagNodeType
              si <- readStyleIdx a ci
              dir <- readTagEnum a ci TagDirection
              pure (nt == NodeScrollContainer && (isScrollStyle2D si || (dir == DirRow) == horizontal))
        writePrimArray gfArr i (if scrolls then 1 else shrinkFactor ax)
      let minCol = if horizontal then StyleMinW else StyleMinH
      shrinkScratch out gfArr (\i -> readPrimArray idxArr i >>= \ci -> readStyle a ci minCol) n (negate slack)

-- | Take @need@ from the first @n@ sizes in @mainArr@ in proportion to their
-- factors in @factorArr@ (0 keeps a size). A child that would drop below its
-- minimum (@minAt@) stops there with its factor cleared, and the others
-- share what it could not give. Inlined, so @minAt@ is a known function and
-- the passes run unboxed.
{-# INLINE shrinkScratch #-}
shrinkScratch :: IOArr Float -> IOArr Float -> (Int -> IO Float) -> Int -> Float -> IO ()
shrinkScratch mainArr factorArr minAt n = pass 0
  where
    pass !sweep !need = when (need > 0) $ do
      total <- foldUpTo n (\acc i -> (acc +) <$> readPrimArray factorArr i) 0
      when (total > 0) $ do
        let stop !i !given !stopped
              | i >= n =
                  if stopped > (0 :: Int)
                    then do
                      need' <-
                        if sweep + 1 == waterFillAfter
                          then waterFillShrink mainArr factorArr minAt (need - given) n
                          else pure (need - given)
                      pass (sweep + 1) need'
                    else forUpTo_ n $ \j -> do
                      f <- readPrimArray factorArr j
                      when (f > 0) $ readPrimArray mainArr j >>= \main -> writePrimArray mainArr j (main - need * f / total)
              | otherwise = do
                  f <- readPrimArray factorArr i
                  if f <= 0
                    then stop (i + 1) given stopped
                    else do
                      lo <- minAt i
                      main <- readPrimArray mainArr i
                      if main - need * f / total < lo
                        then do
                          writePrimArray mainArr i lo
                          writePrimArray factorArr i 0
                          stop (i + 1) (given + main - lo) (stopped + 1)
                        else stop (i + 1) given stopped
        stop 0 0 0

-- | Sort @(keys, vals)@ of length @len@ in place by @keys@, ascending when
-- @ascending@ is 'True' and descending otherwise. Uses insertion sort for small
-- slices and heapsort above, allocating nothing.
sortPairsByKey :: Bool -> IOArr Float -> IOArr Int -> Int -> IO ()
sortPairsByKey ascending keys vals len
  | len <= 1 = pure ()
  | len <= 16 = insertLoop 1
  | otherwise = buildHeap ((len `quot` 2) - 1) >> extractLoop (len - 1)
  where
    {-# INLINE precedes #-}
    precedes :: Float -> Float -> Bool
    precedes a b = if ascending then a < b else a > b

    insertLoop !i = when (i < len) $ do
      k <- readPrimArray keys i
      v <- readPrimArray vals i
      let shift !j
            | j <= 0 = writePrimArray keys 0 k >> writePrimArray vals 0 v
            | otherwise = do
                kj <- readPrimArray keys (j - 1)
                if precedes k kj
                  then do
                    writePrimArray keys j kj
                    readPrimArray vals (j - 1) >>= writePrimArray vals j
                    shift (j - 1)
                  else writePrimArray keys j k >> writePrimArray vals j v
      shift i
      insertLoop (i + 1)

    -- Max-heap under 'precedes' puts the last-in-order element at the root so
    -- extracting to the tail leaves the array sorted.
    siftDown !root !end = do
      rk <- readPrimArray keys root
      rv <- readPrimArray vals root
      let go !cur = do
            let !l = cur * 2 + 1
            if l >= end
              then writePrimArray keys cur rk >> writePrimArray vals cur rv
              else do
                lk <- readPrimArray keys l
                let !r = l + 1
                (bestIdx, bestK) <-
                  if r < end
                    then do
                      rk' <- readPrimArray keys r
                      pure (if precedes lk rk' then (r, rk') else (l, lk))
                    else pure (l, lk)
                if precedes rk bestK
                  then do
                    writePrimArray keys cur bestK
                    readPrimArray vals bestIdx >>= writePrimArray vals cur
                    go bestIdx
                  else writePrimArray keys cur rk >> writePrimArray vals cur rv
      go root

    buildHeap !i = when (i >= 0) $ siftDown i len >> buildHeap (i - 1)

    extractLoop !end = when (end > 0) $ do
      k0 <- readPrimArray keys 0
      v0 <- readPrimArray vals 0
      ke <- readPrimArray keys end
      ve <- readPrimArray vals end
      writePrimArray keys 0 ke
      writePrimArray vals 0 ve
      writePrimArray keys end k0
      writePrimArray vals end v0
      siftDown 0 end
      extractLoop (end - 1)

-- | Lock every remaining shrinkable child whose headroom per unit of factor is
-- less than the shortfall per unit left once the tighter children before it
-- hit their floors. Runs in O(n log n) after 'waterFillAfter' sweeps using
-- unboxed scratch arrays.
waterFillShrink :: IOArr Float -> IOArr Float -> (Int -> IO Float) -> Float -> Int -> IO Float
waterFillShrink mainArr factorArr minAt need0 n
  | need0 <= 0 = pure need0
  | otherwise = do
      keys <- newPrimArray n
      idxs <- newPrimArray n
      slacks <- newPrimArray n
      floors <- newPrimArray n
      let collect !i !m !total
            | i >= n = pure (m, total)
            | otherwise = do
                f <- readPrimArray factorArr i
                if f <= 0
                  then collect (i + 1) m total
                  else do
                    lo <- minAt i
                    main <- readPrimArray mainArr i
                    let !slack = main - lo
                    writePrimArray keys m (slack / f)
                    writePrimArray idxs m i
                    writePrimArray slacks i slack
                    writePrimArray floors i lo
                    collect (i + 1) (m + 1) (total + f)
      (m, total0) <- collect 0 0 0
      sortPairsByKey True keys idxs m
      let stopFrom !j !remNeed !total
            | j >= m || remNeed <= 0 || total <= 0 = pure remNeed
            | otherwise = do
                i <- readPrimArray idxs j
                slack <- readPrimArray slacks i
                f <- readPrimArray factorArr i
                if slack * total < remNeed * f
                  then do
                    readPrimArray floors i >>= writePrimArray mainArr i
                    writePrimArray factorArr i 0
                    stopFrom (j + 1) (remNeed - slack) (total - f)
                  else pure remNeed
      stopFrom 0 need0 total0

{-# INLINE shrinkFactor #-}
shrinkFactor :: AxisSizing -> Float
shrinkFactor (AxisSizing tag val _ _) =
  case tag of
    SizingShrink -> max 0 val
    -- Grow also gives space back when the window is smaller than content.
    SizingGrow -> if val > 0 then val else 1
    -- Percent flexes like CSS: when siblings plus gaps overflow the axis,
    -- percent children give the overflow back so e.g. two 50% columns and a
    -- gap land exactly on the row width. Covers percent on either axis,
    -- should height percent ever be sized that way.
    SizingPercent -> 1
    -- Fit stays content-sized. A pinned header must not squash when a Grow
    -- sibling (page scroll) is taller than the window. 'distributeScratch'
    -- shrinks a fit scroll container along the axis it scrolls all the same.
    _ -> 0

-- One sweep: sum content of non-grow + already-locked children (factor 0) and
-- grow factors of the still-unlocked.
{-# INLINE scanGrow #-}
scanGrow :: IOArr Float -> IOArr Float -> Int -> Int -> Float -> Float -> IO (Float, Float)
scanGrow mainArr crossArr !i !end !occupied !gfSum
  | i >= end = pure (occupied, gfSum)
  | otherwise = do
      gf <- readPrimArray crossArr i
      if gf > 0
        then scanGrow mainArr crossArr (i + 1) end occupied (gfSum + gf)
        else do
          main <- readPrimArray mainArr i
          scanGrow mainArr crossArr (i + 1) end (occupied + main) gfSum

-- Pin every grow child whose content exceeds its would-be share by clearing
-- its factor; its content stays in mainArr.
lockGrow :: IOArr Float -> IOArr Float -> Float -> Float -> Int -> IO Int
lockGrow mainArr crossArr !free !gfSum n = foldUpTo n lock 0
  where
    lock acc i = do
      gf <- readPrimArray crossArr i
      need <- readPrimArray mainArr i
      if gf > 0 && need * gfSum > gf * free
        then (acc + 1) <$ writePrimArray crossArr i 0
        else pure acc

-- Each lock shrinks the share pool, possibly locking more children; the
-- locked set only grows, so this fixpoints within n sweeps, and a row of n
-- children could take n. Rows settle in a few; one still locking after
-- 'waterFillAfter' sweeps is settled by 'waterFillGrow', which the next sweep
-- confirms, so no row takes more than a sort and that many sweeps.
settleGrow :: IOArr Float -> IOArr Float -> Float -> Float -> Int -> Int -> IO (Float, Float)
settleGrow mainArr crossArr avail gapSum n !pass = do
  (occupied, gfSum) <- scanGrow mainArr crossArr 0 n 0 0
  let free = avail - gapSum - occupied
  locked <- lockGrow mainArr crossArr free gfSum n
  if locked == 0 || pass >= n
    then pure (free, gfSum)
    else do
      when (pass + 1 == waterFillAfter) $ waterFillGrow mainArr crossArr (avail - gapSum) n
      settleGrow mainArr crossArr avail gapSum n (pass + 1)

-- | 'settleGrow' for children of which some grow with a maximum along the
-- axis (@horizontal@ picks it), the scratch children in @idxArr@. A child
-- whose share passes its maximum stops there, -1 in @crossArr@, and the
-- rest share what it gave up; the children locked at their content size
-- grow again, since the shares rose. Each round stops one more child, so
-- there are at most as many rounds as children.
settleCapped ::
  NodeArenaArrays -> IOArr Int -> IOArr Float -> IOArr Float -> Bool -> Float -> Float -> Int -> IO (Float, Float)
settleCapped a idxArr mainArr crossArr horizontal avail gapSum n = go
  where
    sizingAt i = readPrimArray idxArr i >>= \ci -> readAxisSizing a ci horizontal
    go = do
      shares@(free, gfSum) <- settleGrow mainArr crossArr avail gapSum n 0
      let cap k i = do
            gf <- readPrimArray crossArr i
            if gf <= 0
              then pure k
              else do
                hi <- axMax <$> sizingAt i
                if free * gf / gfSum > hi + 1.0e-3
                  then (k + 1 :: Int) <$ (writePrimArray mainArr i hi >> writePrimArray crossArr i (-1))
                  else pure k
      stopped <- foldUpTo n cap 0
      if stopped == 0
        then pure shares
        else do
          forUpTo_ n $ \i -> do
            gf <- readPrimArray crossArr i
            when (gf == 0) $ do
              AxisSizing tag val _ _ <- sizingAt i
              when (tag == SizingGrow && val > 0) $ writePrimArray crossArr i val
          go

-- | Sweeps 'settleGrow' makes before it sorts. Rows with varied content and
-- weights settle in three or four, and below this the sweeps cost less than
-- the sort.
waterFillAfter :: Int
waterFillAfter = 8

-- Lock, in one go, every grow child that 'lockGrow' sweeps would lock one
-- after another. A child locks when its content per unit of grow factor is
-- more than the share per unit left once the children above it lock; the
-- share per unit only falls as children lock, so taking children by content
-- per unit, largest first, and stopping at the first that fits locks the
-- same set.
waterFillGrow :: IOArr Float -> IOArr Float -> Float -> Int -> IO ()
waterFillGrow mainArr crossArr room n = do
  keys <- newPrimArray n
  idxs <- newPrimArray n
  let collect !i !m !occupied !gfSum
        | i >= n = pure (m, occupied, gfSum)
        | otherwise = do
            gf <- readPrimArray crossArr i
            need <- readPrimArray mainArr i
            if gf > 0
              then do
                writePrimArray keys m (need / gf)
                writePrimArray idxs m i
                collect (i + 1) (m + 1) occupied (gfSum + gf)
              else collect (i + 1) m (occupied + need) gfSum
  (m, occupied, gfSum) <- collect 0 0 0 0
  sortPairsByKey False keys idxs m
  let lockFrom !j !free !g = when (j < m) $ do
        i <- readPrimArray idxs j
        need <- readPrimArray mainArr i
        gf <- readPrimArray crossArr i
        when (need * g > gf * free) $ do
          writePrimArray crossArr i 0
          lockFrom (j + 1) (free - need) (g - gf)
  lockFrom 0 (room - occupied) gfSum

-- | The size a node takes along an axis where @avail@ is offered, from its
-- sizing, its measured size @intrinsic@ and its limits.
{-# INLINE resolveSize #-}
resolveSize :: AxisSizing -> Float -> Float -> Float
resolveSize (AxisSizing tag val lo hi) intrinsic avail =
  clamp lo hi $ case tag of
    SizingFixed -> val
    SizingGrow -> avail
    SizingPercent -> avail
    _ -> min intrinsic avail

alignX :: AlignX -> Float -> Float -> Float -> Float
alignX ax cx cw iw = cx + (cw - iw) * alignXFraction ax

-- | 'AlignBaseline' places like 'AlignTop' here; 'positionRowFromParent'
-- places a row's baseline-aligned children.
alignY :: AlignY -> Float -> Float -> Float -> Float
alignY ay cy ch ih = cy + (ch - ih) * alignYFraction ay
