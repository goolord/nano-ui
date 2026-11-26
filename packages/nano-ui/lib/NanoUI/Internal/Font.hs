{-# LANGUAGE StrictData #-}

-- | Font measurement, wrapping, truncation, and shared widget metrics.
module NanoUI.Internal.Font
  ( module NanoUI.Internal.Font.Types
  , module NanoUI.Internal.Metrics
  , WrapResult (..)
  , wrapMeasure
  , wrapTextIO
  , wrapTextLinesIO
  , truncateTextIO
  , truncateTextWithIO
  , truncateTextPure
  ) where

import Control.Monad (unless)
import Data.Functor.Identity (Identity (..))
import Data.Primitive.PrimVar (newPrimVar, readPrimVar, writePrimVar)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe (Iter (..), iter)
import Data.Text.Foreign (lengthWord8)
import NanoUI.Internal.Font.Types
import NanoUI.Internal.Metrics
import NanoUI.Internal.Style (TextEllipsis (..))
import NanoUI.Internal.TextCluster (clusterBounds, clusterLength)
import NanoUI.Internal.Types (clamp)

-- | The size of text wrapped to @maxW@ into the lines of @wrap@.
wrapMeasure :: FontMetrics -> Float -> WrapResult -> (Float, Float)
wrapMeasure fm maxW wrap =
  let lineH = fmLineHeight fm
   in case wrLines wrap of
        [] -> (0, lineH)
        textLines -> (min maxW (wrWidest wrap), lineH * fromIntegral (length textLines))

-- | Text wrapped to a width, and the widths it wraps the same at.
data WrapResult = WrapResult
  { wrLines :: [Text]
  -- ^ The lines, as 'wrapTextLinesIO' gives them.
  , wrWidest :: Float
  -- ^ The widest line, which is wider than the width asked for when a
  -- single character is.
  , wrFitW :: Float
  -- ^ The widest line that fit.
  , wrBreakW :: Float
  -- ^ The narrowest width at which some line would take another word or
  -- character, or a paragraph would fit whole.
  }
  deriving (Eq, Show)

-- | Wrap each paragraph to @maxW@ using the host line measure.
wrapTextLinesIO :: (Text -> IO Float) -> Text -> Float -> IO [Text]
wrapTextLinesIO lineW txt maxW = wrLines <$> wrapTextIO lineW txt maxW

-- | 'wrapTextLinesIO' with the widths its result holds for.
wrapTextIO :: (Text -> IO Float) -> Text -> Float -> IO WrapResult
wrapTextIO lineW txt maxW = do
  widestVar <- newPrimVar (0 :: Float)
  fitVar <- newPrimVar (0 :: Float)
  breakVar <- newPrimVar (1 / 0 :: Float)
  let lineOf !w = do
        curW <- readPrimVar widestVar
        if w > curW then writePrimVar widestVar w else pure ()
        if w <= maxW
          then do
            curF <- readPrimVar fitVar
            if w > curF then writePrimVar fitVar w else pure ()
          else breakAt w
      breakAt !w = do
        curB <- readPrimVar breakVar
        if w < curB then writePrimVar breakVar w else pure ()
      wrapParagraph para
        | T.null para = pure [""]
        | otherwise = do
            w <- lineW para
            let !perLine = charsPerWidth w para maxW
            if w <= maxW
              then [para] <$ lineOf w
              else do
                breakAt w
                if T.any (== ' ') para
                  then wrapWords perLine para []
                  else reverse <$> charLines perLine para []
      wrapWords !perLine = startLine
        where
          startLine rest acc
            | T.null rest = pure (reverse acc)
            | otherwise = do
                let !wEnd = wordEnd rest
                    word = T.take wEnd rest
                width <- lineW word
                if width <= maxW
                  then extend rest wEnd width acc
                  else do
                    breakAt width
                    pieces <- charLines perLine word []
                    case pieces of
                      piece : done -> do
                        let !pLen = T.length piece
                            rest' = T.drop (wEnd - pLen) rest
                        pw <- lineW piece
                        extend rest' pLen pw (done ++ acc)
                      [] -> startLine (T.dropWhile (== ' ') (T.drop wEnd rest)) acc
          extend fromLine !lineLen !lineWidth0 acc = do
            let !guess =
                  if T.compareLength fromLine perLine /= GT
                    then T.length (T.dropWhileEnd (== ' ') fromLine)
                    else max lineLen (prevWordEnd fromLine (perLine + 1))
                -- Carry both the last fitting (end, width) and the first
                -- overflowing width so neither is measured a second time.
                up !end !endW = case nextWordEnd fromLine end of
                  Nothing -> pure (end, endW, 1 / 0)
                  Just end' -> do
                    w' <- lineW (T.take end' fromLine)
                    if w' <= maxW then up end' w' else pure (end, endW, w')
                down !end !overW
                  | end' <= lineLen = pure (lineLen, lineWidth0, overW)
                  | otherwise = do
                      w' <- lineW (T.take end' fromLine)
                      if w' <= maxW then pure (end', w', overW) else down end' w'
                  where
                    !end' = prevWordEnd fromLine end
            (!end, !endW, !overW) <-
              if guess <= lineLen
                then up lineLen lineWidth0
                else do
                  gw <- lineW (T.take guess fromLine)
                  if gw <= maxW
                    then up guess gw
                    else down guess gw
            let line = T.take end fromLine
                rest = T.dropWhile (== ' ') (T.drop end fromLine)
            lineOf endW
            if overW < 1 / 0
              then breakAt overW
              else pure ()
            if T.null rest then pure (reverse (line : acc)) else startLine rest (line : acc)
      charLines !perLine chunk acc
        | T.null chunk = pure acc
        | otherwise = do
            (line, rest) <- takeWidth perLine lineW maxW chunk
            if T.null line
              then pure acc
              else do
                lineW line >>= lineOf
                unless (T.null rest) $
                  lineW (T.take (snd (clusterBounds chunk (T.length line + 1))) chunk) >>= breakAt
                charLines perLine rest (line : acc)
  if maxW <= 0
    then pure (WrapResult [] 0 1 0)
    else do
      textLines <- concat <$> mapM wrapParagraph (T.lines txt)
      WrapResult textLines <$> readPrimVar widestVar <*> readPrimVar fitVar <*> readPrimVar breakVar

wordEnd :: Text -> Int
wordEnd t = goSpaces 0 0
  where
    !lenB = lengthWord8 t
    goSpaces !b !chars
      | b >= lenB = chars
      | otherwise = case iter t b of
          Iter c d
            | c == ' ' -> goSpaces (b + d) (chars + 1)
            | otherwise -> goWord (b + d) (chars + 1)
    goWord !b !chars
      | b >= lenB = chars
      | otherwise = case iter t b of
          Iter c d
            | c == ' ' -> chars
            | otherwise -> goWord (b + d) (chars + 1)

nextWordEnd :: Text -> Int -> Maybe Int
nextWordEnd fromLine !end = skipSpaces after 0 0
  where
    after = T.drop end fromLine
    !lenB = lengthWord8 after
    skipSpaces !t !b !chars
      | b >= lenB = Nothing
      | otherwise = case iter t b of
          Iter c d
            | c == ' ' -> skipSpaces t (b + d) (chars + 1)
            | otherwise -> Just (end + scanWord t (b + d) (chars + 1))
    scanWord !t !b !chars
      | b >= lenB = chars
      | otherwise = case iter t b of
          Iter c d
            | c == ' ' -> chars
            | otherwise -> scanWord t (b + d) (chars + 1)

prevWordEnd :: Text -> Int -> Int
prevWordEnd fromLine end =
  T.length (T.dropWhileEnd (== ' ') (T.dropWhileEnd (/= ' ') (T.take end fromLine)))

charsPerWidth :: Float -> Text -> Float -> Int
charsPerWidth w txt maxW
  | w <= 0 = maxBound `div` 2
  | otherwise = floor (maxW * fromIntegral (T.length txt) / w)

largestFitting :: Monad m => (Int -> m Bool) -> Int -> Int -> Int -> m Int
largestFitting fits lo0 hi0 guess
  | hi0 <= lo0 = pure lo0
  | otherwise = do
      let g = clamp (lo0 + 1) hi0 guess
      ok <- fits g
      if ok then up g 1 else down g 1
  where
    up !lo !step
      | lo >= hi0 = pure lo
      | otherwise = do
          let p = min hi0 (lo + step)
          ok <- fits p
          if ok then up p (step * 2) else bisect lo p
    down !hi !step
      | p <= lo0 = bisect lo0 hi
      | otherwise = do
          ok <- fits p
          if ok then bisect p hi else down p (step * 2)
      where
        p = hi - step
    bisect !lo !hi
      | hi - lo <= 1 = pure lo
      | otherwise = do
          let mid = (lo + hi) `div` 2
          ok <- fits mid
          if ok then bisect mid hi else bisect lo mid

takeWidth :: Monad m => Int -> (Text -> m Float) -> Float -> Text -> m (Text, Text)
takeWidth guess lineW maxW txt
  | T.null txt = pure (txt, T.empty)
  | otherwise = do
      k <- largestFitting fits 1 (T.length txt) guess
      let (start, end) = clusterBounds txt k
      pure (T.splitAt (if start > 0 then start else end) txt)
  where
    fits k
      | T.compareLength txt k == LT = pure False
      | otherwise = (<= maxW) <$> lineW (T.take k txt)

takeFitPrefix :: Monad m => Int -> (Text -> m Float) -> Float -> Text -> m Text
takeFitPrefix guess lineW maxW txt
  | T.null txt || maxW <= 0 = pure T.empty
  | otherwise = do
      let (_, end1) = clusterBounds txt 0
      ok1 <- (<= maxW) <$> lineW (T.take end1 txt)
      if not ok1
        then pure T.empty
        else fst <$> takeWidth guess lineW maxW txt

takeTailWidth :: Monad m => Int -> (Text -> m Float) -> Float -> Text -> m Text
takeTailWidth guess lineW maxW txt
  | T.null txt || maxW <= 0 = pure T.empty
  | otherwise = do
      let !len = T.length txt
          startAt k =
            let raw = max 0 (len - k)
                (cs, ce) = clusterBounds txt raw
             in if cs == raw then cs else ce
          fits k =
            let idx = startAt k
             in if idx >= len then pure False else (<= maxW) <$> lineW (T.drop idx txt)
      ok1 <- fits 1
      if not ok1
        then pure T.empty
        else do
          k <- largestFitting fits 1 len guess
          pure (T.drop (startAt k) txt)

-- | Truncate @txt@ with a trailing ellipsis if it exceeds @maxW@ using @lineW@.
truncateTextIO :: (Text -> IO Float) -> Float -> Text -> IO Text
truncateTextIO = truncateTextWithM EllipsisEnd

-- | Truncate @txt@ according to an explicit 'TextEllipsis' mode.
truncateTextWithIO :: TextEllipsis -> (Text -> IO Float) -> Float -> Text -> IO Text
truncateTextWithIO = truncateTextWithM

-- | Pure single-line truncation against prepared 'FontMetrics'.
truncateTextPure :: TextEllipsis -> FontMetrics -> Float -> Text -> Text
truncateTextPure ell fm maxW txt =
  runIdentity (truncateTextWithM ell (Identity . lineWidth fm) maxW txt)

truncateTextWithM :: Monad m => TextEllipsis -> (Text -> m Float) -> Float -> Text -> m Text
truncateTextWithM ell lineW maxW txt
  | maxW <= 0 = pure ""
  | otherwise = do
      w <- lineW txt
      if w <= maxW
        then pure txt
        else do
          ellW <- lineW "..."
          let fitHead lim = fst <$> takeWidth (charsPerWidth w txt lim) lineW lim txt
              fitTail lim = takeTailWidth (charsPerWidth w txt lim) lineW lim txt
              remW = maxW - ellW
              endCase = do
                fit <- fitHead remW
                let dotted = T.dropWhileEnd (== '.') fit <> "..."
                tooWide <- if clusterLength fit == T.length fit then (> maxW) <$> lineW dotted else pure False
                if tooWide then fitHead maxW else pure dotted
              startCase = do
                suf <- fitTail remW
                let dotted = "..." <> T.dropWhile (== '.') suf
                tooWide <- (> maxW) <$> lineW dotted
                if tooWide then fitTail maxW else pure dotted
          if maxW <= ellW
            then (if ell == EllipsisStart then fitTail else fitHead) maxW
            else case ell of
              EllipsisEnd -> endCase
              EllipsisStart -> startCase
              EllipsisMiddle -> do
                suf <- fitTail (remW * 0.5)
                sufW <- if T.null suf then pure 0 else lineW suf
                let preBudget = max 0 (remW - sufW)
                    prePool = T.take (max 0 (T.length txt - T.length suf)) txt
                pre0 <- takeFitPrefix (charsPerWidth w txt preBudget) lineW preBudget prePool
                if T.null pre0 && T.null suf
                  then fitHead maxW
                  else do
                    let dotted0 = T.dropWhileEnd (== '.') pre0 <> "..." <> suf
                    ok0 <- (<= maxW) <$> lineW dotted0
                    if ok0
                      then pure dotted0
                      else if not (T.null pre0)
                        then do
                          let (s, _) = clusterBounds pre0 (max 0 (T.length pre0 - 1))
                              dotted1 = T.dropWhileEnd (== '.') (T.take s pre0) <> "..." <> suf
                          ok1 <- (<= maxW) <$> lineW dotted1
                          if ok1 then pure dotted1 else endCase
                        else startCase
