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
  , truncateTextMiddleIO
  , truncateTextStartIO
  , truncateTextWithIO
  , truncateTextPure
  ) where

import Control.Monad (forM_, unless)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
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
  widestRef <- newIORef 0
  fitRef <- newIORef 0
  breakRef <- newIORef (1 / 0)
  let lineOf w = do
        modifyIORef' widestRef (max w)
        if w <= maxW then modifyIORef' fitRef (max w) else breakAt w
      breakAt w = modifyIORef' breakRef (min w)
      wrapParagraph para
        | T.null para = pure [""]
        | otherwise = do
            w <- lineW para
            let perLine = charsPerWidth w para maxW
            if w <= maxW
              then [para] <$ lineOf w
              else do
                breakAt w
                if T.any (== ' ') para
                  then wrapWords perLine para []
                  else reverse <$> charLines perLine para []
      wrapWords perLine = startLine
        where
          startLine rest acc
            | T.null rest = pure (reverse acc)
            | otherwise = do
                let word = T.take (wordEnd rest) rest
                width <- lineW word
                if width <= maxW
                  then extend rest (T.length word) acc
                  else do
                    breakAt width
                    pieces <- charLines perLine word []
                    case pieces of
                      piece : done -> extend (T.drop (T.length word - T.length piece) rest) (T.length piece) (done ++ acc)
                      [] -> startLine (T.dropWhile (== ' ') (T.drop (T.length word) rest)) acc
          extend fromLine lineLen acc = do
            let fits end = (<= maxW) <$> lineW (T.take end fromLine)
                guess
                  | T.compareLength fromLine perLine /= GT = T.length (T.dropWhileEnd (== ' ') fromLine)
                  | otherwise = max lineLen (prevWordEnd fromLine (perLine + 1))
                up end = case nextWordEnd fromLine end of
                  Just end' -> fits end' >>= \ok -> if ok then up end' else pure end
                  Nothing -> pure end
                down end
                  | end' <= lineLen = pure lineLen
                  | otherwise = fits end' >>= \ok -> if ok then pure end' else down end'
                  where
                    end' = prevWordEnd fromLine end
            end <-
              if guess <= lineLen
                then up lineLen
                else fits guess >>= \ok -> if ok then up guess else down guess
            let line = T.take end fromLine
                rest = T.dropWhile (== ' ') (T.drop end fromLine)
            lineW line >>= lineOf
            forM_ (nextWordEnd fromLine end) $ \end' -> lineW (T.take end' fromLine) >>= breakAt
            if T.null rest then pure (reverse (line : acc)) else startLine rest (line : acc)
      charLines perLine chunk acc
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
      WrapResult textLines <$> readIORef widestRef <*> readIORef fitRef <*> readIORef breakRef

wordEnd :: Text -> Int
wordEnd t =
  let (spaces, rest) = T.span (== ' ') t
   in T.length spaces + T.length (T.takeWhile (/= ' ') rest)

nextWordEnd :: Text -> Int -> Maybe Int
nextWordEnd fromLine end
  | T.null (T.dropWhile (== ' ') after) = Nothing
  | otherwise = Just (end + wordEnd after)
  where
    after = T.drop end fromLine

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
truncateTextIO = truncateTextEndM

-- | Truncate @txt@ in the middle (@"prefix...suffix"@) if it exceeds @maxW@.
truncateTextMiddleIO :: (Text -> IO Float) -> Float -> Text -> IO Text
truncateTextMiddleIO = truncateTextMiddleM

-- | Truncate @txt@ at the start (@"...suffix"@) if it exceeds @maxW@.
truncateTextStartIO :: (Text -> IO Float) -> Float -> Text -> IO Text
truncateTextStartIO = truncateTextStartM

-- | Truncate @txt@ according to an explicit 'TextEllipsis' mode.
truncateTextWithIO :: TextEllipsis -> (Text -> IO Float) -> Float -> Text -> IO Text
truncateTextWithIO = truncateTextWithM

-- | Pure single-line truncation against prepared 'FontMetrics'.
truncateTextPure :: TextEllipsis -> FontMetrics -> Float -> Text -> Text
truncateTextPure ell fm maxW txt =
  runIdentity (truncateTextWithM ell (Identity . lineWidth fm) maxW txt)

truncateTextWithM :: Monad m => TextEllipsis -> (Text -> m Float) -> Float -> Text -> m Text
truncateTextWithM EllipsisEnd = truncateTextEndM
truncateTextWithM EllipsisMiddle = truncateTextMiddleM
truncateTextWithM EllipsisStart = truncateTextStartM

truncateTextEndM :: Monad m => (Text -> m Float) -> Float -> Text -> m Text
truncateTextEndM lineW maxW txt
  | maxW <= 0 = pure ""
  | otherwise = do
      w <- lineW txt
      if w <= maxW
        then pure txt
        else do
          ellW <- lineW "..."
          if maxW <= ellW
            then fst <$> takeWidth (charsPerWidth w txt maxW) lineW maxW txt
            else do
              (fit, _) <- takeWidth (charsPerWidth w txt (maxW - ellW)) lineW (maxW - ellW) txt
              let dotted = T.dropWhileEnd (== '.') fit <> "..."
              tooWide <- if clusterLength fit == T.length fit then (> maxW) <$> lineW dotted else pure False
              if tooWide
                then fst <$> takeWidth (charsPerWidth w txt maxW) lineW maxW txt
                else pure dotted

truncateTextStartM :: Monad m => (Text -> m Float) -> Float -> Text -> m Text
truncateTextStartM lineW maxW txt
  | maxW <= 0 = pure ""
  | otherwise = do
      w <- lineW txt
      if w <= maxW
        then pure txt
        else do
          ellW <- lineW "..."
          if maxW <= ellW
            then takeTailWidth (charsPerWidth w txt maxW) lineW maxW txt
            else do
              suf <- takeTailWidth (charsPerWidth w txt (maxW - ellW)) lineW (maxW - ellW) txt
              let dotted = "..." <> T.dropWhile (== '.') suf
              tooWide <- (> maxW) <$> lineW dotted
              if tooWide
                then takeTailWidth (charsPerWidth w txt maxW) lineW maxW txt
                else pure dotted

truncateTextMiddleM :: Monad m => (Text -> m Float) -> Float -> Text -> m Text
truncateTextMiddleM lineW maxW txt
  | maxW <= 0 = pure ""
  | otherwise = do
      w <- lineW txt
      if w <= maxW
        then pure txt
        else do
          ellW <- lineW "..."
          if maxW <= ellW
            then fst <$> takeWidth (charsPerWidth w txt maxW) lineW maxW txt
            else do
              let remW = maxW - ellW
                  halfW = remW * 0.5
              suf <- takeTailWidth (charsPerWidth w txt halfW) lineW halfW txt
              sufW <- if T.null suf then pure 0 else lineW suf
              let preBudget = max 0 (remW - sufW)
                  prePool = T.take (max 0 (T.length txt - T.length suf)) txt
              pre0 <- takeFitPrefix (charsPerWidth w txt preBudget) lineW preBudget prePool
              if T.null pre0 && T.null suf
                then fst <$> takeWidth (charsPerWidth w txt maxW) lineW maxW txt
                else do
                  let dotted0 = T.dropWhileEnd (== '.') pre0 <> "..." <> suf
                  ok0 <- (<= maxW) <$> lineW dotted0
                  if ok0
                    then pure dotted0
                    else
                      if not (T.null pre0)
                        then do
                          let (s, _) = clusterBounds pre0 (max 0 (T.length pre0 - 1))
                              dotted1 = T.dropWhileEnd (== '.') (T.take s pre0) <> "..." <> suf
                          ok1 <- (<= maxW) <$> lineW dotted1
                          if ok1 then pure dotted1 else truncateTextEndM lineW maxW txt
                        else truncateTextStartM lineW maxW txt
