-- | A chat reply streamed into a Markdown widget a few characters at a time,
-- the way a language model's answer arrives.
module ChatDemo (chatDemoUi) where

import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
  ( NanoUI
  , ScrollMetrics (..)
  , V2 (..)
  , background
  , button
  , card
  , colorRGBA
  , columnWith
  , currentId
  , fillH
  , fillW
  , flex
  , gap
  , getScrollMetricsUi
  , heading
  , label
  , maxW
  , muted
  , padAll
  , padXY
  , panelStyle
  , panelWith
  , rowWith
  , scrollArea
  , setScrollOffsetUi
  , styled
  , tight
  , toolbar
  , uiTime
  , useState
  , v2Y
  , wakeAfter
  , whenM
  )
import NanoUI.Markdown

-- | The reply so far: when it started, how many tokens have arrived, and
-- the document they make.
data Stream = Stream
  { streamStart :: !Double
  , streamShown :: !Int
  , streamDoc :: !MarkdownDoc
  }
  deriving (Eq)

-- | The reply, cut into tokens of a few characters.
tokens :: [Text]
tokens = T.chunksOf 4 reply

tokensPerSecond :: Double
tokensPerSecond = 60

chatDemoUi :: NanoUI ()
chatDemoUi = do
  now <- uiTime
  (started, setStream) <- useState Nothing
  let stream = fromMaybe (Stream now 0 emptyMarkdown) started
      -- The tokens due by now, counted from the start, so a second view
      -- pass in a frame finds none left to append.
      due = min (length tokens) (floor ((now - streamStart stream) * tokensPerSecond))
      fresh = take (due - streamShown stream) (drop (streamShown stream) tokens)
      appended = due > streamShown stream
      stream'
        | appended = stream {streamShown = due, streamDoc = appendMarkdown (T.concat fresh) (streamDoc stream)}
        | otherwise = stream
      streaming = streamShown stream' < length tokens
  -- Stored when it starts and when tokens arrive, whose count then differs
  -- from the stored one's, so the setter's equality check need not compare
  -- the documents.
  when (isNothing started || appended) $ setStream (Just stream')
  -- Ask for a frame when the next token is due: the loop sleeps in between.
  when streaming $
    wakeAfter (max 0.001 (fromIntegral (streamShown stream' + 1) / tokensPerSecond - (now - streamStart stream')))
  (lastLink, setLastLink) <- useState ("" :: Text)
  columnWith (fillW . fillH . padAll 16 . gap 12) $ do
    toolbar $ do
      heading "Chat"
      flex
      muted (if streaming then "Receiving..." else T.pack (show (length tokens)) <> " tokens")
      whenM (button "Replay") $
        setStream (Just (Stream now 0 emptyMarkdown))
    sid <- currentId
    metrics <- getScrollMetricsUi sid
    -- Follow the reply as text arrives, unless it has been scrolled up.
    for_ metrics $ \m ->
      when (appended && v2Y (scrollOffset m) >= v2Y (scrollRange m) - 24) $
        setScrollOffsetUi sid (V2 0 1e9)
    _ <- scrollArea (fillW . fillH) $
      columnWith (fillW . padAll 4 . gap 12) $ do
        styled (panelStyle (background (colorRGBA 58 64 86 255))) $
          rowWith (fillW . tight) $ do
            flex
            panelWith (padXY 12 8 . maxW 520) $
              label "How do I stream Markdown into a nano-ui view?"
        card $ do
          target <- markdown (streamDoc stream')
          for_ target setLastLink
    unless (T.null lastLink) $ muted ("Clicked link: " <> lastLink)

reply :: Text
reply =
  T.unlines
    [ "## Streaming Markdown"
    , ""
    , "Keep a `MarkdownDoc` in your model and **append** the tokens that arrived"
    , "in a frame, joined:"
    , ""
    , "```haskell"
    , "onTokens :: [Text] -> Model -> Model"
    , "onTokens tokens m = m {reply = appendMarkdown (T.concat tokens) (reply m)}"
    , "```"
    , ""
    , "`appendMarkdown` keeps the blocks that are already *finished*, so each"
    , "token re-parses only the unfinished end of the reply."
    , ""
    , "### What it draws"
    , ""
    , "1. Headings, paragraphs, **strong**, *emphasis* and ~~strikethrough~~"
    , "2. Lists, tight or loose, nested:"
    , "   - [x] with task items"
    , "   - [ ] and ones still to do"
    , "3. Links such as [the nano-ui repository](https://github.com/goolord/nano-ui)"
    , "   and bare ones like https://hackage.haskell.org"
    , ""
    , "> Block quotes, too.  "
    , "> Each finished block keeps its widget ids, so its text is not laid out again."
    , ""
    , "| Block | Cached |"
    , "|:------|:------:|"
    , "| Finished | yes |"
    , "| Open | no |"
    , ""
    , "---"
    , ""
    , "Click a link to see its destination below."
    ]
