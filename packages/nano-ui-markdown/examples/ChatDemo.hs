-- | A chat reply streamed into a Markdown widget a few characters at a time
-- with 'useStream', as a language model's answer arrives.
module ChatDemo (newChatDemoUi) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
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
  , useInt
   , useText
  , useStream
  , Stream
  , newStream
  , v2Y
  , whenM
  )
import NanoUI.Markdown

-- | The reply in 4-character tokens.
tokens :: [Text]
tokens = T.chunksOf 4 reply

tokensPerSecond :: Int
tokensPerSecond = 60

newChatDemoUi :: IO (NanoUI ())
newChatDemoUi = chatDemoUi <$> newStream <*> newMarkdownCache

chatDemoUi :: Stream Int (Int, MarkdownDoc) -> MarkdownCache -> NanoUI ()
chatDemoUi stream cache = do
  -- A worker thread appends tokens; the view reads the token count and the
  -- document so far. Replay restarts the stream under a new key.
  (replays, setReplays) <- useInt 0
  (arrived, doc) <- useStream stream replays (0 :: Int, emptyMarkdown) $ \update ->
    forM_ tokens $ \token -> do
      threadDelay (1000000 `div` tokensPerSecond)
      -- Force the document here, so the parse runs on this thread.
      update (\(n, d) -> let !n' = n + 1; !d' = appendMarkdown token d in (n', d'))
  let streaming = arrived < length tokens
  -- Token count shown last frame, used to detect growth.
  (shown, setShown) <- useInt 0
  let grew = arrived /= shown
  when grew (setShown arrived)
  (lastLink, setLastLink) <- useText ""
  columnWith (fillW . fillH . padAll 16 . gap 12) $ do
    toolbar $ do
      heading "Chat"
      flex
      muted (if streaming then "Receiving..." else T.pack (show (length tokens)) <> " tokens")
      whenM (button "Replay") (setReplays (replays + 1))
    sid <- currentId
    metrics <- getScrollMetricsUi sid
    -- Stick to the bottom as text arrives, unless scrolled up.
    for_ metrics $ \m ->
      when (grew && v2Y (scrollOffset m) >= v2Y (scrollRange m) - 24) $
        setScrollOffsetUi sid (V2 0 1e9)
    _ <- scrollArea (fillW . fillH) $
      columnWith (fillW . padAll 4 . gap 12) $ do
        styled (panelStyle (background (colorRGBA 58 64 86 255))) $
          rowWith (fillW . tight) $ do
            flex
            panelWith (padXY 12 8 . maxW 520) $
              label "How do I stream Markdown into a nano-ui view?"
        card $ do
          target <- markdown cache doc
          for_ target setLastLink
    unless (T.null lastLink) $ muted ("Clicked link: " <> lastLink)

reply :: Text
reply =
  T.unlines
    [ "## Streaming Markdown"
    , ""
    , "Receive the reply in a `useStream` producer, and **append** each token"
    , "to a `MarkdownDoc` there:"
    , ""
    , "```haskell"
    , "-- Allocate stream <- newStream once during setup."
    , "doc <- useStream stream replyId emptyMarkdown $ \\update ->"
    , "  onToken client $ \\token -> update (appendMarkdown token)"
    , "```"
    , ""
    , "`appendMarkdown` keeps the blocks that are already *finished*, so each"
    , "token re-parses only the unfinished end of the reply, and it does so on"
    , "the producer's thread."
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
