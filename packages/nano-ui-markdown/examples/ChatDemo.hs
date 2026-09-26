-- | A chat reply streamed into a Markdown widget a few characters at a time,
-- the way a language model's answer arrives, with 'useStream'.
module ChatDemo (chatDemoUi) where

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
  , useState
  , useStream
  , v2Y
  , whenM
  )
import NanoUI.Markdown

-- | The reply, cut into tokens of a few characters.
tokens :: [Text]
tokens = T.chunksOf 4 reply

tokensPerSecond :: Int
tokensPerSecond = 60

chatDemoUi :: NanoUI ()
chatDemoUi = do
  -- The reply streams in on a thread of its own, as a language model's
  -- answer would from the network: each token is appended there, and the
  -- view reads how many have arrived and the document they make. Replay
  -- starts the stream again under a new key.
  (replays, setReplays) <- useInt 0
  (arrived, doc) <- useStream replays (0 :: Int, emptyMarkdown) $ \update ->
    forM_ tokens $ \token -> do
      threadDelay (1000000 `div` tokensPerSecond)
      update (\(n, d) -> (n + 1, appendMarkdown token d))
  let streaming = arrived < length tokens
  -- How many tokens the last frame showed, to follow the reply as it grows.
  (shown, setShown) <- useInt 0
  let grew = arrived /= shown
  when grew (setShown arrived)
  (lastLink, setLastLink) <- useState ("" :: Text)
  columnWith (fillW . fillH . padAll 16 . gap 12) $ do
    toolbar $ do
      heading "Chat"
      flex
      muted (if streaming then "Receiving..." else T.pack (show (length tokens)) <> " tokens")
      whenM (button "Replay") (setReplays (replays + 1))
    sid <- currentId
    metrics <- getScrollMetricsUi sid
    -- Follow the reply as text arrives, unless it has been scrolled up.
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
          target <- markdown doc
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
    , "doc <- useStream replyId emptyMarkdown $ \\update ->"
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
