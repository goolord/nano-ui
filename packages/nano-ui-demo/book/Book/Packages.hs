-- | Code for the book's Ecosystem Packages chapter
-- (@book/src/packages/index.md@). The chart examples are in
-- @nano-ui-svg/app/ChartExamples.hs@.
module Book.Packages where

import Control.Monad (void)
import Data.Foldable (for_)
import Data.Text (Text)
import NanoUI
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, enableSvgExport, frameToSvg)
import NanoUI.Form (Form, FormState, inputText, nanoFormSubmit, notEmpty, prove, validEmail, withFieldErrors)
import NanoUI.Markdown
import NanoUI.Testing (newPixelContext, runFrame)
import NanoUI.Testing.Harness (withInputOff)
import NanoUI.Backend.Rgfw qualified as Rgfw
import NanoUI.Backend.Sdl qualified as Sdl

-- ANCHOR: sdl
sdlMain :: IO ()
sdlMain = Sdl.runSdlApp
  Sdl.defaultSdlOptions
    { Sdl.sdlWindowSettings = defaultWindowSettings {wsTitle = "My App", wsSize = Size 1024 768}
    , Sdl.sdlAppFont = Sdl.FontSearch ["Segoe UI", "SF Pro Text", "Inter"]
    , Sdl.sdlAppFontSize = 14
    , Sdl.sdlAppTheme = Just defaultTheme
    }
  appView
-- ANCHOR_END: sdl

-- ANCHOR: rgfw
rgfwMain :: IO ()
rgfwMain = Rgfw.runRgfwApp Rgfw.defaultRgfwOptions appView
-- ANCHOR_END: rgfw

-- ANCHOR: form
data Signup = Signup Text Text

signup :: Form Text Signup
signup =
  Signup
    <$> withFieldErrors (inputText "Name" "" `prove` notEmpty "Name is required")
    <*> withFieldErrors (inputText "Email" "" `prove` validEmail (const "Not an email address"))
-- ANCHOR_END: form

-- ANCHOR: form_submit
signupView :: FormState -> NanoUI ()
signupView owner = do -- owner <- newFormState in setup
  submitted <- nanoFormSubmit owner "signup" "Sign up" signup
  case submitted of
    Just (Signup name _) -> label ("Welcome, " <> name)
    Nothing              -> pure ()
-- ANCHOR_END: form_submit

-- ANCHOR: markdown
doc :: MarkdownDoc
doc = parseMarkdown "# Hello\n\nSome *emphasis* and `code`."

view :: MarkdownCache -> NanoUI ()
view cache = do -- cache <- newMarkdownCache in setup
  clicked <- markdown cache doc
  for_ clicked $ \url -> liftIO (void (Sdl.openUrl url))
-- ANCHOR_END: markdown

-- ANCHOR: markdown_stream
reply :: Stream Int MarkdownDoc -> MarkdownCache -> Int -> NanoUI ()
reply stream cache replyId = do
  answer <- useStream stream replyId emptyMarkdown $ \update ->
    onToken (\token -> update (appendMarkdown token))
  void (markdown cache answer)
-- ANCHOR_END: markdown_stream

-- ANCHOR: frame_svg
exportViewToSvg :: NanoUI () -> IO Text
exportViewToSvg v = do
  ctx <- newPixelContext
  let inp = withInputOff 1280 800
  enableSvgExport ctx
  (_, drawData, _) <- runFrame ctx inp v
  frameToSvg defaultFrameSvgConfig ctx inp drawData
-- ANCHOR_END: frame_svg

-- Stand-ins for application code the snippets refer to.

appView :: NanoUI ()
appView = pure ()

onToken :: (Text -> IO ()) -> IO ()
onToken emitToken = emitToken "Hello"
