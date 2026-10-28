# Ecosystem Packages

The core toolkit is `nano-ui`; the other packages add a backend or a focused capability. Add only what the application uses.

`nano-ui` is on Hackage. The repository may be ahead of the released API; use the source pin in [Quick Start](../getting-started/quickstart.md) for the API in this book.

| Package | What it adds |
| --- | --- |
| `nano-ui-sdl` | SDL3 window backend, TrueType fonts, native file dialogs, custom chrome |
| `nano-ui-rgfw` | RGFW + OpenGL 3.2 backend with a bundled bitmap font |
| `nano-ui-diagrams` | Line/bar/scatter/area charts and diagrams-lib content |
| `nano-ui-form` | Validated applicative forms built on ditto |
| `nano-ui-markdown` | CommonMark rendering, appendable for streamed replies |
| `nano-ui-svg` | `DrawOp`-to-SVG and whole-frame SVG export |

## Backends

`nano-ui-sdl` is the primary desktop backend:

```haskell
import NanoUI.Backend.Sdl

main = runSdlApp
  defaultSdlOptions
    { sdlWindowSettings = defaultWindowSettings {wsTitle = "My App", wsSize = Size 1024 768}
    , sdlAppFont = FontSearch ["Segoe UI", "SF Pro Text", "Inter"]
    , sdlAppFontSize = 14
    , sdlAppTheme = Just defaultTheme
    }
  appView
```

It adds HarfBuzz shaping, installed-font lookup, high-DPI handling, non-blocking native file dialogs (`useFileDialog`), custom title bars (`windowCaption`), and screenshots (`saveScreenshot`, `captureScreenshot`). See [Desktop Windows](../advanced/window-management.md) for the window side.

`nano-ui-rgfw` is the zero-system-dependency alternative:

```haskell
import NanoUI.Backend.Rgfw

main = runRgfwApp defaultRgfwOptions appView
```

It links RGFW statically, bundles a bitmap font, and produces small standalone binaries. Use it for tools, overlays, or environments where SDL3 is not available.

## Charts and diagrams

`nano-ui-diagrams` provides `NanoUI.Plot` and the diagrams-lib widget:

```haskell
import NanoUI
import NanoUI.Plot

cpuChart app samples =
  void $ plot (appPlots app) (fillW . fixedH 220) $
    withGrid GridBoth
      . withXAxis "time (s)"
      . withYAxis "CPU %"
      . withLegend LegendNone
      $ chart [line "cpu" samples]
```

Series constructors are `line`, `scatter`, `bar`, `area`, and `step`; `withColor`, `withStrokeWidth`, `withMarker`, and `withBaseline` style them. `lineChart`, `barChart`, `scatterChart`, and `areaChart` are one-series shortcuts. `plot` returns a `PlotResponse` whose `plotHover` is the nearest data point under the pointer:

```haskell
case plotHover resp of
  Nothing -> muted "Hover a point"
  Just h  -> kv "value" (T.pack (show (round (hoverDataY h) :: Int)))
```

Allocate `newPlotCache` once in `IO`; it rebuilds a chart's diagram only when the chart or theme changes. `diagram (fixedWH 320 200)` places arbitrary diagrams-lib content. The live chart tutorial uses a `Stream` for the data: [Live Data & Background Work](../tutorials/live-data.md).

## Validated forms

`nano-ui-form` builds applicative forms on ditto. A `Form Text a` has `Text` errors and returns `a`:

```haskell
data Signup = Signup Text Text

signup :: Form Text Signup
signup =
  Signup
    <$> withFieldErrors (inputText "Name" "" `prove` notEmpty "Name is required")
    <*> withFieldErrors (inputText "Email" "" `prove` validEmail (const "Not an email address"))
```

Allocate `owner <- newFormState` once, then run it with a stable prefix:

```haskell
submitted <- nanoFormSubmit owner "signup" "Sign up" signup
case submitted of
  Just (Signup name _) -> label ("Welcome, " <> name)
  Nothing              -> pure ()
```

`nanoFormLive` returns `Just a` whenever the form is valid; `nanoFormEx` takes a `FormConfig`; `runNanoForm` returns the view and result separately. Inputs include `inputText`, `inputPassword`, `inputTextArea`, `inputCheckbox`, `inputSlider`, `inputSelect`, `inputRadio`, and `inputColor`. Validation includes `notEmpty`, `minLength`, `maxLength`, `inRange`, `validEmail`, `matches`, and `customProof`. `inputWidget` adapts any `Var a -> NanoUI Response` widget, with a publication policy (`OnChange`, `OnChangeOr`, `OnlyWhen`).

## Markdown

`nano-ui-markdown` parses with commonmark and draws with rich text:

```haskell
import NanoUI.Markdown

doc = parseMarkdown "# Hello\n\nSome *emphasis* and `code`."

view cache = do
  clicked <- markdown cache doc
  for_ clicked $ \url -> openUrl url
```

Allocate `cache <- newMarkdownCache` once. `markdown` returns the link clicked this frame. For streaming, append without reparsing the whole document:

```haskell
doc <- useStream stream replyId emptyMarkdown $ \update ->
  onToken client (\token -> update (appendMarkdown token))
void (markdown cache doc)
```

When appending on the UI thread, append a frame's tokens in one call. The parser reparses only the tail block, with a few documented exceptions (a long unsplittable final block, or a link-reference definition). `MarkdownConfig.mdBlock` overrides block drawing, for example to syntax-highlight code.

## SVG export

`nano-ui-svg` has two levels:

- **`NanoUI.Draw.Svg`** exports `DrawOp` sequences: `drawOpsToSvg bounds ops` or `renderDrawOpsSvg config`.
- **`NanoUI.Draw.FrameSvg`** re-renders a whole frame: `frameToSvg config ctx input drawData`.

```haskell
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, enableSvgExport, frameToSvg)
import NanoUI.Testing (newPixelContext, runFrame)
import NanoUI.Testing.Harness (withInputOff)

exportViewToSvg view = do
  ctx <- newPixelContext
  let inp = withInputOff 1280 800
  enableSvgExport ctx
  (_, drawData, _) <- runFrame ctx inp view
  frameToSvg defaultFrameSvgConfig ctx inp drawData
```

`enableSvgExport` records the shapes paint draws and moves text onto the raster spans the exporter reads. `frameToSvg` fails on a context without it rather than export text alone. Ids in the document take a prefix from its hash, so several SVGs can be inlined in one page. A `newPixelContext` measures text in a 16 px monospace grid; run the frames inside `withSdl` for text measured in the real font. This is the path used for the interactive specimens in this book and for visual regression tests.

## See also

- Building an application: [Quick Start](../getting-started/quickstart.md) and [Tutorials](../tutorials/index.md).
- Windows and screenshots: [Desktop Windows](../advanced/window-management.md).
- Rendering internals and the frame: [Architecture & Internals](../architecture/index.md).
