# nano-ui-demo

Example applications and single-topic tutorials for [nano-ui](https://github.com/goolord/nano-ui), built primarily on the SDL3 backend (`nano-ui-sdl`).

The package is organized into two parts:

1. **Full applications** combining multiple features into complete programs: a widget and chart sampler, a text editor, a streaming log viewer, and a terminal emulator.
2. **Single-topic examples** in [`examples/`](examples/): short, self-contained programs focused on one concept each (`Var` state, layout modifiers, forms, shortcuts, overlays, theming, animations, background tasks, and more).

## Full applications

| Executable | Source | Description |
| --- | --- | --- |
| `nano-ui-sdl-demo` | [`examples/Demo.hs`](examples/Demo.hs), [`examples/sdl/`](examples/sdl/), [`lib/DemoData.hs`](lib/DemoData.hs) | Multi-tab sampler covering built-in widgets, layouts, custom 2D vector graphics, animated GIFs, rich text, trees, sortable tables, closable tabs, dockable split panes, and `nano-ui-diagrams` charts. Includes a live State sidebar, an About dialog (`F1`), and a Debug window (`F12`) with frame timings, damage counters, and an interactive layout inspector. |
| `nano-ui-web-sampler` | [`examples/Demo.hs`](examples/Demo.hs), [`examples/web/`](examples/web/) | The same sampler compiled to WebAssembly with `nano-ui-sdl-web` and [published online](https://goolord.github.io/nano-ui/demo/). Platform-specific adapters live in `DemoPlatform` under `examples/sdl` and `examples/web`: the web build embeds assets, loads Fontsource and Chromium local fonts, and opens/saves files by contents. Build it with `packages/nano-ui-sdl-web/scripts/build-web.sh dist-web nano-ui-demo:exe:nano-ui-web-sampler`. |
| `nano-ui-sdl-landing` | [`examples/Landing.hs`](examples/Landing.hs), [`examples/Bloom.hs`](examples/Bloom.hs), [`examples/sdl/LandingMain.hs`](examples/sdl/LandingMain.hs) | Desktop SDL3 build of the four-tab [landing page](https://goolord.github.io/nano-ui/) demo, sized to fit mobile viewports without scrolling. Includes the animated Bloom canvas plus Controls, Text, and Data tabs. |
| `nano-ui-web-landing` | Same sources with [`examples/web/LandingMain.hs`](examples/web/LandingMain.hs) | Browser build loaded by [`site/index.html`](../../site/index.html), depending on `nano-ui`, `nano-ui-shader`, and `nano-ui-sdl-web`. Build it with `packages/nano-ui-sdl-web/scripts/build-web.sh dist-web/landing nano-ui-demo:exe:nano-ui-web-landing`. |
| `nano-ui-sdl-notepad` | [`examples/Notepad.hs`](examples/Notepad.hs) | Plain-text editor with a `popup`-based menu bar, keyboard shortcuts, unsaved-change indicator in the window title, and non-blocking native open/save file dialogs. |
| `nano-ui-sdl-logs` | [`examples/Logs.hs`](examples/Logs.hs) | Streaming log viewer backed by a bounded ring buffer, with severity filters, selectable log lines, Select All / Copy All, and auto-scroll that pauses when you scroll up. |
| `nano-ui-sdl-terminal` | [`examples/Terminal.hs`](examples/Terminal.hs) | 80x24 terminal emulator attached to `/bin/sh` over a PTY (Linux and macOS only). Supports UTF-8, 16 ANSI colors with bold and inverse video, cursor motion, line/character edits, 2,000 lines of scrollback, and inline IME composition via `useInputMethod` (`TERM=ansi`). |
| `nano-ui-sdl-profile` | [`examples/Profile.hs`](examples/Profile.hs) | Frame-time and allocation benchmark runner for the demo UI. |
| `nano-ui-demo-svg` | [`examples/DemoSvg.hs`](examples/DemoSvg.hs) | Renders the demo's Plots tab headlessly to an SVG file using `nano-ui-svg`'s `frameToSvg`. Run with `cabal run nano-ui-demo-svg -- out.svg`. |

Launch any application with `cabal run`:

```sh
cabal run nano-ui-sdl-demo
cabal run nano-ui-sdl-notepad
cabal run nano-ui-sdl-logs
cabal run nano-ui-sdl-terminal
cabal run nano-ui-sdl-profile
```

Pass CLI flags to `nano-ui-sdl-demo` after `--`:

```sh
cabal run nano-ui-sdl-demo -- --help
cabal run nano-ui-sdl-demo -- --explain      # start with the layout inspector enabled
cabal run nano-ui-sdl-demo -- --continuous   # render continuously without vsync
cabal run nano-ui-sdl-demo -- --fullscreen   # launch in fullscreen mode
```

## Examples

Each single-topic example is a standalone executable that depends only on public library packages (`nano-ui`, `nano-ui-sdl`, and `nano-ui-diagrams` where needed), with no internal demo helpers. Copy any file straight into your own project as a starting point; the module header at the top explains what it demonstrates.

They are ordered roughly as a learning path:

| Source | Executable | Topics covered |
| --- | --- | --- |
| [`Hello.hs`](examples/Hello.hs) | `nano-ui-example-hello` | Minimal app setup, the per-frame view model, and local state with `useVar`. |
| [`Quickstart.hs`](examples/Quickstart.hs) | `nano-ui-example-quickstart` | Counter application walked through in the Quick Start guide. |
| [`State.hs`](examples/State.hs) | `nano-ui-example-state` | View-local state (`useVar`, `useEnumVar`), app-owned state (`newVar`), record field projections (`state.field`), conditional scopes (`scope`), and keyed list state (`withKey`). |
| [`Elm.hs`](examples/Elm.hs) | `nano-ui-example-elm` | Elm-style typed messages (`NanoUIE`), `emitWhen` / `emitChanged`, `mapMessages`, and `runSdlAppReduce` from `nano-ui-emit`. |
| [`Layout.hs`](examples/Layout.hs) | `nano-ui-example-layout` | `row`, `column`, and `grid` containers, composable `Layout -> Layout` modifiers, sizing (`Fit`, `fixedW`, `fillW`, `grow`), alignment, wrapping, badges, and `explainLayout`. |
| [`Components.hs`](examples/Components.hs) | `nano-ui-example-components` | Building reusable widgets from primitives, isolating internal widget IDs, exposing primed (`'`) and `With` variants, and deciding between internal vs. caller-owned `Var`s. |
| [`Forms.hs`](examples/Forms.hs) | `nano-ui-example-forms` | Storing form state in a single record `Var`, projecting field `Var`s, validating after first submission, disabling controls with `disabledWhen`, submitting on Enter (`respSubmitted`), and resetting defaults. |
| [`TodoList.hs`](examples/TodoList.hs) | `nano-ui-example-todo-list` | Adding, inline-editing, filtering, and reordering rows while keeping per-item `Var`s (`withElemVars`) and local widget state attached across mutations (`withKey`). |
| [`Keyboard.hs`](examples/Keyboard.hs) | `nano-ui-example-keyboard` | Chords (`shortcut`, `shortcutOnce`, `cmdOrCtrl`), arrow navigation, automatic key claiming by focused text fields, programmatic focus (`requestFocus`, `clearFocus`, `takeEscape`), and menu shortcuts. |
| [`Overlays.hs`](examples/Overlays.hs) | `nano-ui-example-overlays` | Menu bars, filter popups (`popup`), right-click context menus (`contextMenu`), tooltips (`tooltip`, `withTooltip`), modals (`modal`), floating windows (`floatingWindow`), and intercepting close requests (`wsExitOnCloseRequest`). |
| [`Windows.hs`](examples/Windows.hs) | `nano-ui-example-windows` | Multi-window desktop sessions using `nativeWindow` with shared `Var` state and independent per-window focus. |
| [`Theming.hs`](examples/Theming.hs) | `nano-ui-example-theming` | Switching built-in and Base16 themes (`setTheme`), following OS light/dark appearance (`systemAppearance`, `lightDark`), scoped overrides (`styled`), and reading theme colors for custom drawing (`uiTheme`). |
| [`Animation.hs`](examples/Animation.hs) | `nano-ui-example-animation` | Eased tweens (`Tween`) and spring physics (`Spring`) with `animateTo` and `animateToA`, smooth mid-flight retargeting, animated colors, continuous motion (`pulse`, `keepAnimating`), and idle sleep after animations settle. |
| [`Images.hs`](examples/Images.hs) | `nano-ui-example-images` | Leasing procedural RGBA textures in the atlas (`useImageRgba`), lazy regeneration via cache keys, CSS-style fit modes (`imageConfigured`), and tinted SVG icons (`parseSvg`, `svgIcon`, `iconButton`). |
| [`Canvas.hs`](examples/Canvas.hs) | `nano-ui-example-canvas` | Freehand 2D vector drawing with `NanoUI.Path` (`canvasConfigured`, `useDrag2DOn`), pointer capture during drags, and path caching with `canvasContent`. |
| [`Shaders.hs`](examples/Shaders.hs) | `nano-ui-example-shaders` | Interactive gallery of Tier 1 surface fills (`shaderBox`) and Tier 2 subtree effects (`withShader`, fused `composeColor` / `distortThenColor`, separable `gaussianBlur`) built with `nano-ui-shader`. |
| [`Bloom.hs`](examples/Bloom.hs) | `nano-ui-example-bloom` | Spring physics driving a layered rose-curve GPU fragment shader (`nano-ui-shader`) with a 2D vector canvas fallback: sliders write plain `Var`s, `animateTo` springs follow them, and dragging rotates the flower (`useDrag2DOn`). Also serves as the first tab of `nano-ui-sdl-landing` (`examples/sdl/BloomMain.hs` provides its standalone `main`). |
| [`Tasks.hs`](examples/Tasks.hs) | `nano-ui-example-tasks` | Running blocking I/O on worker threads (`useTask`, `useTaskStatus`), key-based cancellation and restarts, error retries, and continuous streams (`useStream`). |
| [`Files.hs`](examples/Files.hs) | `nano-ui-example-files` | Non-blocking native file dialogs (`useFileDialog`), asynchronous file reads (`useTaskStatus`), drag-and-drop file paths, and dynamic window titles (`setWindowTitle`). |
| [`LongList.hs`](examples/LongList.hs) | `nano-ui-example-long-list` | Virtualizing a 100,000-row list with `getScrollMetrics`, top/bottom spacers, and programmatic scrolling (`scrollRectIntoView`). |
| [`LiveChart.hs`](examples/LiveChart.hs) | `nano-ui-example-live-chart` | Real-time line chart built with `nano-ui-diagrams` (`plot`, `PlotCache`), driven by a background `useStream` producer that sleeps at zero CPU when paused. |

Run any example by name:

```sh
cabal run nano-ui-example-hello
cabal run nano-ui-example-state
cabal run nano-ui-example-layout
```

## Building and testing

Building requires GHC 9.10 through 9.14 (development uses 9.14), Cabal, SDL3, SDL3_ttf 3.2+, and `pkg-config`. Inside a repository checkout, `nix develop` supplies the entire toolchain; see [Development Setup](https://goolord.github.io/nano-ui/book/getting-started/development.html#setup) for Windows and manual setup. Executables are gated behind the `sdl` Cabal flag (enabled by default).

Two test suites exercise the demo apps:

```sh
cabal test nano-ui-demo-test      # drives the sampler, notepad, log viewer, and IME checks in hidden SDL windows
cabal test nano-ui-terminal-test  # tests the terminal emulator's ANSI parser and PTY integration (Linux and macOS)
```

## Documentation

- [The nano-ui Book](https://goolord.github.io/nano-ui/book/): Tutorials, widget gallery, interactive styling lab, and architecture reference.
- [Development Setup](https://goolord.github.io/nano-ui/book/getting-started/development.html#setup): Toolchain installation, build commands, and profiling.
