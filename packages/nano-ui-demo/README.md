# nano-ui-demo

Example applications and single-topic tutorials for
[nano-ui](https://github.com/goolord/nano-ui), built on the SDL3 backend
(`nano-ui-sdl`).

The package is split into two parts:

1. **Full applications** that combine multiple features into complete programs
   (a widget and chart showcase, a text editor, a streaming log viewer, and a
   terminal emulator).
2. **Single-topic examples** in [`examples/`](examples/): short, self-contained
   programs that each focus on one concept (`Var` state, layout modifiers,
   forms, keyboard shortcuts, overlays, theming, animations, background tasks,
   and so on).

## Full applications

| Executable | Source | Description |
| --- | --- | --- |
| `nano-ui-sdl-demo` | [`examples/Demo.hs`](examples/Demo.hs), [`lib/DemoData.hs`](lib/DemoData.hs) | Multi-tab showcase of built-in widgets, layout containers, custom 2D vector graphics, animated GIFs, rich text, trees, sortable tables, dynamic tabs, split panes, and `nano-ui-diagrams` charts. Includes a live state sidebar showing current `Var` values across tabs, an `About` modal (`F1`), and a `Debug` window (`F12`) with frame timings, damage statistics, and an interactive layout overlay. |
| `nano-ui-sdl-notepad` | [`examples/Notepad.hs`](examples/Notepad.hs) | Plain-text editor with a menu bar built on `popup`, keyboard shortcuts, unsaved-change tracking in the window title, and asynchronous native open/save file dialogs. |
| `nano-ui-sdl-logs` | [`examples/Logs.hs`](examples/Logs.hs) | Streaming log viewer backed by a bounded ring buffer, with severity filtering, selectable log lines, Select All / Copy All, and auto-scroll that pauses when the user scrolls up. |
| `nano-ui-sdl-terminal` | [`examples/Terminal.hs`](examples/Terminal.hs) | 80×24 terminal emulator attached to `/bin/sh` over a PTY (Linux and macOS only). Supports UTF-8, 16 ANSI colors with bold and inverse attributes, cursor movement, screen erasing, inserting and deleting characters and lines, scrolling, repeated characters, 2,000 lines of scrollback, and inline IME composition at the cursor via `useInputMethod` (`TERM=ansi`). |
| `nano-ui-sdl-profile` | [`examples/Profile.hs`](examples/Profile.hs) | Frame-time and allocation benchmark runner for the demo UI. |
| `nano-ui-demo-svg` | [`examples/DemoSvg.hs`](examples/DemoSvg.hs) | Renders the demo's Plots tab headlessly to an SVG file with `nano-ui-svg`'s `frameToSvg`: widget chrome from the frame's vertex buffer, plots and canvas drawings from their cached draw ops, and text from the raster spans. Run `cabal run nano-ui-demo-svg -- out.svg`. |

Run any application with `cabal run`:

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
cabal run nano-ui-sdl-demo -- --explain      # start with the layout inspector overlay enabled
cabal run nano-ui-sdl-demo -- --continuous   # render continuously without vsync
cabal run nano-ui-sdl-demo -- --fullscreen   # start in fullscreen mode
```

## Examples

Each file below is a standalone executable that depends only on the public
library packages (`nano-ui`, `nano-ui-sdl`, and `nano-ui-diagrams` where
needed), never on internal demo helpers. You can copy any file directly into a
new project as a starting point, and each file's module header explains the
concepts it demonstrates.

They are listed in roughly the order you would read them as a tutorial:

| Source | Executable | Topics covered |
| --- | --- | --- |
| [`Hello.hs`](examples/Hello.hs) | `nano-ui-example-hello` | Minimal application setup, the per-frame execution model, and local state with `useVar`. |
| [`State.hs`](examples/State.hs) | `nano-ui-example-state` | View-local state (`useVar`, `useEnumVar`), application-owned state (`newVar`), record field projection with `OverloadedRecordDot` (`state.field`), conditional widget scopes (`scope`), and keyed list state (`withKey`). |
| [`Layout.hs`](examples/Layout.hs) | `nano-ui-example-layout` | `row`, `column`, and `grid` containers, composable `Layout -> Layout` modifiers, width/height sizing (`Fit`, `fixedW`, `fillW`, `grow`), cross-axis alignment, wrapping, badges, and the interactive layout inspector (`explainLayout`). |
| [`Components.hs`](examples/Components.hs) | `nano-ui-example-components` | Defining reusable custom widgets from built-in primitives, isolating internal widget IDs, exposing `Response` (`'`) and `LayoutModifier` (`With`) variants, and choosing between internal and caller-owned `Var`s. |
| [`Forms.hs`](examples/Forms.hs) | `nano-ui-example-forms` | Keeping form state in a single record `Var`, projecting per-field `Var`s, validating fields after the first submit attempt, disabling buttons with `disabledWhen`, submitting on `Enter` (`respSubmitted`), and resetting to defaults. |
| [`TodoList.hs`](examples/TodoList.hs) | `nano-ui-example-todo-list` | Adding, inline-editing, filtering, and reordering list items while preserving per-item `Var`s (`withElemVars`) and local widget state across list mutations (`withKey`). |
| [`Keyboard.hs`](examples/Keyboard.hs) | `nano-ui-example-keyboard` | Key chords (`shortcut`, `shortcutOnce`, `cmdOrCtrl`), arrow-key navigation, how focused text inputs claim key events automatically, programmatic focus (`requestFocus`, `clearFocus`, `takeEscape`), and menu shortcuts. |
| [`Overlays.hs`](examples/Overlays.hs) | `nano-ui-example-overlays` | Menu bars, filter popovers (`popup`), right-click context menus (`contextMenu`), hover tooltips (`tooltip`, `withTooltip`), modal dialogs (`modal`), draggable/resizable floating windows (`window`), and intercepting window close requests (`wsExitOnCloseRequest`). |
| [`Theming.hs`](examples/Theming.hs) | `nano-ui-example-theming` | Switching between built-in and Base16 themes (`setUiTheme`), following system light/dark mode (`systemAppearance`, `lightDark`), scoped style overrides (`styled`), and reading theme colors for custom drawing (`uiTheme`). |
| [`Animation.hs`](examples/Animation.hs) | `nano-ui-example-animation` | Eased tweens (`Tween`) and spring physics (`Spring`) with `animateTo` and `animateToA`, smooth mid-flight retargeting, animated colors, continuous animations (`pulse`, `keepAnimating`), and idle loop sleep once animations settle. |
| [`Images.hs`](examples/Images.hs) | `nano-ui-example-images` | Leasing procedural RGBA textures in the backend atlas (`useImageRgba`), lazy regeneration via cache keys, content fit modes (`imageConfigured`), and tinted SVG icons (`parseSvg`, `svgIcon`, `iconButton`). |
| [`Canvas.hs`](examples/Canvas.hs) | `nano-ui-example-canvas` | Interactive 2D vector drawing with `NanoUI.Path` (`canvasConfigured`, `useDrag2DOn`), pointer capture for freehand strokes, and path caching via `canvasContent`. |
| [`Tasks.hs`](examples/Tasks.hs) | `nano-ui-example-tasks` | Running blocking IO on background worker threads (`useTask`, `useTaskStatus`), key-driven cancellation and restarts, error handling and retries, and incremental streams (`useStream`). |
| [`Files.hs`](examples/Files.hs) | `nano-ui-example-files` | Non-blocking native open/save file dialogs (`useFileDialog`), loading file contents asynchronously with `useTaskStatus`, drag-and-drop file paths, and dynamic window titles (`setWindowTitleUi`). |
| [`LongList.hs`](examples/LongList.hs) | `nano-ui-example-long-list` | Viewport virtualization for a 100,000-row list using `getScrollMetricsUi`, top/bottom spacer nodes, and programmatic scrolling by content rectangle (`scrollRectIntoViewUi`). |
| [`LiveChart.hs`](examples/LiveChart.hs) | `nano-ui-example-live-chart` | Real-time line chart rendered with `nano-ui-diagrams` (`plot`, `PlotCache`), driven by a background `useStream` producer that sleeps at zero CPU cost when paused. |

Run any example by its executable name:

```sh
cabal run nano-ui-example-hello
cabal run nano-ui-example-state
cabal run nano-ui-example-layout
```

## Building and testing

Requires GHC 9.10 through 9.14 (development uses GHC 9.14), Cabal, SDL3,
SDL3_ttf 3.2+, and `pkg-config`. In a repository checkout, `nix develop`
provides the full toolchain and native dependencies; see the
[development guide](https://github.com/goolord/nano-ui/blob/main/docs/development.md#setup)
for details. All executables are gated behind the package's `sdl` Cabal flag
(enabled by default).

Two test suites exercise the demo applications:

```sh
cabal test nano-ui-demo-test      # runs the widget demo, notepad, log viewer, and IME checks in hidden SDL windows
cabal test nano-ui-terminal-test  # tests the terminal emulator's ANSI parser and PTY integration (Linux and macOS)
```
