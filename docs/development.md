# Development

## Setup

Requires GHC 9.10 through 9.14 and Cabal. Development uses GHC 9.14, which is
provided by `nix develop` along with Cabal, SDL3, SDL3_ttf, and pkg-config. To
verify compatibility with GHC 9.10, use a separate build directory:

```sh
cabal build all -w ghc-9.10.3 --builddir=dist-newstyle-910
```

## Building and testing

```sh
cabal build -j1 all
cabal test -j1 all
```

Run builds with `-j1`. Running multiple optimising GHC processes in parallel
can exhaust memory on a desktop machine; if you want a hard cap,
`GHCRTS=-M4G cabal build -j1 ...` will fail the build instead of swapping.

| Test suite | Covers |
| --- | --- |
| `nano-ui-test` | Widgets, layout, input, focus, damage, and drawing, run headlessly frame by frame |
| `nano-ui-emit-test` | Typed emission, widget adapters, reducers, rebuilds, and queue isolation |
| `text-buffer-spec` | The multi-line text buffer |
| `nano-ui-inspection` | Compiler checks for SIMD writers, typed store slots, animation channels, unboxed commands, and canvas construction |
| `nano-ui-rgfw-test` | RGFW input translation, glyph atlas, and software-rasterised frames; with a display, OpenGL frames and loop wakes |
| `nano-ui-rgfw-bindings-test` | Native event union layouts, key width, modifiers, and constant values; with a display, `stopWaitForEvent` |
| `nano-ui-font-search-test`, `nano-ui-font-effects-test` | SDL font discovery, measurement, and handle lifetimes |
| `nano-ui-render-test` | SDL key/cursor translation and theme events; native SDL readback of partial-damage triangles, clipping, images, window options, and screenshots (also on OpenGL when a display is available) |
| `nano-ui-sdl-pointer-test` | SDL mouse buttons through the SDL event queue |
| `nano-ui-diagrams-test` | Diagram conversion, tessellation, and charts |
| `nano-ui-form-test` | Form scopes, validation, reset, and submission |
| `nano-ui-markdown-test` | Markdown parsing, incremental append vs. full-text parsing (QuickCheck), and drawing |
| `nano-ui-demo-test` | The SDL demo, notepad, log viewer, and IME composition, run in hidden windows |
| `nano-ui-terminal-test` | The terminal demo's escape-sequence parser and PTY |

Run a single suite with `cabal test nano-ui-test --test-show-details=failures`.

The headless suites do not test native window presentation. After modifying a
backend, run its demo. `nano-ui-demo-test` runs the SDL demo (in both buffered
and continuous presentation modes), notepad, and log viewer in hidden SDL
windows:

```sh
cabal test nano-ui-demo-test
```

For the RGFW backend, run `cabal run nano-ui-rgfw-demo`.

`scripts/record-demo.sh [OUT]` re-records the SDL demo video for the README
into `OUT/demo.mp4` (requires `ffmpeg`). It runs the demo in a hidden window
via `--record DIR` (`SdlRecord`), so re-run it after changing any widget shown
in the tour. The video file is not committed to git: upload it to a GitHub
issue or PR comment box and copy the resulting `user-attachments` URL into
`README.md`.

### Flags

`cabal.project` enables the following flags:

- `nano-ui-sdl:sdl` builds the SDL backend. `nano-ui-demo`, `nano-ui-form`, and
  `nano-ui-markdown` have matching `sdl` flags for their SDL executables.

## Repository layout

| Path | Contents |
| --- | --- |
| `packages/nano-ui` | Core library: widgets, layout, input handling, and the draw list |
| `packages/nano-ui-emit` | Typed emitting views, widget adapters, and reducer runners |
| `packages/nano-ui-sdl`, `packages/nano-ui-rgfw` | Window backends |
| `packages/nano-ui-rgfw-bindings` | RGFW bindings and bundled C source |
| `packages/nano-ui-diagrams`, `packages/nano-ui-form` | Charts/diagrams and validated forms |
| `packages/nano-ui-markdown` | Markdown parsing and rendering |
| `packages/nano-ui-demo` | Example applications |
| `scripts/` | Font subsetting (`prune_inter.py`, `prune_cozette.py`) and profiling helpers |

Core modules live under `packages/nano-ui/lib`. Modules outside
`NanoUI/Internal/` form the public API. Everything under `NanoUI/Internal/` is
internal; some internal modules are exposed for backends, tests, and tooling,
but may change without notice. The other packages follow the same convention
(`NanoUI.Sdl.Internal`, `NanoUI.Rgfw.Internal`, `NanoUI.Diagrams.Internal`,
`NanoUI.Form.Internal`, `NanoUI.Markdown.Internal`).

| Path | Contents |
| --- | --- |
| `NanoUI.hs` | Public API re-exports and Haddock overview |
| `NanoUI/Widgets/` | Public widget modules, for qualified imports and symbols not re-exported by `NanoUI` |
| `NanoUI/Internal/Widgets/` | Widget implementations |
| `NanoUI/Internal/Monad.hs`, `NanoUI/Internal/Id.hs` | The `Ui` effect, widget IDs, and keys |
| `NanoUI/Internal/Hooks.hs`, `NanoUI/Internal/Store.hs`, `NanoUI/Internal/Context.hs`, `NanoUI/Internal/Context/` | Widget state and frame context |
| `NanoUI/Internal/Layout/` | Layout tree storage and solver |
| `NanoUI/Internal/Frame.hs`, `NanoUI/Internal/Frame/` | Per-frame input routing, focus, painting, and damage |
| `NanoUI/Internal/Draw.hs`, `NanoUI/Internal/Draw/` | Vertex arenas and draw list generation |
| `NanoUI/Runner.hs` | Shared backend event loop |
| `NanoUI/Testing.hs`, `NanoUI/Testing/` | Headless test harness |

## How a frame works

See [How it works](../README.md#how-it-works) in the README for the per-frame
steps, and [rendering-pipeline.svg](rendering-pipeline.svg) (source:
`rendering-pipeline.d2`) for the full diagram including the backend event loop.

![Rendering pipeline](rendering-pipeline.svg)

## Conventions

- Public widgets follow the conventions documented in `NanoUI`: the return
  value comes first, primed variants (`button'`, `textInput'`) also return a
  `Response`, input widgets take the `Var` to update, and layout options are
  `Layout -> Layout` modifiers.
- Input widgets are implemented as a value-style core
  (`fooCore :: ... -> a -> NanoUI (Response, a)`) wrapped with `bindVar` (or
  `bindVarBy` for a custom equality check) from `NanoUI.Widgets.Custom`. Pass
  `RenderFinal` when the widget renders the updated value on the same pass, and
  `Conservative` when it rendered using the old value before the edit was
  resolved (which triggers another pass on change). Inside the core, sync with
  the widget store using `adoptStoreInt`, `adoptStoreFloat`, or
  `adoptStoreText` and record the returned value with the corresponding
  `recordStore*` function so out-of-band edits by widget ID (such as menus or
  text commands) are preserved.
- Read and write widget store state using the slot functions in
  `NanoUI.Internal.Store` (`findSlot fieldInt 0 key store`,
  `insertSlot fieldPoint key p . deleteSlot fieldInt key`) rather than
  manipulating the underlying maps directly. These inline to direct record
  updates. For state written every frame, use `writeSlots`, which skips both
  the write and the store diff when the value is unchanged.
- View-local state is implemented in `NanoUI.Internal.Var` (`useVar` uses a
  per-scope counter separate from widget IDs). Keyboard handlers should check
  `NanoUI.Internal.Widgets.Behavior.keyboardFocused` first so disabled widgets
  and modals block keyboard input.
- Damage tracking is part of correctness: a state change may require a
  follow-up frame even without new user input. Add a regression test when
  changing damage logic.
- Backend-independent event sequencing lives in `NanoUI.Runner`. Each backend
  provides a `SessionDriver` to translate native events and present frames.
- Keep `NanoUI` focused on the user-facing GUI API, and put backend plumbing in
  `NanoUI.Backend` (input construction, font callbacks, damage types, frame
  metrics, ID derivation).
- Prefer writing code inline; extract a helper function only when it removes
  real duplication or names a non-obvious computation.
- Add tests for observable behaviour. Core integration tests live in
  `packages/nano-ui/test/integration/Cases/` and are registered in `Main.hs`
  and `nano-ui.cabal`. `NanoUI.Testing.Harness` provides helpers for pointer
  frames (`pressAt`, `holdAt`, `releaseAt`, `clickPair`), text-span queries
  (`spanRect`, `spanRectOf`), and warm-up passes; unwrap required rectangles or
  spans with `assertJust` so missing geometry fails the test.
- Format Haskell code with the repository's `fourmolu.yaml`.

## Performance

Packages build with `-O1` by default. To test an `-O2` build, add to
`cabal.project.local`:

```cabal
package nano-ui
  optimization: 2
```

- Reserve `INLINE` pragmas for small functions and helpers inside tight
  per-vertex, per-glyph, or per-node loops. Inlining large functions slows down
  downstream builds for little runtime benefit.
- Measure both runtime and compile time before adding optimisation flags; extra
  inlining and specialisation can bloat code size without speeding up frames.
- Without a `widgetContent` key, a custom widget's draw ops are rebuilt and
  diffed every frame to detect visual changes. That is fine for a few ops, but
  expensive for thousands: pass a content key (built with `contentKey`) that
  hashes all inputs to the drawing so unchanged frames skip both rebuilding ops
  and repainting.
- Compare `bytes allocated` from `cabal run nano-ui-profile -- <scene> +RTS -s`
  using the same compiler, flags, and scene. Allocation differences show
  regressions that wall-clock timing noise can hide, such as thunks retained in
  composed store updates.
- Check per-module compile times with
  `cabal build <target> --ghc-options="-ddump-timings -ddump-to-file"`.

### Profilers and benchmarks

[The abstraction audit](abstraction-audit.md) documents the baseline metrics,
dependency choices, inspection checks, and before/after numbers from past
refactors.

| Command | Runs |
| --- | --- |
| `cabal run nano-ui-profile -- <scene> +RTS -s` | Headless frames. Scenes: `widgets` (default), `canvas` (op-heavy unkeyed custom widget), `canvas-keyed` (the same drawing with a content key), `canvas-paths` (filled and stroked paths rotated slightly each frame to force rebuild and repaint), `canvas-paths-build` (path construction without running a frame), `textarea` (typing into a 100,000-line `textAreaDocument`), `textarea-text` (the same via the `Text` API, which rejoins the document on every edit), `richtext` (200 paragraphs alternating between full-width and width-capped, with one hovered/unhovered each frame; `richtext resize` also resizes the window every frame), and `svg` (icon rasterization) |
| `cabal run nano-ui-sdl-profile` | The SDL demo UI in a hidden window: full demo, individual tabs, widget microbenchmarks, and scaling |
| `cabal run nano-ui-rgfw-profile` | The RGFW demo frame loop on the OpenGL path in a hidden window |
| `cabal run nano-ui-sdl-anim` | Tween and spring animations in an SDL window for visual inspection |
| `cabal run nano-ui-sdl-idle -- <scene> [hidden]` | Idle CPU/wake test window. Scenes: `static`, `focus`, `wake`, `spinner`, `clock`, `type`, and `startup`. Pass `hidden` to keep the window off-screen so pointer events do not wake it |
| `cabal bench nano-ui-id-bench` | Widget ID generation benchmark (fails if a frame of IDs allocates) |
| `cabal bench nano-ui-sdl-bench` | `runFrame` and SDL rendering benchmarks for small, medium, and large UIs |

`scripts/profile/` contains scripts for cost-centre profiling and timing the
SDL demo's event loop.

For code-size tracking, `python scripts/profile/source-budget.py BASE_REF`
counts maintained library Haskell and C lines (excluding demos and vendored
RGFW), after formatting modified Haskell files with Fourmolu on both sides.
`compare-builds.py BASE_BUILD CANDIDATE_BUILD --suite core|sdl|render|atlas|events --output FILE`
runs pre-built executables in alternating order and records raw output, binary
hashes, and allocation/timing medians without rebuilding the baseline (pass
`--executables` to supply binary paths directly). Set `NANO_PROFILE_ITERATIONS`
to increase the SDL profiler's default 40-frame sample count, or
`NANO_RENDER_ITERATIONS` to increase the render probe's default 500-frame
sample count (`cabal test nano-ui-render-test --test-options=--bench`). Pass
`--test-options=--atlas-bench` for glyph atlas reset/upload timing, and
`--test-options=--native` to test readback on the native renderer instead of
the default software renderer.

### Idle cost

When a window is idle, no frames should run. The session loop blocks waiting for
backend events unless an animation, timed wake, or debug overlay refresh is
pending (though OS events may still wake the process).

Set `NANO_LOOP_TRACE=1` to log event-loop activity to stderr roughly once per
second while active. Each line prints the elapsed time, pass count, draw
count, and wake reasons: `A` (animation), `D` (dirty context), `R` (window
redraw request), `T` (timed wake or debug readout refresh), and `-` (input
event). Nothing is printed while the loop is blocked on input.

```sh
NANO_LOOP_TRACE=1 cabal run nano-ui-sdl-idle -- spinner
```

The SDL backend reads three more variables when a session opens.
`NANO_FRAME_TRACE` prints a timing line each time the debug readout refreshes,
so only while a view reads `askSdlDebug`. `NANO_CURSOR_TRACE` logs each cursor
change, and `NANO_FORCE_SCALE=2` replaces the display's pixel density. The
traces take any value.

To keep an application idle when nothing is changing:

- Only call `keepAnimating` for animations that are currently on screen.
  Because `keepAnimating` applies only to the current frame, call it inside the
  animating widget rather than from an unconditionally executed top-level path.
- For scheduled updates (clocks, debounces, key-repeat timers), schedule the
  next frame with `wakeAfter` (or `requestWakeAt` when working with a
  `Context`). Pending wakes reset at the start of each frame and must be
  requested again on each frame that still needs them, so unmounted widgets
  stop scheduling wakes. Never force continuous frames by marking the context
  dirty on every pass, as that creates an unthrottled busy loop.
- Run background tasks with `useTask` or `useTaskStatus` (which wake the loop
  once on completion) or `useStream` (which wakes the loop on updates and
  coalesces bursts that arrive within a single frame). If an external thread
  mutates state read by the UI, wake the loop using the callback returned by
  `askWake`, which schedules a full-window redraw. When waking via
  `ctxWakeLoop` on a `Context`, normal damage tracking determines what is
  redrawn; if the change is not tracked by damage (such as mutating pixels under
  an existing image ID), also call `damageFull`.

When measuring idle CPU usage on Windows, inspect thread cycle time
(`QueryProcessCycleTime`) and context-switch counts over a ~10-second window.
Process-table CPU percentages are sampled on 15.6 ms scheduler ticks and can
miss short wake loops; also note that Task Manager reports percentages across
all logical cores (e.g. 0.4% on a 32-thread machine is 12.8% of one thread).

## Documentation

Hackage displays a package's `README.md` and `CHANGELOG.md` from its sdist
tarball. Every package must have a regular file (not a symlink) at `README.md`
listed under `extra-doc-files` in its `.cabal` file, along with `CHANGELOG.md`
if present. Because the top-level `README.md` is the GitHub landing page and
`packages/nano-ui/README.md` is the Hackage landing page for `nano-ui`, keep
changes to the introduction and initial example synchronised between the two.
Use absolute URLs for repository links in package READMEs so they resolve on
Hackage.

Document exported symbols with Haddock comments. State units, coordinate
spaces, ownership, defaults, and failure modes when they are not obvious from
the type signature. For variant functions (such as `fooWith` or `foo'`), link
to the base function and state how the variant differs. Include a module header
explaining when to import the module.

Build Haddock documentation and QuickJump indices for a single package with:

```sh
cabal haddock -j1 <package> --disable-documentation --haddock-quickjump
```

`--disable-documentation` skips building Haddock for dependencies, though Cabal
may still rebuild dependencies if documentation flags alter the build plan
(and re-exported symbols from dependencies without built Haddock files may
appear undocumented in the coverage report). Check both the Haddock coverage
output and the rendered HTML. Write code comments in the present tense
describing what the code does and why; historical context about previous
implementations belongs in commit messages and changelogs.

## Releasing

Run `cabal check` in each package directory, run `cabal sdist all`, and test
building the generated tarballs in a clean directory to catch files missing
from `extra-source-files` (such as C headers or bundled fonts). Verify that
each archive's `README.md` is a regular file with the expected contents:

```sh
tar -tvzf dist-newstyle/sdist/nano-ui-0.1.0.1.tar.gz
tar -xOf dist-newstyle/sdist/nano-ui-0.1.0.1.tar.gz nano-ui-0.1.0.1/README.md
```

Repeat for all packages using the versions in their `.cabal` files, and check
that links and bundled assets work when extracted outside the repository tree.

Run `python scripts/check-sdist-docs.py` after `cabal sdist` whenever
documentation changes; it verifies every package's README declaration, archive
entry types, changelogs, and user guide against the working tree.
