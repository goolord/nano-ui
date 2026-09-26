# Development

## Setup

You need GHC 9.10 or newer and Cabal. Development uses GHC 9.14, which
`nix develop` provides along with Cabal, SDL3, SDL3_ttf, and pkg-config. To
check that the packages still build on 9.10, use a separate build directory:

```sh
cabal build all -w ghc-9.10.3 --builddir=dist-newstyle-910
```

`nano-ui-form` depends on ditto 0.5, which `cabal.project` builds from a
checkout at `../ditto`. Clone [ditto](https://github.com/goolord/ditto) next to
this repository before building.

## Building and testing

```sh
cabal build -j1 all
cabal test -j1 all
```

Keep to one job. Several optimising GHC processes at once can exhaust memory on
a desktop machine; if you need a hard limit, `GHCRTS=-M4G cabal build -j1 ...`
fails the build instead of swapping.

| Test suite | Covers |
| --- | --- |
| `nano-ui-test` | Widgets, layout, input, focus, damage, and drawing, run headlessly frame by frame |
| `text-buffer-spec` | The multi-line text buffer |
| `nano-ui-inspection` | Compiler checks for SIMD writers, typed store slots, animation channels, unboxed commands, and canvas construction |
| `nano-ui-rgfw-test` | RGFW input translation, the glyph atlas, and frames drawn by a software rasteriser kept in the test suite; with a display, OpenGL frames and loop wakes |
| `nano-ui-rgfw-bindings-test` | Native event union layouts, key width, modifiers, and constant values; with a display, `stopWaitForEvent` |
| `nano-ui-font-search-test`, `nano-ui-font-effects-test` | SDL font discovery, measurement, and handle lifetimes |
| `nano-ui-render-test` | SDL key and cursor translation and the theme event; native SDL readback of partial-damage triangles and clipping, images, window options, and screenshots, also on OpenGL with a display |
| `nano-ui-sdl-pointer-test` | SDL mouse buttons, through SDL's event queue |
| `nano-ui-diagrams-test` | Diagram conversion, tessellation, and charts |
| `nano-ui-form-test` | Form scopes, validation, reset, and submission |
| `nano-ui-markdown-test` | Markdown parsing, appending against whole-text parsing (QuickCheck), and drawing |
| `nano-ui-demo-test` | The SDL demo, notepad, log viewer, and input-method composition, driven in hidden windows |
| `nano-ui-terminal-test` | The terminal demo's escape-sequence parser and PTY |

Run one suite with `cabal test nano-ui-test --test-show-details=failures`.

The headless suites don't exercise native presentation. After changing a
backend, run its demo. `nano-ui-demo-test` drives the SDL demo (in both
buffered and continuous presentation), notepad, and log viewer through real
SDL windows, hidden:

```sh
cabal test nano-ui-demo-test
```

The RGFW backend has its own demo, `cabal run nano-ui-rgfw-demo`.

`scripts/record-demo.sh [OUT]` re-records the README's tour of the SDL demo
into `OUT/demo.mp4`. It drives the demo in a hidden window (`--record DIR`,
in `SdlRecord`), so rerun it after changing a widget the tour visits. It
needs ffmpeg. The video is not committed: drop it into a GitHub issue or PR
comment box and put the `user-attachments` URL GitHub gives back in
`README.md`.

### Flags

`cabal.project` turns on these flags:

- `nano-ui-sdl:sdl` builds the SDL backend. `nano-ui-demo`, `nano-ui-form` and
  `nano-ui-markdown` have their own `sdl` flag for the executables that need it.

## Repository layout

| Path | Contents |
| --- | --- |
| `packages/nano-ui` | The core: widgets, layout, input handling, and the draw list |
| `packages/nano-ui-sdl`, `packages/nano-ui-rgfw` | Window backends |
| `packages/nano-ui-rgfw-bindings` | RGFW bindings, with the C source |
| `packages/nano-ui-diagrams`, `packages/nano-ui-form` | Charts and diagrams, and forms |
| `packages/nano-ui-markdown` | Markdown parsing and drawing |
| `packages/nano-ui-demo` | Example applications |
| `scripts/` | Font subsetting (`prune_inter.py`, `prune_cozette.py`) and profiling helpers |

The core's modules, under `packages/nano-ui/lib`. A module outside
`NanoUI/Internal/` is public API. Everything else lives under
`NanoUI/Internal/`; some of those modules are exposed for backends, tests and
tools that need more than the API, but they can change at any time. The other
packages follow the same rule with `NanoUI.Sdl.Internal`, `NanoUI.Rgfw.Internal`,
`NanoUI.Diagrams.Internal`, `NanoUI.Form.Internal` and `NanoUI.Markdown.Internal`.

| Path | Contents |
| --- | --- |
| `NanoUI.hs` | The public API and its documentation |
| `NanoUI/Widgets/` | Public widget modules, for qualified imports and names `NanoUI` does not re-export |
| `NanoUI/Internal/Widgets/` | The widget implementations |
| `NanoUI/Emit.hs` | Reducer-style widgets |
| `NanoUI/Internal/Monad.hs`, `NanoUI/Internal/Id.hs` | The `Ui` effect, widget ids, and keys |
| `NanoUI/Internal/Hooks.hs`, `NanoUI/Internal/Store.hs`, `NanoUI/Internal/Context.hs`, `NanoUI/Internal/Context/` | Widget state and the frame context |
| `NanoUI/Internal/Layout/` | Layout storage and the solver |
| `NanoUI/Internal/Frame.hs`, `NanoUI/Internal/Frame/` | Per-frame input, focus, painting, and damage |
| `NanoUI/Internal/Draw.hs`, `NanoUI/Internal/Draw/` | Vertex arenas and the draw list |
| `NanoUI/Runner.hs` | The event loop the backends share |
| `NanoUI/Testing.hs`, `NanoUI/Testing/` | The headless test harness |

## How a frame works

The [README's "How it works" section](../README.md#how-it-works) lists the steps of a frame, and
[rendering-pipeline.svg](rendering-pipeline.svg) (source:
`rendering-pipeline.d2`) draws them with the backend loop around them.

![Rendering pipeline](rendering-pipeline.svg)

## Conventions

- Public widgets follow the rules in the `NanoUI` module documentation: the
  result comes first, a primed variant adds the `Response`, inputs are
  controlled, and layout arguments are modifiers.
- A controlled input adopts the caller's value with `adoptStoreInt`,
  `adoptStoreFloat`, or `adoptStoreText`, and records what it returned with the
  matching `recordStore*`. New inputs should use the same pair, so edits made
  between frames survive a caller that passes the previous result back.
- Widget state in the store is read and written through the slot functions of
  `NanoUI.Internal.Store` (`findSlot fieldInt 0 key store`,
  `insertSlot fieldPoint key p . deleteSlot fieldInt key`), not through the
  maps. They compile to the record code they stand for. A widget that
  publishes state every frame writes it with `writeSlots`, which skips the
  write, and the store diff behind it, when nothing changed.
- Local state goes through `NanoUI.Internal.Hooks`. Keyboard handling checks
  `NanoUI.Internal.Widgets.Behavior.keyboardFocused` first, so disabled widgets and
  modals are respected.
- Damage is part of correctness: a state change can need a follow-up frame
  without any new input. Add a test when you change it.
- Backend-independent event sequencing lives in `NanoUI.Runner`. A backend
  supplies a `SessionDriver` for event translation and presentation.
- `NanoUI` is the view API and `NanoUI.Backend` is what a backend is built
  from. A name a view never uses belongs in `NanoUI.Backend`: input
  construction, font callbacks, damage, frame metrics, id derivation. Adding
  one to `NanoUI` instead is what makes the root module hard to read.
- Write code inline. Add a function when it removes real duplication or names a
  computation that isn't obvious.
- Add tests for observable behaviour. Core cases live in
  `packages/nano-ui/test/integration/Cases/` and are registered in `Main.hs`
  and the package's Cabal file. `NanoUI.Testing.Harness` builds pointer
  frames (`pressAt`, `holdAt`, `releaseAt`, `clickPair`), finds spans
  (`spanRect`, `spanRectOf`) and warms views up; a test that needs a rect or
  a span to go on takes it with `assertJust`, which counts a failure when it
  is missing.
- Format Haskell with the repository's `fourmolu.yaml`.

## Performance

Packages build at Cabal's default `-O1`. To compare an optimised build, add this to
`cabal.project.local`:

```cabal
package nano-ui
  optimization: 2
```

- Keep `INLINE` for small bodies and helpers inside per-vertex, per-glyph, or
  per-node loops. A large inlined body slows every importer's build for little
  runtime gain.
- Measure both runtime and compile time before adding optimisation flags.
  More inlining and specialisation can increase code size without making a
  frame faster.
- A custom widget without a `widgetContent` key has its ops rebuilt and
  compared every frame, since only building them shows whether what it draws
  changed. That is right for a handful of ops and wasteful for thousands: give
  an op-heavy drawing a key covering everything it reads, and an unchanged key
  skips the rebuild and the repaint. `contentKey` hashes numbers into one.
- Compare `bytes allocated` from `cabal run nano-ui-profile -- <scene> +RTS -s`
  with the same compiler, flags, and scene. Allocation changes can reveal work
  that timing noise hides, including thunks retained by composed store writes.
- Compare compile times per module with
  `cabal build <target> --ghc-options="-ddump-timings -ddump-to-file"`.

### Profilers and benchmarks

[The abstraction audit](abstraction-audit.md) records the baseline, dependency
decisions, compiler checks, and before/after measurements for the refactors.

| Command | Runs |
| --- | --- |
| `cabal run nano-ui-profile -- <scene> +RTS -s` | Headless frames. Scenes: `widgets` (the default), `canvas` (an op-heavy unkeyed custom widget), `canvas-keyed` (the same drawing with a content key), `canvas-paths` (filled and stroked paths, turned a little every frame so they are built and painted again), `canvas-paths-build` (the same paths built without a frame), `textarea` (typing into a 100,000-line `textAreaDocument`), `textarea-text` (the same through the `Text` API, which joins the document on every edit), and `svg` (icon rasterization) |
| `cabal run nano-ui-sdl-profile` | The SDL demo's UI in a hidden window: the full demo, each tab, widget microbenchmarks, and scaling |
| `cabal run nano-ui-rgfw-profile` | The RGFW demo's frame loop on the OpenGL path in a hidden window |
| `cabal run nano-ui-sdl-anim` | Tween and spring animations in an SDL window, for checking animation pacing by eye |
| `cabal run nano-ui-sdl-idle -- <scene> [hidden]` | A window that should cost nothing while it is left alone. Scenes: `static`, `focus`, `wake`, `spinner`, `clock`, `type`, and `startup`. `hidden` keeps it off screen, where no pointer can disturb it |
| `cabal bench nano-ui-id-bench` | Widget id generation; fails if a frame of ids allocates |
| `cabal bench nano-ui-sdl-bench` | `runFrame` and SDL drawing for small, medium, and large UIs |

`scripts/profile/` has helpers for cost-centre profiles and for timing the SDL
demo's real event loop.

For source-reduction work, `python scripts/profile/source-budget.py BASE_REF`
counts all maintained library Haskell and C, excluding demos and vendored RGFW.
Changed Haskell is formatted to the same Fourmolu fixed point on both sides.
`compare-builds.py BASE_BUILD CANDIDATE_BUILD --suite core|sdl|render|atlas|events --output FILE`
runs already-built executables in alternating order and records raw output,
executable hashes, allocation and timing medians. It never rebuilds the baseline.
Use `--executables` to supply executable paths instead of build directories.
`NANO_PROFILE_ITERATIONS` lengthens the SDL profiler's default 40-frame samples;
`NANO_RENDER_ITERATIONS` lengthens the render probe's default 500-frame samples.
The latter runs with `cabal test nano-ui-render-test --test-options=--bench`.
Use `--test-options=--atlas-bench` for atlas reset/upload timing and
`--test-options=--native` for readback on the selected native renderer instead
of the software/dummy renderer used by the default test suite.

### Idle cost

An idle view should not schedule frames. The session loop blocks in the
backend's event wait unless an animation, timed wake, or debug readout needs a
frame. OS events and runtime activity can still wake the process.

Set `NANO_LOOP_TRACE` to see what the loop is doing. About once a second while
it runs, it prints to stderr the time covered, how many passes it made, how
many drew, and why: `A` an animation, `D` a dirty context, `R` a window redraw
request, `T` a timed wake or a debug readout refresh, and `-` for a pass
caused by input. An idle window prints nothing, and the first line after a
quiet spell covers all of it.

```sh
NANO_LOOP_TRACE=1 cabal run nano-ui-sdl-idle -- spinner
```

Three rules keep a view idle:

- Nothing may run frames by itself except an animation that is on screen.
  `keepAnimating` lasts as long as it keeps being called, so call it from the
  widget it animates and not from somewhere that is always built.
- A view that changes on a schedule (a clock, a debounce, a held button's
  repeat) asks for its next frame with `wakeAfter`, or `requestWakeAt` from
  code holding a `Context`. Each frame starts with no wake pending and the
  widgets still built ask again, so one that is gone stops costing anything.
  Do not mark the context dirty every frame to get there: a dirty context is
  redrawn at once, which is a busy loop.
- Background work goes in `useTask` or `useTaskStatus`, which wake the loop
  once, when the job finishes, or in `useStream`, which wakes it on each
  update and costs one frame for a burst of them. Any other thread that
  changes what the view reads wakes the loop with the action `askWake`
  returns. Each wake runs one frame, which
  repaints the whole window. Code holding a `Context` can wake through
  `ctxWakeLoop` instead: that frame's damage decides what is presented, so
  waking for a change that is not on screen is cheap, and a change damage
  cannot see, such as new pixels under a registered image id, needs
  `damageFull`.

To measure a process, read its cycle time (`QueryProcessCycleTime` on
Windows) and its threads' context switch counts over ten seconds or so. CPU
time from the process table is counted in scheduler ticks of 15.6 ms and
misses a loop of short wakes almost entirely. A percentage in Task Manager is
of the whole machine, so 0.4% on 32 logical processors is an eighth of a
core.

## Documentation

Hackage can show a package's `README.md` and `CHANGELOG.md` from its source
distribution. Every package has a regular `README.md` listed under
`extra-doc-files`; packages with a changelog list that too. Keep these files
inside the package directory. A symlink outside it depends on the checkout,
and a checkout without symlink support may contain only the target path. The
repository's top-level `README.md` is the GitHub front page, and
`packages/nano-ui/README.md` is the core's page on Hackage, so a change to the
introduction or the first example belongs in both. Links in a package README
should use absolute URLs for repository content so they work on Hackage too.

Document exported names with Haddock comments. Explain units, coordinate
spaces, ownership, defaults, and failure cases where the type does not tell
the reader. For a variant, link to the base operation and describe the
difference. A module header should explain when to use the module.

Build a package's documentation and search index with:

```sh
cabal haddock -j1 <package> --disable-documentation --haddock-quickjump
```

This omits dependency documentation. Cabal can still rebuild dependencies
when documentation flags change the build plan. The coverage report lists
undocumented exports; re-exports may lack documentation if the dependency's
Haddock files are unavailable. Check the rendered output too: coverage does
not detect incorrect prose or broken examples.

Comments say what the code does and why, in the present tense. What the code
used to do belongs in the commit message and the changelog.

## Releasing

Run `cabal check` in each package directory, then `cabal sdist all`, and build
the archives from a clean directory. That catches files missing from
`extra-source-files`, such as C headers and bundled fonts. Check that each
archive's `README.md` is a regular file with the expected content. For example:

```sh
tar -tvzf dist-newstyle/sdist/nano-ui-0.1.0.1.tar.gz
tar -xOf dist-newstyle/sdist/nano-ui-0.1.0.1.tar.gz nano-ui-0.1.0.1/README.md
```

Repeat for all seven packages, using the versions in their Cabal files.
Check package-page links and bundled assets from the extracted archives,
where paths outside the package are unavailable.

`python scripts/check-sdist-docs.py` checks every package's README declaration,
archive member types, and all listed documents against the working copy,
including changelogs and the user guide. Run it after `cabal sdist` whenever these
documents change.
