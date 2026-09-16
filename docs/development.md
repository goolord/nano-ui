# Development

## Setup

You need GHC 9.14 and Cabal. `nix develop` provides both, along with SDL3,
SDL3_ttf, and pkg-config.

`nano-ui-form` depends on ditto 0.5, which `cabal.project` builds from a
checkout at `../ditto`.

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
| `nano-ui-inspection` | Compile-time checks that the vertex writers inline without dictionaries or tuples |
| `nano-ui-rgfw-test` | RGFW input translation, the software rasteriser, and the glyph atlas |
| `nano-ui-font-search-test`, `nano-ui-font-effects-test` | SDL font discovery, measurement, and handle lifetimes |
| `nano-ui-diagrams-test` | Diagram conversion, tessellation, and charts |
| `nano-ui-form-test` | Form scopes, validation, reset, and submission |
| `nano-ui-terminal-test` | The terminal demo's escape-sequence parser and PTY |

Run one suite with `cabal test nano-ui-test --test-show-details=failures`.

The headless suites don't exercise native presentation. After changing a
backend, run its demo. The SDL demo also has a self-test that renders into a
hidden window:

```sh
cabal run nano-ui-sdl-demo -- --selftest
```

### Flags

- `nano-ui-sdl:sdl` builds the SDL backend. `cabal.project` turns it on.
- `nano-ui-sdl:simd` compiles the draw-batch culler with AVX2. It only applies
  on x86-64, and the resulting binary needs an AVX2 CPU.

## Repository layout

| Path | Contents |
| --- | --- |
| `packages/nano-ui/lib/NanoUI.hs` | The public API and its documentation |
| `NanoUI/Widgets/` | One module per widget family |
| `NanoUI/Emit.hs` | Reducer-style widgets |
| `NanoUI/Monad.hs`, `NanoUI/Id.hs` | The `Ui` effect, widget ids, and keys |
| `NanoUI/Hooks.hs`, `NanoUI/Store.hs`, `NanoUI/Context/` | Widget state |
| `NanoUI/Layout/` | Layout storage and the solver |
| `NanoUI/Frame.hs`, `NanoUI/Frame/` | Per-frame input, focus, painting, and damage |
| `NanoUI/Draw/` | Vertex arenas and the draw list |
| `NanoUI/Runner.hs` | The event loop the backends share |
| `NanoUI/Testing.hs` | The headless test harness |
| `packages/nano-ui-sdl`, `packages/nano-ui-rgfw` | Window backends |
| `scripts/` | Font subsetting, a Windows RGFW build, and profiling helpers |

Paths in the first column after the first row are under `packages/nano-ui/lib`.

## How a frame works

![Rendering pipeline](rendering-pipeline.svg)

The backend waits for input and runs a frame only when something needs
redrawing. A frame resets the arenas and runs the view, where widgets add
layout nodes and write to the widget store. Layout is then solved, pointer and
keyboard input is resolved against the new geometry, widgets are painted into
vertex arenas, and damage is computed against the previous frame. The backend
presents the resulting draw list.

## Conventions

- Public widgets follow the rules in the `NanoUI` module documentation: the
  result comes first, a primed variant adds the `Response`, inputs are
  controlled, and layout arguments are modifiers.
- A controlled input adopts the caller's value with `adoptStoreInt`,
  `adoptStoreFloat`, or `adoptStoreText`, and records what it returned with the
  matching `recordStore*`. New inputs should use the same pair, so edits made
  between frames survive a caller that passes the previous result back.
- Local state goes through `NanoUI.Hooks`. Keyboard handling checks
  `Widgets.Behavior.keyboardFocused` first, so disabled widgets and modals are
  respected.
- Damage is part of correctness: a state change can need a follow-up frame
  without any new input. Add a test when you change it.
- Backend-independent event sequencing lives in `NanoUI.Runner`. A backend
  supplies a `SessionDriver` for event translation and presentation.
- Write code inline. Add a function when it removes real duplication or names a
  computation that isn't obvious.
- Add tests for observable behaviour. Core cases live in
  `packages/nano-ui/test/integration/Cases/` and are registered in `Main.hs`
  and the package's Cabal file.
- Format Haskell with the repository's `fourmolu.yaml`.

## Performance

Packages build at Cabal's default `-O1`. The core compiles in under a minute
that way, against more than four minutes at `-O2`, and the headless profiler
runs slightly faster. To profile a release build, add this to
`cabal.project.local`:

```cabal
package nano-ui
  optimization: 2
```

- Keep `INLINE` for small bodies and helpers inside per-vertex, per-glyph, or
  per-node loops. A large inlined body slows every importer's build for little
  runtime gain.
- `-fspecialise-aggressively`, `-flate-specialise`, `-fmax-worker-args`, and
  `-funbox-strict-fields` made the core slower at runtime as well as slower to
  build. Leave them off.
- A custom widget without a `widgetContent` key has its ops rebuilt and
  compared every frame, since only building them shows whether what it draws
  changed. That is right for a handful of ops and wasteful for thousands: give
  an op-heavy drawing a key covering everything it reads, and an unchanged key
  skips the rebuild and the repaint. `contentKey` hashes numbers into one.
- `cabal run nano-ui-profile -- +RTS -s` runs frames headlessly. It takes a
  scene: `widgets` (the default), `canvas` for an op-heavy unkeyed custom
  widget, and `canvas-keyed` for the same drawing with a content key.
  `scripts/profile/` has helpers for cost-centre profiles and for timing the
  SDL demo's real event loop.
- Compare compile times per module with
  `cabal build <target> --ghc-options="-ddump-timings -ddump-to-file"`.

## Releasing

Run `cabal check` in each package directory, then `cabal sdist all`, and build
the archives from a clean directory. That catches files missing from
`extra-source-files`, such as C headers and bundled fonts.
