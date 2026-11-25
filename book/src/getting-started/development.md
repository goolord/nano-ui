# Development Setup

This page covers building, testing, and profiling `nano-ui` itself. If you are building an application with the library, start at the [Quick Start](quickstart.md).

## Setup

The repository's `nix develop` shell provides GHC 9.14, Cabal, SDL3, SDL3_ttf, `pkg-config`, `mdbook` (used by `nano-ui-book-test`), and Python 3 (used by `scripts/`). Outside Nix, install the same dependencies listed under [Quick Start prerequisites](quickstart.md#prerequisites).

## Building and testing

```sh
cabal build -j1 all
cabal test -j1 all
```

> [!WARNING]
> Always pass `-j1` when building the whole workspace. Parallel optimizing GHC jobs can quickly exhaust system RAM. If you want a hard memory ceiling, prefix the command with `GHCRTS=-M4G`.

### Test suites

| Suite | What it tests |
| --- | --- |
| `nano-ui-test` | Headless frame-by-frame tests for widgets, layout, input, focus, damage, and draw lists, plus seeded QuickCheck properties over random layout trees, damage diffs, and input streams. |
| `text-buffer-spec` | Text buffers and documents: lines, grapheme clusters, carets, undo history, and QuickCheck random edit sessions compared against plain `Text`. |
| `style-bits-spec` | Verifies that the packed bit fields each node type stores in `NanoUI.Internal.StyleBits` never overlap and fit within 64 bits. |
| `nano-ui-inspection` | Checks with `inspection-testing` that hot paths (quad writers, store and focus lookups, canvas helpers) compile without unexpected dictionaries or allocations. |
| `nano-ui-emit-test` | Typed emitting views, widget adapters, and message reducers. |
| `nano-ui-svg-test` | `DrawOp`-to-SVG export (clips, gradients, XML escaping, rasterization) and full-frame comparisons against golden SVGs in `test/golden`. |
| `nano-ui-diagrams-test` | Chart rendering and `diagrams-lib` conversion. |
| `nano-ui-form-test` | Form validation, submission lifecycles, and resets. |
| `nano-ui-markdown-test` | CommonMark parsing, layout, and QuickCheck equivalence between incremental appends and full parses. |
| `nano-ui-shader-test` | Typed fragment shader EDSL compilation, algebraic fusion, std140 uniform packing, SPIR-V, GLSL, MSL, and CPU evaluator parity. |
| `nano-ui-rgfw-test`, `nano-ui-rgfw-bindings-test` | RGFW input translation, bitmap font metrics, and frame delivery. |
| `nano-ui-sdl-web-test` | Browser backend input decoding, draw batching, font loading, file dialogs, and debug snapshots (run natively against a scripted host). |
| `nano-ui-font-search-test`, `nano-ui-font-effects-test` | SDL font discovery, text shaping, line caches, and handle lifetimes. |
| `nano-ui-render-test` | Native SDL readback checks for partial damage, clipping, and screenshots. |
| `nano-ui-demo-test`, `nano-ui-terminal-test` | End-to-end tests of the demo apps and terminal emulator in hidden SDL windows. |
| `nano-ui-book-test` | Validates this book: rejects inline Haskell, checks `#include` anchors and specimen tags, and runs `mdbook build`. |

CI (`.github/workflows/ci.yml`) runs every headless suite (`HEADLESS_SUITES`) on Linux with GHC 9.10 and 9.14, and on Windows with GHC 9.14. On Linux 9.14 it also runs the native window suites (`nano-ui-render-test`, `nano-ui-font-effects-test`, `nano-ui-demo-test`, `nano-ui-terminal-test`, `nano-ui-rgfw-test`, and `nano-ui-rgfw-bindings-test`) under Xvfb with Mesa's software GL. Those steps currently use `continue-on-error` so failures are visible without blocking PRs while runner stability is verified. Whenever you change rendering, fonts, file dialogs, or DPI scaling, run the native suites locally.

To run a single suite and see failure details immediately:

```sh
cabal test nano-ui-test --test-show-details=failures
```

`nano-ui-test`, `nano-ui-rgfw-test`, `nano-ui-sdl-web-test`, and `nano-ui-font-effects-test` all use `NanoUI.Testing.Runner`, which lets you filter specs by name or prefix. Passing `--list` shows what your filters match without running anything:

```sh
cabal run -v0 nano-ui:test:nano-ui-test -- --list scroll-
cabal run -v0 nano-ui:test:nano-ui-test -- scroll- page-backdrop
```

Failed assertions are tallied while the suite finishes, so a single run surfaces every failure at once. In `packages/nano-ui/test/integration/Main.hs`, keep the `Cases.*` imports and the `tests` list sorted alphabetically.

When an intentional change alters SVG output, regenerate the golden files from `packages/nano-ui-svg`:

```sh
NANO_UI_UPDATE_GOLDEN=1 cabal test nano-ui-svg-test
```

## Profiling

Profiling helpers live in `scripts/profile/`, with a one-line summary of each in `scripts/README.md`.

### Allocation gate

Each scene in `nano-ui-profile` runs a fixed number of frames, making total bytes allocated deterministic for a given GHC version. `alloc-gate.py` runs every scene recorded in `scripts/profile/alloc-baseline.json` and fails if any scene allocates more than 5% above baseline:

```sh
python scripts/profile/alloc-gate.py                              # verify against baseline
python scripts/profile/alloc-gate.py --update                     # update baseline totals
python scripts/profile/alloc-gate.py --exe path/to/nano-ui-profile  # test a specific binary
```

The baseline is recorded on Windows with GHC 9.14.1, matching the CI gate. If a change intentionally increases allocation, rerun the script with `--update` and commit the updated JSON file.

### Comparing two builds

`compare-builds.py` benchmarks two already-compiled build trees by alternating runs between them (`--runs 7` by default) and reporting median times and percentage deltas. Compile your candidate branch into a separate `--builddir`, then run:

```sh
python scripts/profile/compare-builds.py dist-newstyle ../candidate/dist-newstyle --suite core --output core.json
```

Use `--suite` to select the benchmark binary: `core` runs `nano-ui-profile` (time and allocations), `sdl` runs `nano-ui-sdl-profile`, `render` and `atlas` run benchmarks inside `nano-ui-render-test`, and `events` runs `nano-ui-rgfw-bindings-test --bench`. Pass `--executables` if you want to give binary paths directly instead of build directories. Every individual measurement is saved to the JSON output file.

### Cost-center profiles

On Windows (with MSYS2 UCRT64), `run-profile.ps1` builds `nano-ui-profile` with profiling at `-O1`, and `run-sdl-profile.ps1` builds `nano-ui-sdl-profile` with profiling at `-O2`. Both write a text profile and a JSON profile for [speedscope](https://www.speedscope.app/) into `scripts/profile/`, then invoke `summarize_prof.py` to list the top 40 cost centers by time ticks alongside their megabytes allocated:

```sh
pwsh scripts/profile/run-profile.ps1
python scripts/profile/summarize_prof.py profile-json.prof
```

`cabal.project` sets `profiling-detail: late`, placing cost centers on top-level functions after optimization so profiling does not distort GHC's inlining decisions.

### Continuous frame rate on Linux

`continuous-meter.c` is an `LD_PRELOAD` helper for timing `SDL_RenderPresent` calls in the SDL demo's `--continuous` mode. It skips 1,000 warm-up frames, times the next 10,000, prints wall time, CPU time, FPS, and p50/p95/p99 frame times to stderr, and exits. Build and run commands are documented at the top of the C file. For fair comparisons, alternate runs using the same renderer, window size, DPI, and RTS flags.

### Source line budget

`source-budget.py <git-rev>` reports the net lines of library code changed since a given commit (`packages/*/lib` and `cbits`, excluding `nano-ui-demo`). It counts both raw lines and lines after formatting each modified Haskell file through `fourmolu` on stdin, so pure reformatting does not masquerade as code reduction. It requires `fourmolu` on `PATH` and never modifies files on disk.

## Haddocks and Hoogle

Released packages have Haddock documentation on Hackage. To generate docs for the whole checkout locally:

```sh
cabal haddock-project --output=haddocks --hoogle
```

Open `haddocks/index.html` in a browser; `--hoogle` also writes Hoogle database files into the same directory. To build documentation for just one package with less memory:

```sh
cabal haddock nano-ui --haddock-hoogle
```

That writes HTML under `dist-newstyle/.../doc/html/nano-ui/`. To build a local Hoogle index from the generated `.txt` files, run `hoogle generate --local=...` (see [Installing Hoogle locally](https://github.com/ndmitchell/hoogle/blob/master/docs/Install.md)).

## Book code

Every Haskell snippet in this book comes from an anchored region of a compiled module: either `packages/nano-ui-demo/book/Book/*.hs` for regular chapters, or `packages/nano-ui-svg/app/*Examples.hs` for snippets paired with rendered diagrams. Include them with an anchored `#include` directive; `nano-ui-book-test` fails if any Haskell is written inline in a Markdown file.

## Module conventions

See [Ecosystem Packages](../packages/index.md) for the companion packages. In `packages/nano-ui/lib`, `NanoUI` (`packages/nano-ui/lib/NanoUI.hs`) re-exports the core view API, while specialized public modules cover focused subsystems: `NanoUI.Themes`, `NanoUI.Chrome`, `NanoUI.Path`, `NanoUI.Shortcut`, `NanoUI.Svg`, `NanoUI.Shader.Raw`, `NanoUI.Effectful`, `NanoUI.Widgets.PaneGrid`, `NanoUI.Widgets.TextDocument`, `NanoUI.Widgets.Custom`, `NanoUI.Testing` / `NanoUI.Testing.Sim`, and `NanoUI.Backend` / `NanoUI.Runner`. Modules under `NanoUI.Internal.*` are exposed for companion packages and tests and may change without notice between minor releases.
