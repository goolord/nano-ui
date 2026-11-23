# Development Setup

Working on nano-ui itself. To use it in an application, see the [Quick Start](quickstart.md).

## Setup

This repository's `nix develop` shell provides GHC 9.14, Cabal, SDL3, SDL3_ttf, pkg-config, mdbook (for `nano-ui-book-test`), and Python 3 (for `scripts/`). Outside it, install the same tools listed under [Quick Start prerequisites](quickstart.md#prerequisites).

## Building and testing

```sh
cabal build -j1 all
cabal test -j1 all
```

> [!WARNING]
> Build with `-j1`. Parallel optimizing GHC processes can exhaust memory. For a hard cap, prefix with `GHCRTS=-M4G`.

### Test suites

| Suite | Covers |
| --- | --- |
| `nano-ui-test` | Widgets, layout, input, focus, damage, and draw lists, frame by frame and headless; seeded QuickCheck runs over random layout trees, damage against repaints, and input streams |
| `text-buffer-spec` | Text buffer and document edits: lines, graphemes, carets, undoable edits, random edit sessions against a plain `Text` (QuickCheck) |
| `style-bits-spec` | The bit fields each node type packs into its style index (`NanoUI.Internal.StyleBits`) do not overlap and fit in 64 bits |
| `nano-ui-inspection` | Hot paths (quad writers, store and focus access, canvas) still optimize as written |
| `nano-ui-emit-test` | Emitting views, adapters, and reducers |
| `nano-ui-svg-test` | `DrawOp`-to-SVG: clips, gradients, escaping, rasterization, whole frames against golden documents in `test/golden` |
| `nano-ui-diagrams-test` | Chart and diagrams-lib rendering |
| `nano-ui-form-test` | Form validation, reset, and submission |
| `nano-ui-markdown-test` | Parsing, incremental append against a full parse (QuickCheck), layout |
| `nano-ui-rgfw-test`, `nano-ui-rgfw-bindings-test` | RGFW input translation, bitmap font, frame delivery |
| `nano-ui-sdl-web-test` | The browser backend's input decoding, draw batching, font loading, file dialogs and debug snapshot, run natively |
| `nano-ui-font-search-test`, `nano-ui-font-effects-test` | SDL font discovery, shaping, caches, handle lifetimes |
| `nano-ui-render-test` | Native SDL readback: partial damage, clipping, screenshots |
| `nano-ui-demo-test`, `nano-ui-terminal-test` | The demos and the terminal in hidden SDL windows |
| `nano-ui-book-test` | This book: no inline Haskell, `#include` anchors, specimen tags, and an `mdbook` build |

Continuous integration (`.github/workflows/ci.yml`) runs the suites that open no window (its `HEADLESS_SUITES` list) on Linux with GHC 9.10 and 9.14 and on Windows with GHC 9.14. On Linux with GHC 9.14 it also runs `nano-ui-render-test`, `nano-ui-font-effects-test`, `nano-ui-demo-test`, `nano-ui-terminal-test`, `nano-ui-rgfw-test` and `nano-ui-rgfw-bindings-test` under Xvfb with Mesa's software GL, one step per suite. Each of those steps has `continue-on-error` until its suite has passed on the runner, so a failure there is reported without failing the job. Run the native suites locally when a change touches rendering, fonts, dialogs or DPI.

One suite, with failure details:

```sh
cabal test nano-ui-test --test-show-details=failures
```

`nano-ui-test`, `nano-ui-rgfw-test`, `nano-ui-sdl-web-test` and `nano-ui-font-effects-test` run their specs through `NanoUI.Testing.Runner`, so their arguments pick specs by name. A spec's full name runs that spec, any other argument runs every spec whose name starts with it, and `--list` prints the names the other arguments pick without running them:

```sh
cabal run -v0 nano-ui:test:nano-ui-test -- --list scroll-
cabal run -v0 nano-ui:test:nano-ui-test -- scroll- page-backdrop
```

A failed check in these suites is counted and the run goes on, so one run reports every failure. In `packages/nano-ui/test/integration/Main.hs`, keep the `Cases.*` imports and the `tests` list in alphabetical order.

After a change meant to alter SVG export, rewrite the golden documents from `packages/nano-ui-svg`:

```sh
NANO_UI_UPDATE_GOLDEN=1 cabal test nano-ui-svg-test
```

## Profiling

The scripts are in `scripts/profile/`; `scripts/README.md` has one line on each.

### Allocation gate

Each scene of `nano-ui-profile` runs a fixed number of frames, so its allocation total is the same on every run with one GHC version. `alloc-gate.py` runs the scenes in `scripts/profile/alloc-baseline.json` and fails when one allocates more than 5% over its baseline:

```sh
python scripts/profile/alloc-gate.py                              # check
python scripts/profile/alloc-gate.py --update                     # accept the new totals
python scripts/profile/alloc-gate.py --exe path/to/nano-ui-profile  # check another binary
```

The baseline is recorded with GHC 9.14.1 on Windows, where CI runs the gate. A change that allocates more on purpose reruns the gate with `--update` and commits the new baseline.

### Comparing two builds

`compare-builds.py` times two trees that are already built, alternating between them for `--runs` rounds (7 by default), and prints the median of each metric with the change in percent. Build the candidate into its own directory with `--builddir`, then:

```sh
python scripts/profile/compare-builds.py dist-newstyle ../candidate/dist-newstyle --suite core --output core.json
```

`--suite` picks the binary: `core` runs the `nano-ui-profile` scenes (time and allocation), `sdl` runs `nano-ui-sdl-profile`, `render` and `atlas` run `nano-ui-render-test`'s benchmarks, and `events` runs `nano-ui-rgfw-bindings-test --bench`. With `--executables`, the two positional arguments are paths to binaries instead of build directories. The JSON output holds every run.

### Cost-centre profiles

`run-profile.ps1` builds `nano-ui-profile` with profiling at the project's default `-O1` and `run-sdl-profile.ps1` builds `nano-ui-sdl-profile` with profiling at `-O2` (Windows with MSYS2 UCRT64 only). Each writes a JSON profile for [speedscope](https://www.speedscope.app/) and a text profile into `scripts/profile/`, then runs `summarize_prof.py`, which prints the 40 cost centres with the most ticks and the megabytes each allocated:

```sh
pwsh scripts/profile/run-profile.ps1
python scripts/profile/summarize_prof.py profile-json.prof
```

`cabal.project` sets `profiling-detail: late`, so cost centres sit on top-level functions after optimisation and do not change what GHC inlines.

### Continuous frame rate on Linux

`continuous-meter.c` is an `LD_PRELOAD` library that counts `SDL_RenderPresent` calls in the SDL demo's `--continuous` mode. After 1,000 warm-up frames it times 10,000 more, prints wall and CPU milliseconds per frame, frames per second and the 50th, 95th and 99th percentile frame times to stderr, and exits the demo. The build and run commands are in its header comment. Compare alternating runs with the same renderer, window size, DPI and RTS options.

### Source line budget

`source-budget.py <git-rev>` counts the net lines of library code (`packages/*/lib` and `cbits`, without `nano-ui-demo`) changed since a revision, both as written and after running each changed Haskell file through `fourmolu` to its fixed point, so a reformat does not count as a saving. It needs `fourmolu` on `PATH`, passes source through it on stdin only, and never rewrites a file.

## Haddocks and Hoogle

Released packages have Haddock pages on Hackage. To build documentation for the current checkout:

```sh
cabal haddock-project --output=haddocks --hoogle
```

The index is `haddocks/index.html`; `--hoogle` also writes Hoogle input files there. One package, with less memory:

```sh
cabal haddock nano-ui --haddock-hoogle
```

Its output is under `dist-newstyle/.../doc/html/nano-ui/`.

To search locally, point `hoogle generate --local=...` at the directory holding the generated `.txt` files. See [Installing Hoogle locally](https://github.com/ndmitchell/hoogle/blob/master/docs/Install.md).

## Book code

The book's snippets are anchored regions of compiled modules: `packages/nano-ui-demo/book/Book/*.hs`, or `packages/nano-ui-svg/app/*Examples.hs` for code drawn to a picture. Include them with an anchored `#include`; `nano-ui-book-test` rejects Haskell written inline.

## Module conventions

The companion packages are listed in [Ecosystem Packages](../packages/index.md). In `packages/nano-ui/lib`, the public API is `NanoUI` and `NanoUI.Widgets.*`; `packages/nano-ui/lib/NanoUI.hs` is the full export list. `NanoUI.Internal.*` is exposed for backends and tests and may change between minor releases.
