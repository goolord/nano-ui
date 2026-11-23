# scripts

Run each script from the repository root unless noted otherwise. See [Development Setup](../book/src/getting-started/development.md#profiling) for usage examples of the profiling tools.

- `check-cabal-common.py`: Checks that the `common warnings` stanza is identical across every `packages/*/*.cabal` file (enforced in CI).
- `prune_cozette.py [in.otb] [out.otb]`: Subsets the Cozette bitmap font to the glyphs bundled by `nano-ui-rgfw` as `data/cozette.min.otb` (requires `fonttools`).
- `prune_inter.py path/to/Inter.ttf`: Subsets Inter 4.1 (`extras/ttf/Inter-Regular.ttf`) to printable ASCII and writes `packages/nano-ui-ttf/data/inter.ttf`. With `--web`, generates the browser fallback font `packages/nano-ui-sdl-web/web/fonts/inter-extended.ttf` covering extended Latin, Greek, Cyrillic, punctuation, and common symbols (requires `fonttools`).
- `record-demo.sh [out-dir]`: Drives the SDL demo's scripted tour in a hidden window and encodes `demo.mp4` with `ffmpeg`.
- `profile/alloc-gate.py`: Runs every `nano-ui-profile` scene and fails if allocation exceeds the baseline in `profile/alloc-baseline.json` by more than 5%. Pass `--update` to record new baselines.
- `profile/alloc-baseline.json`: Per-scene allocation baseline for the CI gate, recorded with GHC 9.14.1 on Windows.
- `profile/compare-builds.py baseline candidate --suite ... --output out.json`: Benchmarks two compiled build trees (or two binaries with `--executables`) in alternating rounds and prints median times and allocation deltas.
- `profile/run-profile.ps1`: Builds `nano-ui-profile` with profiling enabled, saves JSON and text profiles into `scripts/profile/`, and prints a summary.
- `profile/run-sdl-profile.ps1`: Same as `run-profile.ps1`, targeting `nano-ui-sdl-profile` at `-O2` on Windows (MSYS2 UCRT64).
- `profile/summarize_prof.py [file.prof]`: Lists the top 40 cost centers by time ticks from a GHC JSON profile (`-pj`) along with megabytes allocated.
- `profile/continuous-meter.c`: Linux `LD_PRELOAD` wrapper that measures frame timings in the SDL demo's `--continuous` mode (build and run commands are in the file header).
- `profile/source-budget.py <git-rev>`: Counts net library lines changed since a git revision, both raw and after normalizing Haskell through `fourmolu` on stdin without modifying files.
- `profile/ttf-text-engine.c`: Standalone experiment evaluating SDL_ttf's renderer text engine, kept for reference only.
