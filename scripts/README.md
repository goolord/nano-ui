# scripts

Run each from the repository root unless its line says otherwise.
[Development Setup](../book/src/getting-started/development.md#profiling)
shows the profiling ones in use.

- `check-cabal-common.py`: fails when the `common warnings` stanzas of the `packages/*/*.cabal` files differ. CI runs it.
- `prune_cozette.py [in.otb] [out.otb]`: strips the Cozette bitmap font down to the glyphs `nano-ui-rgfw` bundles as `data/cozette.min.otb`. Needs `fonttools`.
- `prune_inter.py path/to/Inter.ttf`: subsets Inter to printable ASCII and writes `packages/nano-ui-sdl/data/inter.ttf`. Needs `fonttools`.
- `record-demo.sh [out-dir]`: records the SDL demo's scripted tour in a hidden window and encodes `demo.mp4` with ffmpeg.
- `profile/alloc-gate.py`: runs the `nano-ui-profile` scenes and fails when one allocates more than its baseline in `profile/alloc-baseline.json`; `--update` rewrites the baseline.
- `profile/alloc-baseline.json`: the per-scene allocation totals the gate compares against, recorded with GHC 9.14.1 on Windows.
- `profile/compare-builds.py baseline candidate --suite ... --output out.json`: runs two already-built trees (or two executables with `--executables`) alternately and prints median time and allocation per scene.
- `profile/run-profile.ps1`: builds `nano-ui-profile` with profiling, writes JSON and text profiles into `scripts/profile/`, and summarises them.
- `profile/run-sdl-profile.ps1`: the same for `nano-ui-sdl-profile` at `-O2`, on Windows with MSYS2 UCRT64.
- `profile/summarize_prof.py [file.prof]`: prints the 40 cost centres with the most time ticks in a JSON profile (`-pj`) in `scripts/profile/`, with the megabytes each allocated.
- `profile/continuous-meter.c`: an `LD_PRELOAD` library for Linux that times the SDL demo's frames in `--continuous` mode; build and run lines are in its header.
- `profile/source-budget.py <git-rev>`: counts net library lines changed since a revision, normalising Haskell through `fourmolu` on stdin. It never rewrites a file.
- `profile/ttf-text-engine.c`: a one-off feasibility probe of SDL_ttf's renderer text engine, kept for reference. Nothing builds or runs it.
