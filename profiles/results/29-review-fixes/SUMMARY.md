# Review fixes: frame time before allocation counts

Compared with `6d1f596bfad4b04d1f7db20a88c57dbb3da5b145`, the implementation
reviewed after `e73150d1689f1b75b701a336f37a51002e69ea8d`.
Measured on September 12, 2026, Linux x86_64, Ryzen 7 7840U, GHC 9.14.1.

## Changes

- Keep the fused rounded-fill/stroke emitters and widget background painter
  out of line. This avoids copying large geometry loops into many callers.
- Do not capture floating/custom-measured layouts. Ineligible frames invalidate
  the old cache, including when a custom measurement is subsequently removed.
- Stop layout comparison at the first mismatch. Exclude paint-only values and
  leaf colors; restore only geometry and solver-written scroll extents, never
  overwrite the freshly built paint state with cached styles.
- Track the identity of pure font/measurement configurations. At frame entry,
  changing configurations clears shared caches. This also handles alternating
  between two differently configured Contexts derived from the same parent.
- Cache single widget-label placements in local coordinates, including header
  alignment in the key. Runtime widget painting consumes them directly without
  rebuilding translated placement lists. Compatibility span/list APIs remain.
- Apply disabled/modal guards to keyboard navigation, with an early exit on
  frames containing no keyboard input.
- Measure glyph allocation with the thread allocation counter. Preserve the
  cached `Maybe GlyphQuad` with `NOUNPACK`; force benchmark character selection
  before the indirect glyph call to avoid measuring the probe's own thunk.
- Fix the profiler's debug-window click lifecycle, verify that it really opens,
  and close it through its title-bar button when it occludes the toolbar.

## Continuous demo throughput

The actual demo event loop was measured, not just `runFrame`: 1280x800,
`--continuous`, CPU affinity `taskset -c 6`, RTS `-N1 -A64m -T -I0`, 1,000 warmup
presents followed by 10,000 measured presents. Both builds use identical core
and SDL dependency unit IDs and the same project optimization flags.

Each comparison used **before / after / after / before** runs. Values below
combine the two equal-frame-count runs for each version. CPU is process
user+system time, not CPU hardware cycles or GPU time.

| Workload | Before ms/frame | After ms/frame | Before FPS | After FPS | FPS change |
|---|---:|---:|---:|---:|---:|
| X11/OpenGL, debug closed | 0.4702 | 0.4448 | 2,127 | 2,248 | +5.7% |
| X11/OpenGL, debug open | 0.9500 | 0.8977 | 1,053 | 1,114 | +5.8% |
| Wayland/OpenGL, debug closed | 0.6739 | 0.6605 | 1,484 | 1,514 | +2.0% |

CPU time/frame fell by 5.3%, 5.5%, and 1.8%, respectively. These are observations
from one machine, not universal speedup guarantees. Wayland scheduling and
presentation introduced more variation than the X11 runs.

P95 improved in these comparisons; P99 was noisy, particularly under Wayland.
Do not infer a universal "no worse tail" guarantee. Raw per-run percentiles are
in `continuous.csv`.

Executable text segment: **87,089,292 -> 43,107,852 bytes** (~50.5% smaller).
Full-demo `runFrame` allocation in the short profiler: **238.0 -> 245.9 KiB/frame**
(~3.3% more than the reviewed HEAD). This deliberately accepts a small allocation
increase for lower CPU/frame time. The earlier >=70% allocation result was for
synthetic medium/large button grids, not the full demo.

The debug-open profiler now emits roughly 9,535 vertices / 27,417 indices / 30
commands, versus 6,047 / 16,335 / 24 closed (live statistics can change glyph
counts). Old reports with identical open/closed geometry did not measure an
open debug window and must not be used as a debug-open allocation baseline.

## Reproduction

Build the baseline commit in a separate checkout/build directory using matching
compiler, dependencies, optimization flags, fonts, display scale and renderer.
Use executable paths from `cabal list-bin nano-ui-sdl-demo` for each build.

```sh
cabal build nano-ui-sdl-demo nano-ui-sdl-profile nano-ui-sdl-bench
cc -O2 -Wall -Wextra -Werror -shared -fPIC profiles/continuous-meter.c \
  -o /tmp/nano-continuous-meter.so -ldl

env SDL_VIDEODRIVER=x11 SDL_RENDER_DRIVER=opengl \
  LD_PRELOAD=/tmp/nano-continuous-meter.so \
  taskset -c 6 /path/to/nano-ui-sdl-demo --continuous --width 1280 --height 800 \
  +RTS -N1 -A64m -T -I0 -RTS
```

Repeat in alternating order. Set `NANO_DEBUG_OPEN=1` for debug-open runs; use
`SDL_VIDEODRIVER=wayland` for the Wayland comparison. The Linux-only probe exits
the single-window demo after its measured presents and reports mean frame time,
CPU time/frame, FPS, P50/P95/P99, and time inside `SDL_RenderPresent`.

For allocation and workload inspection:

```sh
cabal run nano-ui-sdl-profile -- +RTS -N1 -A64m -T -I0 -RTS
cabal run nano-ui-sdl-bench -- --list-tests +RTS -N1 -A64m -T -RTS
```

The corrected warm-glyph gate reports **0.195 B/lookup**, below its 1 B/lookup
budget. Before the fixes, accurate accounting exposed about 40 B/lookup:
16 B from the unpacked cached result, 24 B from a lazy character-selection
argument in the benchmark, plus fixed probe overhead. The former GC-dependent
RTS snapshots falsely reported zero when no collection occurred.

## Correctness verification

- `cabal test all --test-show-details=failures`: **PASS**.
- New tests cover custom-measure removal, font/measurer replacement and Context
  branching, header alignment, translation at scales 1/1.5/2, preservation of
  new paint values/colors on cache hits, disabled keyboard activation and modal
  keyboard eligibility. Cached/fresh paths compare copied vertex/index bytes
  and commands, not just geometry counts.
- SDL profile: completes with verified debug opening and closing.
- SDL demo `--selftest`: **still fails at `About Close button missing`**. The
  same failure was reproduced on the pre-fix inlining-only review build; it is
  not counted as a passing check or claimed fixed here.
- No idle CPU/wakeup or native/GPU memory bound was measured by the continuous
  probe; no new claims are made about those metrics.
