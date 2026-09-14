# Continuous presentation and rounded hairlines

Measured on Linux, GHC 9.14.1 (`-O2`), SDL 3.4.16, X11/OpenGL,
with the demo's default RTS options (`-N1 -A64m -T -I0`).

## Changes

- Continuous full repaints draw directly to the window when its pixel size
  matches the retained framebuffer's size. This avoids allocating the retain
  texture, switching away from it, and copying it to the window each frame.
  If display content scale differs from actual window pixel density, the
  retained path still performs the required scaling. Window render scale is
  restored before event processing.
- Rounded strokes of width <= 1 share their coincident core rings: three
  vertices and two quads per arc segment instead of four vertices and three
  quads. The removed triangles have zero area. Thicker strokes keep their
  existing geometry.
- Rounded fills pass quadrant indices directly, removing conversion through
  floating-point angles.
- `--selftest --continuous` exercises the direct presentation path.
- The continuous meter reports framebuffer size **after warmup**, because the
  window manager can resize the initial window before measurements start.

## Measurements: presentation/geometry stage

The fresh GHC cost-centre profile identified `pushRoundedStroke` (7.7% of
sampled time), `renderDrawDataPass` (6.2%), and `pushRoundedRectRaw` (4.5%)
among the largest costs. This short, mixed workload includes startup; use it
to locate work, not to estimate continuous FPS.

Real event-loop timing uses `profiles/continuous-meter.c`: 1,000 warmup frames
and 10,000 measured frames, including GC and presentation. Alternating saved
baseline and modified release binaries gave roughly 5–6% more FPS in the
final Controls-tab runs. The final pair, at a settled **716 × 925** pixels:

| Metric | Baseline | Modified |
| --- | ---: | ---: |
| Mean ms/frame | 0.462788 | 0.436638 |
| FPS | 2160.8 | 2290.2 |
| CPU ms/frame | 0.457610 | 0.432035 |
| p50 ms | 0.439978 | 0.414370 |
| p95 ms | 0.562860 | 0.535678 |
| p99 ms | 0.867283 | 0.823500 |

These are desktop measurements, not a universal performance guarantee.
The debug-open pair was essentially flat (1071.6 → 1079.6 FPS); do not claim
a substantial debug-window speedup. Haskell allocation was not improved.

The fixed-size 1280 × 800 profile workload's Controls draw data shrank from
6,390 to 5,870 vertices (**8.1%**) and 17,910 to 15,414 indices (**13.9%**),
with 24 draw commands in both versions.

## Follow-up: cached debug rows and simpler batching

The demo now caches its four formatted debug sections per context, keyed by
the complete SDL snapshot. The backend samples at 4 Hz; between samples the
demo reuses the formatted text instead of calling `printf` for every row on
every frame. The cache holds only the latest snapshot and its rows, and does
not change widget state or invalidate layout by itself.

The internal Haskell/C batch interface also drops five unused arguments:
index-buffer length, texture ID, texture width, texture height, and scale.
Texture lookup now returns only the pointer, eliminating the redundant
dimension conversion and tuple. The associated unused atlas fields and C
batch field were removed. Renderer scaling still happens at target setup.

Debug-open allocation fell from approximately **735 KB to 504 KB/frame**
(31% lower), with exactly the same draw-data counts as the first stage.
Alternating debug-open runs improved throughput by roughly **9–12%** over
the first-stage binary. Final verified binary pair:

| Debug-open metric | First stage | Final |
| --- | ---: | ---: |
| Mean ms/frame | 0.901630 | 0.828404 |
| FPS | 1109.1 | 1207.1 |
| CPU ms/frame | 0.893717 | 0.820732 |
| p50 ms | 0.858838 | 0.778987 |
| p95 ms | 1.134237 | 1.121863 |
| p99 ms | 1.595826 | 1.516347 |

The final Controls pair against the original baseline was **0.484780 →
0.452742 ms/frame**, or **2062.8 → 2208.8 FPS**. All pairs above use the same
settled 716 × 925 X11/OpenGL window. Absolute timings vary with desktop load;
no separate FPS benefit is attributed to removing the unused batch arguments.

The follow-up rebuilt both executables, reran the SDL test suites and full
profile workload, and passed both self-test modes. The self-tests now also
assert that the debug draws counter advances across a backend refresh and
that the new sampled value appears in the visible text. The final renderer
capture was pixel-identical to the first-stage capture.

## Verification

- `cabal build -j1 nano-ui-sdl-demo nano-ui-sdl-profile`
- `cabal test -j1 nano-ui nano-ui-sdl`: all five suites passed.
- `cabal run nano-ui-sdl-demo -- --selftest`: passed.
- `cabal run nano-ui-sdl-demo -- --selftest --continuous`: passed.
- Full SDL profiling workload completed.
- Pre-present RGB captures matched the baseline exactly at 1×, including
  the 1280 × 800 and window-manager-resized layouts.
- A forced 1.5× display-scale / 1× pixel-density capture matched exactly
  through the retained fallback. This synthetic check caught the scaling
  mismatch before the size guard was added; it is not a native HiDPI test.
- The capture probe checked that the window target and unit render scale
  were restored before presentation.

Reproduce throughput with the existing meter (save the baseline executable
before rebuilding):

```sh
cc -O2 -Wall -Wextra -shared -fPIC profiles/continuous-meter.c \
  -o /tmp/opencode/frame-meter.so -ldl
SDL_VIDEODRIVER=x11 SDL_RENDER_DRIVER=opengl \
  LD_PRELOAD=/tmp/opencode/frame-meter.so path/to/nano-ui-sdl-demo --continuous
```
