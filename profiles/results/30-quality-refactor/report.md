# Code-quality refactor: performance check

## Compared versions and method

- Pre-refactor baseline: `479a655`.
- Initial refactor checkpoint: `6d0a5df`.
- Final: the performance fixes accompanying this report.
- Linux x86-64, AMD Ryzen 7 7840U, GHC 9.14.1, workspace `-O2` settings.
- Processes pinned to logical CPU 8; versions run sequentially in rotating
  order. Builds finished before timing. CPU boost remained enabled.
- Stock `nano-ui-sdl-profile`: median of five process runs, each with its
  existing five warmup frames and 40 measured frames per workload.
- Tasty: median of three process runs, `-j1 --stdev 3 --timeout 30s`.
- Additional headless probes: median of five runs, 20 warmup iterations and
  200 measured iterations, `-N1 -A64m -T`. Allocation includes a final GC so
  the last nursery is counted; that final GC is outside the timing interval.
- Both native builds used the existing local SDL ABI-assertion override.

Small timing differences, especially in the stock 40-frame profiler, should
be treated as noise rather than claimed speedups. The allocation differences
were repeatable and were used to isolate the refactor costs.

## Regressions found and corrected

1. **Table allocation:** the 200-row table rose from 557.1 to 610.8 KiB/frame
   at the checkpoint (+9.6%). Forcing the shared encoded-row vector before
   traversing columns restored the single encoding pass. Changing the shared
   keyed-row helper to traverse its three input lists directly removed
   per-cell zip tuples. Final allocation: 557.0 KiB/frame.
2. **Text measurement closures:** a short-lived measurement bundle captured
   separate line/wrap functions for every node. Keeping the font variant and
   host callback as data, with inline dispatch functions, removed that cost.
   The one-label-per-row HUD workload fell from 92.1 to 85.6 KiB/frame
   (baseline: 86.1).
3. **Pure wrapping traversal:** the first shared pure/IO implementation raised
   the multiline HUD block from the checkpoint's 81.3 to 92.2 KiB/frame.
   Retaining `concatMap` for the pure outer traversal, while sharing paragraph,
   word-fitting and truncation policy, removed most of this increase.
4. **Form scope re-entry:** immediate form runners unnecessarily entered the
   same prefix scope twice. They now evaluate and render in one scope; only
   deferred views re-enter it. This halved the extra allocation in the
   100-independent-form probe, from about 128 to 64 KiB/frame.
5. **Hover candidates:** removing the extra distance/candidate pair reduced
   allocation while retaining the standard `minimumBy` traversal and its
   first-candidate tie behavior.

## Final results versus pre-refactor baseline

### Tasty CPU frame and software-renderer benchmarks

| Workload | Baseline | Final | Time change |
| --- | ---: | ---: | ---: |
| `ui/runFrame.small` | 4.090 us | 3.982 us | -2.6% |
| `ui/runFrame.medium` | 180.099 us | 176.028 us | -2.3% |
| `ui/runFrame.large` | 385.785 us | 380.749 us | -1.3% |
| `sdl3/draw.small` (software) | 0.2072 ms | 0.2041 ms | -1.5% |
| `sdl3/draw.medium` (software) | 1.7394 ms | 1.7419 ms | +0.1% |
| `sdl3/draw.large` (software) | 3.2658 ms | 3.2497 ms | -0.5% |

Allocation is effectively unchanged for these six workloads. The warm glyph
lookup gate passed, below its 1 B/lookup budget.

The default-renderer Tasty draw cases failed on the **pre-refactor baseline**
with `SDL_SetRenderTarget(retain) failed`. The comparisons above explicitly
use `SDL_RENDER_DRIVER=software`; they are not GPU timings. The stock profiler
below ran successfully with the default renderer and its native presentation
path.

### Selected stock profiler workloads

| Workload | Baseline ms | Final ms | Baseline KiB | Final KiB |
| --- | ---: | ---: | ---: | ---: |
| Full demo, idle SDL present | 0.553 | 0.553 | 240.5 | 240.6 |
| Full demo, CPU frame | 0.210 | 0.203 | 234.2 | 234.3 |
| Debug window, SDL present | 1.005 | 0.997 | 709.5 | 709.5 |
| Debug window, CPU frame | 0.471 | 0.460 | 700.9 | 701.0 |
| Table, 50 rows x 5 columns | 0.187 | 0.176 | 362.9 | 362.6 |
| Table, 200 rows x 5 columns | 0.253 | 0.245 | 557.1 | 557.0 |
| HUD, one mono label per row | 0.061 | 0.061 | 86.1 | 85.6 |
| HUD, one multiline block per section | 0.056 | 0.053 | 79.7 | 81.5 |

### Additional headless probes

| Workload | Time change | Allocation change |
| --- | ---: | ---: |
| One form, one field | +5.0% | +3.9% |
| One form, 100 fields | -1.3% | +0.1% |
| 100 independent one-field forms | +4.7% | +11.0% |
| 30 wrapping labels, changing width, regular font | -0.4% | -0.5% |
| 30 wrapping labels, changing width, mono font | -1.9% | -2.1% |
| Nearest hover, 500 points | -16.0% | -7.9% |
| Nearest hover, 50,000 points | -38.3% | -12.0% |

The resize probes alternate widths of 400 and 416 to exercise layout and text
wrapping rather than only whole-layout cache hits. Hover probes vary the query
position, disable decimation, and force the returned coordinates.

## Remaining costs and interpretation

- Scoped form ownership and exception-safe prefix restoration still have a
  fixed cost of roughly 650 bytes per form per frame. A 100-field form has
  almost unchanged total allocation, but 100 separate forms rise from
  577.8 to 641.6 KiB/frame and from 0.284 to 0.297 ms/frame. The isolation
  guarantees were retained rather than optimizing away the correctness fix.
- The multiline HUD block retains about 1.8 KiB/frame extra allocation (+2.3%),
  despite a lower median runtime. This is a residual regression, not an
  allocation-neutral result. The uncached wrapping probes improve allocation.
- These results cover the measured workloads and machine, not every backend,
  operating system, font, or application shape.

## Reproduction and artifacts

Build each revision separately, using the same compiler, dependencies, native
libraries and flags. Compare interleaved runs of the resulting executables:

```sh
cabal build nano-ui-sdl-profile nano-ui-sdl-bench
taskset -c 8 cabal run nano-ui-sdl-profile
SDL_RENDER_DRIVER=software taskset -c 8 cabal bench nano-ui-sdl-bench \
  --benchmark-options='-j1 --stdev 3 --timeout 30s --csv results.csv'
```

Raw logs, CSV samples, JSON summaries, comparison script and the additional
`QualityProbe.hs` used for this local check are in
`/tmp/opencode/nano-quality-perf-results/`. Relevant sets are
`initial-*`, `focused-fixed-*`, `software-*`, and `final-profile-summary.json`.
The extracted baseline is `/tmp/opencode/nano-quality-perf-baseline/`.

Verification after the fixes: full workspace build, all seven test suites
(including the 14 compiler inspection checks), and the SDL hidden-window
self-test passed. `git diff --check` passed.
