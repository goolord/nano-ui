# Source reduction follow-up

Baseline: `a92ac09`, Windows x86-64, GHC 9.14.1, Cabal's `-O1`.
The baseline build and all nine available test suites passed before editing.
Baseline binaries remain in `dist-newstyle`; candidates build in a separate
directory. No dependency or library code is removed from the source accounting
by moving it into another local package.

## SDL geometry boundary

The accepted implementation retains native coalescing and its full-redraw
path. It replaces the quad-only scalar/AVX2 damage loops with a conservative
triangle check, drops the obsolete headers/flag, and removes redundant Haskell
argument-marshalling helpers. Native readback reproduces the old failure for
independent triangles and three-index tails; both cases pass after the change.

This boundary removes 137 physical / 152 formatter-normalized maintained
library lines, including the replacement C implementation and Haskell callers.

Seven alternating baseline/candidate pairs, 5,000 measured frames per workload:

| Workload | Baseline ms | Candidate ms | Change | Allocated B/frame, both |
| --- | ---: | ---: | ---: | ---: |
| 4,096 offscreen quads, partial damage | 0.066638 | 0.066672 | +0.05% | 561.1 |
| Offscreen quads plus visible quad, partial | 0.133472 | 0.134222 | +0.56% | 560.0 |
| 256 visible quads, partial | 0.069885 | 0.070044 | +0.23% | 560.0 |
| 256 visible quads, full | 0.059406 | 0.059698 | +0.49% | 520.0 |

The extended probe was compiled against the preserved baseline and candidate
package databases with the same source and flags. The baseline retains AVX2;
the candidate requires no architecture-specific flag. The native font effects
suite and demo/notepad/logs self-tests also pass.

An end-to-end SDL audit (seven pairs, 400 frames per workload) has unchanged
allocation apart from tiny runtime/statistics fluctuations. Timing remains
noise-sensitive: idle presentation changes +2.04%, active -0.38%, debug
presentation -1.48%, forced-full replay -0.33%, and hover sweep +2.51%.
Unchanged runFrame-only workloads also move several percent. These results
do not establish a universal sub-1% bound; the focused submission measurements
above establish the narrower cost envelope of the accepted change.

### Rejected variants

- Direct SDL submission without CPU rejection: the offscreen partial workload
  changes 0.059871 to 0.127634 ms (+113.18%), and Haskell allocation increases
  560 to 744 B/frame. Reject.
- Direct submission with a small triangle rejector: microbenchmarks improve,
  but the longer end-to-end probe reports +4.88% forced-full replay. Retain
  coalescing rather than relying on SDL batching to have identical economics.
- An additionally simplified native batch layout/flush path did not establish
  the performance budget (dense-full probe +10.29%). Preserve the original
  layout and full-redraw implementation instead.

The benchmark scripts save all samples and executable hashes. Measurement
files for this boundary are under the session's approved temporary directory,
including `nano-ui-render-preserved-batch.json` and
`nano-ui-sdl-preserved-batch.json`.

## Core fallback and cursor boundary

Use `MaybeT`/`Alternative` for short-circuiting overlay and scroll-target
queries, `asum` for cursor priority, and the existing custom draw-context
constructor for custom cursor callbacks. The new helper imports use the
existing `transformers` dependency. This removes 26 formatter-normalized
library lines. Physical lines increase by 79 because these modules had not
previously been normalized to the repository's formatter configuration.

The hot live/previous clip query was also piloted with `MaybeT`. It erased
the transformer and dictionaries, but generated a different join structure
from the handwritten branch; its exact Core-equivalence check failed.
Retain the original implementation at that particularly hot boundary.

To remove uncertainty about pre-existing build artifacts, a fresh detached
checkout of `a92ac09` was compiled at `-O1` in `nano-ui-loc-baseline` under
the session's temporary directory. Only the ditto checkout path and profiler
sample-length/precision instrumentation differ from that commit. Subsequent
comparisons use this freshly compiled baseline.

Seven paired headless runs against that baseline (`nano-ui-core-fallback-fresh.json`):

| Scene | Baseline MUT seconds | Candidate MUT seconds | Allocated bytes, both |
| --- | ---: | ---: | ---: |
| widgets | 0.581701 | 0.573951 | 592,624,096 |
| canvas | 0.233204 | 0.231805 | 636,939,944 |
| canvas-keyed | 0.091284 | 0.091009 | 120,750,624 |
| textarea | 0.109863 | 0.109625 | 248,726,552 |
| svg | 0.397240 | 0.388935 | 449,985,488 |

The unchanged SVG path also moves in timing: these are regression measurements,
not evidence that every workload sped up. Allocation is exactly unchanged.

## SDL_ttf forwarding boundary

Nine trivial SDL_ttf wrappers now use header-checked `capi` imports. GHC
generates the ABI adaptation (including C `bool`), while line skip/ascent use
the API's integer result types and convert at the existing snapshot boundary.
The memory stream opens through `TTF_OpenFontIO` with autoclose instead of
manually populating SDL properties. Rendering policy, glyph-image conversion,
shaping, fallback ordering and owned stream storage retain their implementations.

This removes 75 maintained library lines, physical and normalized. Native
font-effects and font-search suites pass, including fallback shaping, immutable
snapshots, cache eviction, embedded-font reload, and retained-handle rejection.

## SDL atlas storage boundary

Upload RGBA glyph surfaces directly with their own pitch. SDL's texture lock
and surface-fill APIs initialize/reset the texture, including the white patch
and transparent padding. This removes 27 library lines and the library-owned
2048 x 2048 x 4-byte shadow allocation. SDL/driver staging storage is still
backend-dependent; this is not a claim of 16 MiB less process residency on
every renderer.

Readback tests pass before and after the change, with both software and native
renderers. They cover a padded source pitch, glyph pixels, transparent padding,
the white patch, and clearing the old glyph pixels on reset. Font-effects and
all three SDL app self-tests pass. The SDL benchmark's warm lookup gate reports
0.090 B/lookup against its existing 1 B budget; all six frame/draw cases pass.

Seven paired native atlas runs, each measuring 100 reset-and-1,024-insertion
cycles after five warmups: 2.966593 to 2.886646 ms/cycle (-2.69%), with zero
Haskell bytes/cycle on both sides (`nano-ui-atlas-direct.json`). This exercises
cold uploads and resets rather than only warmed glyph lookups.
