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
