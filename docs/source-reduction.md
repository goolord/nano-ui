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

## RGFW binding boundary

`RGFW.Raw.hsc` derives event offsets, storage size and constants from the
bundled C header. It removes the parallel C event getters while preserving
their Haskell signatures. RGFW physical keys are one byte (the old getter
widened them to 32 bits); the generated getter explicitly preserves this
conversion instead of reading neighbouring repeat/modifier bytes. Key/button
event construction shares press/release handling, and independent coordinate
reads use applicative construction.

The boundary removes 14 physical / 31 normalized library lines. The `.hs` to
`.hsc` migration is counted raw on both sides: it receives no artificial
formatter savings from changing to a format Fourmolu cannot parse. There is
no new runtime dependency; `hsc2hs` is declared as a build tool.

An independent C fixture writes each relevant union member, including a key
with nonzero neighbouring bytes. All native ABI and existing RGFW tests pass,
and the native profile completes 500 frames. Seven paired 10-million-read
probes change 1.622720 to 0.623470 ns/key read (-61.58%), with 16 B/read on
both sides (`nano-ui-events-hsc.json`). These are accessor measurements, not
a claim that rendering has accelerated by the same proportion.

## Larger backend candidates: acceptance not established

### SDL_ttf renderer text engine

The retained renderer-engine probe in `scripts/profile/ttf-text-engine.c`
uses the bundled Inter font at 16 pt, with the backend's light hinting and
kerning settings. SDL 3.4.14 / SDL_ttf 3.2.2 produce:

| Native style | Text size | Regular layout preserved? | Pixel hash |
| --- | --- | --- | --- |
| regular | 249 x 20 | yes | d4b90961c93a69d3 |
| bold | 284 x 20 | no | 236a6c53a759a8a5 |
| italic | 249 x 20 | yes | 8735982c41c2553a |
| bold + italic | 284 x 20 | no | 89818b1a21d9a798 |

The existing backend preserves regular-face layout for seven synthetic
weights and shears quads by exactly 0.18 around the baseline
(`NanoUI/Draw/Text.hs`). Native bold is therefore not a behavior-preserving
substitute. Repeated upright native draws could emulate weights, but the
renderer-engine API does not expose a custom shear or atlas draw geometry.
`TTF_GetGPUTextDrawData` belongs to the separate SDL_GPU engine; it cannot
feed textures to the existing renderer abstraction across its supported
drivers and SDL >=3.2 minimum.

A hybrid would retain glyph geometry/atlas support for synthetic slant or add
per-run render targets and transformed-text caching, alongside a new ordered
text-command path and immutable caret snapshots. The probe rejects the direct
substitution before migrating widget/editing/custom-drawing consumers. No
subsystem-deletion or negligible-performance-loss claim is established for
the larger hybrid, so it is not landed. The smaller atlas and FFI replacements
above provide concrete dependency reuse without this compatibility cost.

The probe can be compiled through the built SDL package, for example with
`cabal exec -- ghc -no-hs-main -optc-O2 -package-db dist-newstyle/packagedb/ghc-9.14.1 -package-id nano-ui-sdl-0.1.0.1-inplace -o PROBE scripts/profile/ttf-text-engine.c`.
On Windows add `-optl-mconsole`. Run it with the absolute path to
`packages/nano-ui-sdl/data/inter.ttf`.

### RGFW text emission

The separate RGFW text pass is constrained by two existing behaviors:
per-character half-up physical-pixel snapping (`Rgfw/Gl.hs` and the snapping
tests), and text drawn after the corresponding geometry layer
(`Rgfw/Context.hs`). The core glyph path snaps the pen and uses a different
geometry/text ordering. Replacing it with a `FontBackend` adapter alone is
not equivalent; introducing another snapping policy and text-order queue in
core would add machinery instead of simply deleting the backend's emitter.
Retain the current renderer and its dependency-light bitmap-font backend.

## Context construction boundary

Use `RecordWildCards` only at the headless context factory, where every
reference is already bound locally under its exact field name. Keep font,
clipboard and recursive resolver defaults explicit. This removes 31 lines
without adding an abstraction, changing record layout, or changing the
allocation sequence. Core tests and all 21 existing inspection checks pass.

## Core policy reuse and rejected cache-key pilot

Built-in palettes and Base16 construction reuse `defaultTheme` and
`accentColor` for shared defaults instead of repeating them. Complete `Show`
snapshots of the four built-ins and all three Base16 constructors over both
bundled palettes match the freshly compiled baseline exactly (ten themes).
Text damage uses `IntMap.differenceWith` for its asymmetric changed/new-key
set rather than supplying all three policies to a general merge. Existing
damage, styling and core integration tests pass; this removes 20 library lines.

A strict, unpacked `SpanCacheKey` with stock-derived `Eq` was implemented
and tested as a bounded deriving pilot. Behavior tests passed, but the entire
migration saved only four normalized library lines after the new type and
exports were counted. The `-O1` inspection probe failed key erasure: the
generated comparison called `$fEqSpanCacheKey_$c==` with two reconstructed
`SpanCacheKey` records, including the unpacked cached key. The pilot and its
temporary inspection assertion were reverted. Retain flat cache inputs and
explicit comparisons rather than introduce hot-path allocation and a public
record migration for that saving.

Text-menu painting and complete span queries share their allowed-menu/theme
resolution without materializing a common draw-op list. Pane-tree removal
uses `liftA2` and left-biased alternatives to retain either surviving child;
pane moves map the existing split operation over a successful removal. These
changes remove another 11 normalized lines and pass the core integration suite.

The remaining core candidates were narrowed rather than turned into a new
widget/geometry framework:

- The text-command engine is already shared. Numeric input's rejection-before-
  history rule and multiline document identity, width-cache invalidation and
  selection damage remain in their adapters. A unified persisted editor would
  require a store/damage migration, not just deletion of duplicate commands.
- Axis scrollbar calculations already share the normal-scroller kernel.
  Combo rows retain integer window rounding, bounded thumb sizing and delayed
  drag start; viewport reveal policies differ for oversized targets. Moving
  these into a policy record would retain the branches and add call adapters.
- The solver's column loops share storage but have distinct height and gap
  rules (visible-slice caps versus fixed-height/chrome gaps). Keep the reusable
  primitive buffers and specialized loops, including their explicit recursion.
- `microlens` is not added: the entire existing `Field` abstraction and slot
  helpers are only about 76 documented lines, much of which would remain as
  slot policy and compatibility wrappers. There is no demonstrated drastic
  saving to justify replacing the already-inspected zero-cost interface.
- Existing pointer-aware/Dynamic store equality and function-valued damage
  semantics are not ordinary stock/Generic equality; retain those instances.
