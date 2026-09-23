# Source reduction follow-up

Baseline: `a92ac09`, Windows x86-64, GHC 9.14.1, Cabal's `-O1`.
The baseline build and all nine available test suites passed before editing.
Baseline binaries remain in `dist-newstyle`; candidates build in a separate
directory. No dependency or library code is removed from the source accounting
by moving it into another local package.

## Result

Across every maintained library package (core, SDL, RGFW, bindings, diagrams,
forms; excluding demos, tests, and vendored `RGFW.h`), the accepted changes
remove **237 physical / 373 formatter-normalized lines** from `a92ac09`.
Counting new tests, docs, and generators is outside that figure either way.

Accepted boundaries:

| Commit | Boundary | Normalized lines |
| --- | --- | ---: |
| `c03d4db` | Simpler, triangle-correct SDL damage rejection | -152 |
| `3b69c4c` | Shared cursor state and short-circuiting fallbacks | -26 |
| `30575c6` | Header-checked SDL_ttf imports in place of forwarding wrappers | -75 |
| `162d9f3` | SDL-owned atlas storage, direct glyph uploads | -27 |
| `f472ff9` | RGFW event bindings and constants derived from the header | -31 |
| `0dd0f79` | `RecordWildCards` at the context factory | -31 |
| `4cc2784` | Shared palette defaults and map policies | -20 |
| `496c32b` | Shared text-menu setup and pane fallbacks | -11 |

Rejected after measurement: direct SDL submission and its variants; the strict
unpacked span-cache key; the form scalar-control helper; a shared SVG/diagrams
cubic flattener; and the larger SDL_ttf renderer-engine migration. Each is
recorded below with the evidence that failed the gate.

## Final validation

- `cabal build -j1 all`: succeeds with the repository's `-Werror` project
  settings.
- `cabal test -j1 all`: all eleven suites pass (`nano-ui-test`,
  `nano-ui-inspection`, `text-buffer-spec`, `nano-ui-render-test`,
  `nano-ui-font-effects-test`, `nano-ui-font-search-test`, `nano-ui-rgfw-test`,
  `nano-ui-rgfw-bindings-test`, `nano-ui-diagrams-test`, `nano-ui-form-test`,
  `ditto-test`).
- SDL demo, notepad, and logs `--selftest`: pass.
- `cabal run nano-ui-rgfw-profile`: completes 500 frames.
- `cabal bench nano-ui-id-bench`: passes, including the per-frame id-allocation
  gate.
- `cabal check` for nano-ui, nano-ui-sdl, nano-ui-rgfw-bindings, and
  nano-ui-form: no errors or warnings.
- `cabal sdist all`: all eight archives generated; the RGFW bindings archive
  contains `lib/RGFW/Raw.hsc` and `test/Events.c`.

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
(the text emitters in `NanoUI/Internal/Draw.hs`). Native bold is therefore not a behavior-preserving
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

## Forms and diagrams boundaries

A shared scalar-control helper for the form package was prototyped twice. With
the caption applied inside the helper it saved and spent exactly the same 24
lines. Widening it to carry the activation flag and every scalar control left
the raw count 13 lines lower, but Fourmolu expands an argument-heavy signature
and five call sites at the repository's column limit, so the
formatter-normalized count was **18 lines higher**. The plan's gate rejects an
abstraction that enlarges the normalized total, so both variants were reverted.
Measure after formatting, not before.

The form runner's `nanoFormSubmit` and `nanoFormEx` share only the
`submitted-before` lookup and `withNanoForm` scope; their Enter handling,
validation-status return, and submit-button behavior differ. A helper for two
call sites saves no lines before the shared scope is counted. `FormView`'s
`Semigroup`/`Monoid` are already two small instances. Retain the runners and
instances as written.

The core SVG flattener and the diagrams `flattenCubic` share an algorithm but
not a contract: one is an `ST` emitter over `Point` with a depth cap and a
two-control flatness test at 0.25 device-pixel tolerance, the other a pure list
over float pairs that tests one control at 0.5 logical-unit tolerance.
Extracting a shared, parameterized subdivision would add a generic point
abstraction and a cross-package export used once, and would change one side's
tessellation. Retain both, as the earlier audit did for its tessellation
candidates.

## Fourth pass

Baseline `92df64d`, same toolchain. Haskell across all packages goes from
62,434 to 61,591 physical lines (**-843**) and from 70,746 to 69,892
Fourmolu-normalized lines (**-854**); library directories account for -681
physical lines, tests for -138. The SDL shim loses another 30 lines of C.

| Commit | Change |
| --- | --- |
| `501d279` | `LANGUAGE` pragmas already on through GHC2024 or `default-extensions` (-109) |
| `3f23b53` | The facade lists `Widgets.Custom`, `RichText`, `TextDocument` and `TextField` as modules; the export set is unchanged (checked against the interface file) |
| `7c81c4a` | `IOArr` for the arena's mutable arrays |
| `f63d5c1` | SDL: every non-empty line is shaped, so the per-character glyph path (codepoint-keyed atlas, ASCII/non-ASCII quad caches, C glyph renderer) was only reached for empty text. Removed; the atlas re-warm now shapes a printable-ASCII line |
| `29ac074` | SDL sessions open from `SdlOptions`; `WindowConfig` was a field-for-field copy |
| `9b4f0c3` | SDL cursors in a `SmallArray` indexed by a derived `Enum` |
| `22eeeb4` | Cozette rows unpacked directly, one bit reader; the 1x/2x/4x tables are unchanged |
| `dea82d8` | `maybe`/`<\|>` in place of nested `case` fallbacks |
| `6fa50d2` | Widget layouts composed from the `Style` modifiers instead of record literals |
| `a110dcd` | `Response` built positionally |
| `b712c50` | One flip rule for the four rect-anchored popup sides (equal to the old placement on 11,016 cases) |
| `7d6d041`, `a5d89e2` | Pane grid `axisLay`; one four-point emitter in the SVG stroker (rasters byte-identical for every cap/join) |
| `ae14ff8` | Diagrams: `foldMap` titles, one fill for marker shapes (chart ops unchanged) |
| `0feccc2`, `0112fff`, `1e6fa04` | Demo options parsed into `SdlOptions`; RGFW demo cycles scales with `nextEnum`; `whenM` in the log viewer |
| tests | `assertJust`, `t`/`px` runner helpers, one thumb-drag test for both axes, tuple table fixtures, shared pane grid config |

Checks: `cabal build all`, all twelve buildable suites, the demo UI tests,
500 RGFW profile frames and `cabal check` pass. Headless allocation is
byte-identical in all five scenes. The SDL bench's warm lookup gate now
measures shaped lines (0.002 B/lookup). SDL microbenchmarks shift by about
±100 B/frame after `29ac074`; with ten times longer samples that falls to
about ±10 B, so it is a one-off startup allocation amortized over the
samples, not a per-frame cost. Seven-pair SDL timings are centred on zero.

The headless `svg` scene reads about 2% slower from `dea82d8` on. That scene
only calls `rasterizeSvg`, and `NanoUI/Svg.o` is byte-identical before and
after the commit (GHC does not recompile it), so this is a binary-layout
effect on the rasterizer's loop rather than slower code.

Considered and not landed: `hsc2hs` for the shaped-result struct (about -10
net once the new module and build tool are counted); `RecordWildCards` at SDL
constructors (the locals do not share the field names); a shared
theme-scope accessor (-1 after imports); `fixedSpec` for custom widgets, a
`benchN` profile helper, the form `select`/`radio` helper and `FormView`
deriving via `Ap` (each under five lines net); a `MultiWayIf` rewrite of
the solver's height resolution (two lines on the per-node path). The
excluded trade-offs were a shared SDL/RGFW event type, trimming the RGFW
bindings, public re-export shells over internal modules, and demo/debug
text changes. Internal modules keep explicit export lists.

## Fifth pass

Baseline `866b610`, same toolchain. Haskell across all packages goes from
61,591 to 61,060 physical lines (**-531**). Maintained library code accounts
for -124 physical / **-151 Fourmolu-normalized** lines, tests for -423, and
the demo library for -10.

| Commit | Change |
| --- | --- |
| `194782d` | Each integration module exports one `tests` list; `Main` concatenates them instead of naming all 252 tests a third time (-100) |
| `eaf7a53` | A `Spec` prelude re-exports the library, harness, assertions, `Control.Monad` and `Data.IORef` for the integration modules (-308); three local helpers that shadowed harness names are renamed or replaced |
| `f133b74` | `writeStoreInt`/`Float`, `adoptStore*` and `recordStore*` were partial applications of `writeSlot`, `adoptSlot` and `recordSlot`; callers use those directly (-54) |
| `ff9e71e` | `fbDrawGlyph` dropped from `FontBackend`: SDL's always returned `Nothing` and RGFW attaches no backend, so `drawGlyph` reads `fmGlyph` (breaking; in the changelog) |
| `ba90e13` | `splitMins` for the three hand-written `subtreeMin`/`mainMins` pairs; the test-only `dropPreview`/`dropPreviewTree` wrappers and the unused `treeMovePane` export are gone (-28) |
| `1061603` | A click selects as a drag that has not moved: `textSelectionForClick` and `applyTextAreaClick` repeated the drag rules (-19) |
| `69b4599` | `withHiddenWindow` in `DemoApp` for the three demo self-tests and the recorder (-10) |
| `8444ed3` | Select rows pick through the combo row index, offset to their centred block |
| `cee897c` | `forNodesOfType_` for the five arena walks that visit one node type |
| `77b847f` | One `hasFlag` for the packed style flags in place of nine one-bit predicates (-28) |
| `4fbf32e` | `isInteractiveNode` is `isWidgetNode` without `NodeWidget`; the `cacheableWidgetLabel` alias is inlined (-13) |

Checks: `cabal build all`, all twelve suites, the demo self-tests, both
benchmark suites (glyph lookup 0.002 B against its 1 B budget), `cabal check`
and `cabal sdist all` pass. Headless allocation is byte-identical in all six
profile scenes after every library commit.

The plan for this pass estimated structural savings from a code survey
(about -1.6k to -2.3k library lines). Reading the sites showed most of the
apparent duplication is deliberate, and those candidates were not landed:

- **One clip walk.** The paint walk pushes draw-arena clips as it goes, the
  scroll-target hit test deliberately includes the scrollbar lanes, and the
  view-time viewport test reads the previous frame's clips. Only the span walk
  could read the stored clips (about 6 lines), and the stored clip falls back
  to the parent's where the span walk culls, so it was left alone.
- **1D scrolling as 2D.** `ScrollAxes`, `getScrollOffsetIn` and
  `scrollBarLayoutIn` already share the kernel; what remains is three stored
  offset slots, whose merger is a persisted-state migration.
- **Dropdowns and the text menu on `NodePopup`.** They are post-layout
  overlays so they open, pick and route the pointer within one frame. The
  widget-side select pick looked redundant with `finalizeSelectPick`, but
  removing it fails `select-drag-to-select`: a press on the select that is
  released over a row keeps its route and only the widget sees it.
- **A node-kind table.** Painting and measuring differ per kind; the kind
  predicates are a handful of short documented functions. `hasFlag` and the
  derived `isInteractiveNode` took what was shared.
- **Axis-generic solver reads.** Each site treats a fixed size differently
  (clamped or not, defaults of 32 or 8, percent, padding); an eight-field
  record was already rejected in the first pass.
- **One text-wrap decision.** The sites already share `textWrapCap`,
  `findAncestorMaxW` and `wrapsNarrower`; an `effectiveMaxW` helper nets zero.
- **Behaviour hooks.** Table resize, the pane divider and the combo thumb
  each fold their drag into a different state encoding; the raw key reads in
  the colour picker, numeric input and combo sit in IO or pure step functions
  that `KeyNav` (an `Eff` hook gated on focus) does not reach; the drag latch
  shared by `useDrag1D` and `useDrag2D` is two lines.
- **Stored-editor command helper, backend runner helper.** Both pairs load and
  save different state; a shared function needs as many parameters as it
  saves lines.
- **`DragGeom` in `GridEnv`.** `GridEnv` is built from the drag result.
- **The text-area wheel test pair and the other packages' suites.** The two
  tests diverge after their first steps; the RGFW, forms, diagrams and SDL
  suites have no repeated registration to remove.

## Sixth pass

Baseline `4831382`. Goal: cut the two libraries hard, breaking changes
allowed, keeping every feature and every exported helper an application
could use. Ten agents each took one subsystem in its own worktree, then two
more took the work that crosses subsystems (core) and the plumbing the core
shares with the backends (core, SDL, RGFW). Their branches merged into
`loc6-int`, and each merge was built, tested and profiled before the next.
A last commit opens the libraries' multi-line imports of their own modules;
it is separate so it can be reverted alone.

| | Before | After | Change |
| --- | ---: | ---: | ---: |
| `packages/nano-ui/lib` | 35,690 | 30,095 | -5,595 (-15.7%) |
| `packages/nano-ui-sdl/lib` | 5,067 | 3,961 | -1,106 (-21.8%) |
| Both, non-comment non-blank lines | 30,166 | 23,865 | -6,301 (-20.9%) |
| Both, comment lines | 7,611 | 7,520 | -91 |

Of the -6,701 physical lines, the import commit accounts for -2,072 and the
simplifications for -4,629. Per subsystem: layout -629, frame/damage/runner
-641, SDL -910 (two passes), text editing -429, big widgets -408, paint and
draw -366, scroll and overlays -304, small widgets -279, context core -188,
types/style/testing -182, cross-cutting -180, backend plumbing -52.

Headless allocation (bytes, `nano-ui-profile`) fell or held in every scene:

| Scene | Before | After | Change |
| --- | ---: | ---: | ---: |
| widgets | 443,193,128 | 404,187,328 | -8.8% |
| svg | 449,972,096 | 449,965,824 | 0 |
| canvas | 540,878,712 | 539,008,488 | -0.3% |
| canvas-keyed | 24,946,616 | 23,060,736 | -7.6% |
| window | 1,632,184,712 | 1,518,883,256 | -6.9% |
| window drag | 1,644,036,040 | 1,528,914,944 | -7.0% |
| pointer | 14,182,503,048 | 13,034,713,904 | -8.1% |
| grow | 19,637,229,312 | 15,803,642,888 | -19.5% |
| textarea | 217,784,416 | 210,498,856 | -3.3% |
| textarea-text | 7,065,295,160 | 7,057,601,064 | -0.1% |

The largest wins came from strictness the rewrites exposed: strict
placement arguments and the single-axis share-out in the solver (`grow`),
a strict id-scope pair on container entry, and resolving every node font
through one path (`widgets`). Candidates that raised allocation were
reverted or reshaped before merging, among them plain text through the
styled path (+18% on widgets), a lazy `<$>` over the view's static record
(+1.8 to +5%), a closure shared by the hover tint (+9 MB on widgets) and
publishing scroll metrics from one node per id, which broke virtualized
lists (+5%).

Public API changes are in the changelog. The notable ones: `SessionDriver`
loses `sdIsHardQuit` (the loop checks each event for Ctrl+C) and `sdDraw`
returns `IO Bool`; `ComboInput` carries `ciInput`; `runClickReduce` moves to
`NanoUI.Testing.Harness`; `withExternalText` is gone. The exposed
`NanoUI.Internal.*` modules changed widely.

Bugs found and fixed on the way, each with a regression test: a settled
`animateTo` value reset to zero after 300 frames; grow children of a plain
column overflowed by its gaps; the picked row of an open dropdown had the
wrong span colour; RGFW missed a Ctrl+C whose Ctrl was released within the
same event batch; and the SDL bench's glyph-lookup gate failed at the
baseline (16 B per warm lookup, now 0.003 B).

Rejected, with the reason:

- **Text area as a scroll node.** Its gutter geometry differs from a scroll
  container's, moving its offset into the float slots changes damage
  semantics, and `TextAreaState.scrollOffset` is public. The shared
  scrollbar layout (`scrollNodeBars`, `textAreaBarLayouts`) took what the
  two had in common.
- **Deleting dead `Slot` constructors.** Slot keys come from `fromEnum`, so a
  deletion reshuffles every later key; one measured +0.075% on textarea.
- **Merging the plain and custom drawing registries.** They cache
  differently (a plain drawing with key 0 is cached; a custom one is
  rebuilt and compared every frame).
- **Derived slot keys.** Keys nest, so additive schemes collide within one
  widget.
- **Sharing SDL and RGFW event translation through the core.** Lines moved
  into the core count against it; the per-event Ctrl+C check was the part
  that paid.
- **Rounded rects as three quads, one text path for every single-line
  field.** Both change pixels.

## Seventh pass

Baseline `247b236`. Goal: zero-cost abstractions that cut the two libraries
further. Seven read-only agents surveyed every module for repeated shapes
across files. After six passes little repeats: they rejected about 130
candidates, and what survived totals under 1%. Three agents implemented the
accepted items in their own worktrees, merged into `loc7-int`.

| | Before | After | Change |
| --- | ---: | ---: | ---: |
| `packages/nano-ui/lib` | 30,095 | 29,858 | -237 |
| `packages/nano-ui-sdl/lib` | 3,961 | 3,912 | -49 |
| `packages/nano-ui-sdl/cbits` | | | -15 |

| Commit | Change | Lines |
| --- | --- | ---: |
| `8138510` | A scroll node's viewport and bars computed from the node; three 10-12 argument helpers, a dead one and a one-caller one gone | -79 |
| `d22e9c8` | Record fields bound in function heads instead of alias lines | -70 |
| `5a36e60` | Core and SDL share one `GenCache` insert and lookup; SDL drops `hashable` and `unordered-containers` | -25 |
| `cc95794` | A window resize edge is the side of each axis it moves | -22 |
| `b4ebfb8`, `7242c9e` | The position pass hands boxes down as a `Rect`; `withAxisSnaps` inlined | -33 |
| `0f3e7da`, `e261968` | Scalar index and vertex stores in `NanoUI.Internal.SIMD` | -28 |
| `2f27552` | `freshWidget` for the id and context 25 widget bodies start with | -16 |
| `333263d`, `9710303`, `e7dd590` | The glyph atlas returns UVs; a dead SDL export; two draw loops through `foldUpTo`/`snapRectOrigin` | -13 |

Headless allocation (bytes, `nano-ui-profile`) fell or held in every scene:

| Scene | Before | After | Change |
| --- | ---: | ---: | ---: |
| widgets | 404,187,328 | 404,108,568 | -0.02% |
| svg | 449,965,824 | 449,965,824 | 0 |
| canvas | 539,008,488 | 538,929,240 | -0.01% |
| canvas-keyed | 23,060,736 | 22,993,032 | -0.3% |
| window | 1,518,883,256 | 1,517,268,552 | -0.1% |
| window drag | 1,528,914,944 | 1,527,043,976 | -0.1% |
| pointer | 13,034,713,224 | 13,033,298,536 | -0.01% |
| grow | 15,803,642,888 | 14,595,963,496 | -7.6% |
| textarea | 210,498,856 | 210,424,280 | -0.04% |
| textarea-text | 7,057,601,064 | 7,057,535,424 | 0 |

The `grow` drop comes from the `Rect` boxes in the position pass, whose
strict head match unboxes them. The scalar vertex stores draw about 5% faster
in `nano-ui-sdl-profile` (button, checkbox and slider microbenchmarks 15-19%),
measured over eight alternating pinned rounds; GHC's NCG builds a packed
`FloatX4#` from scalars with a long shuffle chain. The first scroll-node
rewrite raised `pointer` by 108 KB: a local gutter helper boxed its Float
arguments until they were made strict.

Rejected, with the reason: `OrPatterns` (needs GHC 9.12; the libraries should
build on 9.10); merging small internal modules (separate modules compile in
parallel); the six all-empty state records built positionally (labelled
fields); a widget-construction kit beyond `freshWidget` (the rest of each
widget's start differs); per-emitter reservation or RGBA records in the draw
code (they become free variables of the per-glyph loops).

Found on the way and fixed separately (branch `fix-field-font`): a text field
with its own font size drew its glyphs from the base font when the draw arena
draws text.
