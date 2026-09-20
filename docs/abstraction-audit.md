# Abstraction audit

Baseline: `b083037`. Environment: Windows x86-64, GHC 9.14.1,
Cabal 3.18.1.0, the project's default `-O1`, single-job builds.

## Source reduction

The library Haskell sources have **155 fewer physical lines**. Applying the
same `fourmolu.yaml` to both versions before counting gives **374 fewer lines**.
The difference is formatting: the baseline contains long declarations that the
80-column configuration expands. Both counts include comments and blank lines;
neither counts tests, examples, documentation files, dependencies, or vendor C
as library-code savings. Renamed and deleted modules are included.

## Implemented

| Area | Change |
| --- | --- |
| Widget construction | One inlined leaf initializer, with text-style packing kept on the styled path |
| Controlled inputs | Shared boolean activation and scalar completion; explicit live-versus-caller comparison policy |
| UI scopes | Shared bridge for the existing bracketed ID and paint scopes |
| Forms | One named/positional input family, independent field captions, and `inputWidget` for custom controls |
| Reducers | Three generic adapters replace per-widget `NanoUI.Emit` wrappers |
| Scrollbars | One scalar calculation for both orientations, including inverse thumb positioning |
| Themes | Shared Base16 construction with the existing dark/light palette choices |
| Animation | Indexed applicative channel traversal replaces lists and partial reconstruction |
| Draw layers | Cumulative offsets replace the custom packed slice instance; counts become sort cursors |
| Draw commands | `vector`'s `IsoUnbox`/`As` deriving replaces the handwritten `Prim` instance |
| Canvas | Standard strict `State` replaces handwritten monad instances |
| Solver/document | `primitive` bulk copying and `containers` lifted sequence equality replace local mechanics |
| Diagrams | Standard pointwise render composition; cubic samples stay in float pairs |
| SDL ownership | `Data.Acquire` pairs acquisition and release, including partial startup failures |

`resourcet` is the new external package, confined to SDL session ownership.
`vector` and `transformers` were already in the dependency closure; components
that now import them declare them directly. No `vector-algorithms` dependency
was introduced.

## Compiler and behavior checks

The inspection suite has 21 passing assertions. Alongside the existing SIMD
and store checks, it checks:

- vector animation traversal equals handwritten scalar arithmetic;
- command reads/writes contain no dictionaries and inline the representation
  codecs;
- canvas construction equals direct construction of the draw-op array.

Behavior checks cover command field round trips and growth, layer offsets and
stable ordering, channel order and colour clamping, generic reducer event gates,
custom form decoding, positional fields, and caption-independent persistence.
Existing widget, layout, text, damage, form reset, and backend tests also pass.

The new SDL failure test was run against both versions. With an unavailable
renderer, the baseline leaves `SDL_WasInit(0) == 16416`; the refactor leaves
zero. The same test then opens a successful session and verifies that retained
font callbacks reject their released native handles.

## Runtime measurements

Seven interleaved baseline/candidate runs, alternating execution order. The
headless table reports median RTS **MUT elapsed seconds**, excluding GC and
startup/shutdown time. Allocation is the whole program's RTS heap allocation.
The normal profile uses `-N1 -A64m -T -I0`.

| Headless scene | Baseline bytes | Candidate bytes | Baseline seconds | Candidate seconds |
| --- | ---: | ---: | ---: | ---: |
| `widgets`, 3,000 frames | 590,980,936 | 591,478,728 | 0.582 | 0.574 |
| `canvas`, 3,000 frames | 636,199,400 | 636,768,520 | 0.237 | 0.238 |
| `canvas-keyed`, 3,000 frames | 120,232,368 | 120,586,704 | 0.093 | 0.093 |
| `textarea`, 1,000 keystrokes | 248,139,128 | 248,657,776 | 0.111 | 0.111 |
| `svg`, existing raster workload | 450,074,488 | 450,071,456 | 0.402 | 0.402 |

The largest heap increase is **0.295%**, from the structure-of-arrays command
representation. It has more array headers than the old interleaved buffer.
This is a measured low-cost tradeoff, rather than a claim of zero allocation
overhead for the storage migration.

Native SDL measurements use the existing profiler's warmup and 40-frame means,
again taking medians across seven paired process runs:

| Scene | Baseline ms/frame | Candidate ms/frame |
| --- | ---: | ---: |
| Full demo, idle presentation | 0.653 | 0.633 |
| Full demo, active presentation | 0.574 | 0.550 |
| Full demo, `runFrame` only | 0.259 | 0.250 |
| Forced full replay with debug window | 0.795 | 0.797 |
| Table, SDL presentation | 0.329 | 0.328 |
| Four charts and a diagram | 0.045 | 0.044 |
| 500-point line chart | 0.047 | 0.048 |

All 30 profiler workloads were compared. Timings are noise-sensitive, especially
at the profiler's 0.001 ms display precision; these samples establish a small
cost envelope rather than a general speedup claim. Decimation, chart caches,
font measurement policy, and tessellation algorithms remain intact.

A separate form workload renders 20 independent four-field forms (text, slider,
checkbox, select), with 20 warmup frames and 1,000 measured frames. Seven paired
runs give 0.413286 to 0.405704 ms/frame and 648,187.896 to 648,255.656 allocated
bytes/frame: **0.0105%** more allocation, including the core storage changes.

### Build and code size

Fresh core-library builds in separate empty build directories, with dependencies
already installed, took 69.03 seconds before and 71.07 seconds after. These are
single samples, not a compile-time regression threshold.

`llvm-size` reports these combined code/data section sizes for the profile
executables (not complete on-disk file sizes):

| Executable | Baseline bytes | Candidate bytes |
| --- | ---: | ---: |
| Headless profile | 26,236,353 | 27,019,049 |
| SDL profile | 54,384,505 | 54,523,021 |

The unboxed-vector representation removes maintained primitive code but adds
generated vector-instance code: source reduction is not binary-size reduction.

## Evaluated and retained

| Candidate | Decision and evidence |
| --- | --- |
| Table merge sort | Keep it: its index buffer is `PrimArray`, not an existing `Vector` |
| `svg-tree` | Reject under the footprint constraint: isolated solved core closure grows from 55 to 81 packages |
| `rasterific-svg` | Reject under the footprint constraint: core closure grows from 55 to 88 packages |
| `Chart-diagrams` | Current index cannot solve `diagrams-svg` on `base-4.22`; its font/picking adapter is also substantial. Reuse the existing diagrams backend instead |
| `text-icu` | Public bidi API lacks the visual-run mapping the shaper needs; local solve also requires unavailable `icu-i18n >= 62.1` |
| Typed arena columns | Pilot float reads/writes pass four Core equality/erasure checks at saturated call sites, but a complete six-scalar-field source migration adds 18 library lines once consumers are included; retain existing strided accessors |
| Bracketing every container | Rejected measured variant: widgets allocation rises from 590,980,936 to 605,021,848 bytes. Existing container behavior retained |
| Deferred generic widget descriptors | Existing `With'` implementations already own construction. Retain convenient value/response wrappers; remove the parallel reducer/form families instead |

Dependency counts include the root and boot packages and use the locally
available Cabal index. The SVG additions include a second XML stack, image
decoding, optics, and geometry; the raster renderer additionally brings
Rasterific and font parsing. Moving these imports to another local package
would not count as removing their dependency footprint.

The arena pilot covers grid column count, minimum column width, and node value.
Their existing six getters/setters occupy 18 nonblank implementation,
signature, and pragma lines. The corresponding `Column` representation,
constructor, generic read/write operations, and three descriptors occupy 20,
before imports, exports, documentation, or caller changes. This small boundary
demonstrates specialization but not a source reduction; it is not a claim that
a future, broader schema redesign could never pay off.

The follow-up source-budget experiment expands this to **all six simple scalar
read/write pairs**: grid column count, grid minimum width, scroll content width,
node value, node font size, and style index. It generates the descriptor API,
removes the twelve accessors, preserves the field documentation, and migrates
imports and calls in all 25 affected files. Both sides are formatted until
Fourmolu reaches a fixed point, including import merging and wrapping.

| Full scalar-column candidate | Formatted line change |
| --- | ---: |
| `Layout/Arena.hs` | -8 |
| Other library modules | +26 |
| Library total | **+18** |
| Tests | +7 |
| Complete migration | **+25** |

Thus the constructor cost does amortize, but the consumer changes outweigh the
local reduction. This candidate is rejected at the source-reduction gate,
before changing production files or running a performance experiment. The
374-line normalized reduction above was also rechecked using formatting to a
fixed point, so it does not count transient import-layout differences as savings.

## Verification and migration

- `cabal build -j1 all`
- `cabal test -j1 all`: all nine available suites pass
- SDL demo, notepad, and logs `--selftest`: pass
- Native RGFW profile: 500 frames complete
- `cabal check`: core, SDL, RGFW, diagrams, and forms pass without warnings
- `cabal sdist all`: all eight project archives generated, including ditto
- Fresh project containing only unpacked source archives: full build and all
  nine suites pass, including all 21 compiler inspection assertions

The archive verification includes the renamed `NanoUI.Form.Input` module and
confirms that the removed `Named` and `Unnamed` modules are absent. The isolated
project uses the same compiler, SDL/SIMD flags, and single-job builds; none of
its package entries refer to the original source checkout. Archives are unpacked
before testing because Cabal treats tarball package entries as non-local
dependencies and does not run their test suites directly.

The core and form changelogs document the breaking API changes: indexed
`Animatable` traversal, vector draw commands and cumulative layer offsets,
generic reducer adapters, and the unified `FieldName` input API. The public
command iteration helpers retain their signatures.
