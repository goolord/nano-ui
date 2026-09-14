# Data structure and evaluation audit

Scope: Haskell libraries and their demo/example consumers in `packages/`, plus
the SDL font C bridge. Particular attention went to layout, text editing,
plotting, the session loop, and both rendering backends.

## Changes

| Finding | Resolution |
| --- | --- |
| Plot diagrams lived in a process-global `unsafePerformIO` cache keyed only by widget ID; font changes were absent from the key. | Store each cached diagram in the owning context's dynamic widget store. Validate the context's metric generation, theme, style, and chart, and version content per widget. |
| SDL font queries shared four writable foreign scratch buffers globally. | Use call-scoped `alloca`/`allocaBytes` buffers, so separate calls cannot overwrite one another's output storage. |
| The bounded shaped-run cache reversed its insertion-order list twice on every eviction, and stored lazy map/queue updates. | Use `Sequence` for amortized constant-time queue operations and force map and queue updates. The existing FIFO eviction policy is preserved. |
| Cozette's embedded-font parser used `unsafePerformIO` solely to build immutable primitive arrays. | Build and freeze the arrays within `runST`. |
| SDL hid native metric queries, glyph insertion, and lazy run rasterisation in pure callbacks. | `FontBackend` separates IO preparation from IO drawing. Pure `FontMetrics` callbacks read immutable snapshots; `RunQuad` fields are strict. Layout, caret/selection, tables, diagrams, and drawing consumers use the explicit preparation path. |
| Memory-loaded fonts retained an SDL stream over a borrowed `ByteString` pointer. | The C bridge copies the bytes into an owned dynamic SDL stream, whose autoclose lifetime follows the font. Retained backend callbacks check font/atlas lifecycle before native access. |
| Numeric plot points and stroke geometry used boxed vectors. | Retain `PointsXY`, stroke coordinates, and normals in unboxed vectors. Sampling is generic with an unboxed specialization. |
| Table widths/classification and font lookup metrics were boxed. | Table scalar metrics are unboxed, SDL ASCII advances use a primitive array, and Cozette cmap groups use unboxed triples. |
| Label-fit pairwise comparisons indexed boxed six-tuples of numeric metrics. | Collect the metrics directly into an unboxed vector. |
| LTTB built and reversed an intermediate linked list; min/max sampling built a vector of slices and small per-bucket vectors. | Fill bounded mutable output vectors in `ST`; scan each min/max bucket once, preserving input order and tie behavior. |
| Step plots materialized zipped pairs and a small vector per segment. | Generate the output vector directly by index. |
| Ear clipping rebuilt the remaining immutable coordinate vector after every triangle. | Keep coordinates fixed and remove vertices through mutable predecessor/successor indices. |
| Hover selection converted coordinates to lists and constructed a candidate per point. | Scan the unboxed series with a strict indexed fold, retaining the best candidate. |
| Text-area click, drag, and caret paths used `length` followed by partial list indexing. | Add total `TextBuffer.lineAt`, decoding only the requested row; count raw zipper lines without mapping text conversions. |
| The list-based focus helper searched, counted, and indexed the same list. | Traverse with total pattern matching. The live focus path already uses a primitive buffer. |
| Combo commit used partial list indexing. | Match the selected suffix explicitly and retain the current text if no valid selection exists. |
| Numeric widget, form, and tick labels formatted through intermediate `String`s. | Use text builders for integers/floats; cache the 256 hexadecimal byte strings used by color pickers. |
| Tick generation repeatedly measured its accumulated list and could round infinite intermediate values. | Use a strict count, stop when floating-point progress stalls, and guard overflowing spans/noise snapping. |
| Font caches could retain arbitrarily long edited strings and unbounded kerning pairs. | Cap kerning entries and skip text-cache insertion for strings longer than 4096 characters, in addition to the 1024-entry text-cache limits. |
| Demo configuration and probe termination used `unsafePerformIO`. | Load configuration through context-owned host state in IO; signal probe termination through the session input. Font IDs use `newUnique` in IO. |

## Existing design and retained boundaries

- Layout and draw arenas already use mutable primitive arrays and contiguous
  vertex/index buffers. Tables use vectors for indexed collections; replacing
  the text editor's zipper with an immutable vector would make localized edits
  copy the whole document. `lineAt` remains linear in the row position.
- Heterogeneous draw commands and text-bearing collections retain boxed
  vectors. Lists passed to the diagrams library remain at its list-valued
  boundary; indexed numeric workspaces use unboxed vectors.
- The session and message reducers use strict folds. The UI uses Effectful and
  context-owned state, rather than a deep monad-transformer stack. IORefs used
  by the single-threaded UI loop are not, by themselves, evidence of a race;
  this audit does not make a context safe for concurrent rendering.
- SDL submits buffered draw ranges through its C batch layer. Clip and texture
  changes impose batching boundaries. RGFW rasterizes into its owned surface.
  No per-widget FFI batching rewrite was indicated by these paths.
- Image upload uses `withForeignPtr`; RGFW surface allocation and destruction
  are explicitly bracketed. No `StorableVector` foreign-pointer slicing or
  `(//)` cell-update loop was found. The equivalent immutable-rebuilding
  problem in ear clipping was fixed rather than relying solely on that search.
- There are no remaining `unsafePerformIO`/`unsafeDupablePerformIO` uses in the
  package Haskell sources. Font/atlas effects remain confined to the owning
  SDL thread; lifecycle checks do not authorize concurrent use of SDL handles.
- Diagnostic `printf`/`Text.pack` formatting and String-valued external API
  boundaries remain. They are distinct from the numeric widget formatting
  paths changed here. Generic `Show`-based enum labels preserve their existing
  API. No whole-program allocation improvement is claimed.

## API migration

- `FontMetrics` gains `fmBackend :: Maybe FontBackend`. Explicit record
  constructors for pure/custom fonts should set it to `Nothing`; constructors
  such as `monospaceMetrics` already do so.
- With a native backend, use `lineWidthIO`/`measureTextIO`, or obtain an immutable
  snapshot with `prepareFontMetrics fm text` before pure measurement. Use
  `prepareFontMetricsMany` for a finite multi-label workspace. A snapshot's
  shaped-run measurement is for its prepared text, not arbitrary future text.
- `fmGlyph`/`fmRun` snapshots contain metric geometry, not native rasterisation
  effects. Renderers use `drawGlyph`/`drawRun` in IO. Normal widgets handle this
  internally. Returned atlas quads belong to the current atlas epoch/frame.
- `PointsXY` takes `Data.Vector.Unboxed.Vector (Double, Double)`. Numeric
  `lineVec`/`scatterVec`/`areaVec`/`stepVec` accept generic vector inputs,
  converting boxed/storable inputs at construction. Existing Foldable/list
  constructors remain source-compatible; `barVec` still accepts boxed text
  categories. Sampling functions accept boxed and unboxed vectors.

## Verification

Regression coverage includes chart reuse and font invalidation, independent
context versions, total Unicode/tab row lookup, equal-extrema sampling,
boxed/unboxed equivalence, and finite extreme tick values.
Existing decimation tests cover empty inputs, small budgets, endpoint
preservation, ordering, and extrema. Existing integration tests cover focus,
text-area interaction, layout, cache invalidation, thunk checks, and concave
polygon triangulation in both windings.

`nano-ui-font-effects-test` runs headlessly against SDL and checks shaped
measurement, scaling, bounded-cache churn, oversized fallback, font/atlas
replacement, immutable snapshots after shutdown, and rejection of native
callbacks after shutdown.

All test suites passed with the normal `-O2` project configuration. After an
OpenChamber crash was reported during the parallel all-target build,
compilation was limited to one job and a 4 GiB GHC heap:

```sh
GHCRTS=-M4G cabal build all:tests -j1
cabal test all -j1 --test-show-details=failures
```

The demo's `-O2` compilation exceeded that cap. It built with the invocation-only
options `-O1 -fno-specialise-aggressively -fno-late-specialise`, and its full
headless `--selftest` passed. No permanent project optimization settings changed.

The SDL benchmark startup can exercise real font warmup and drawing headlessly:

```sh
SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software \
  cabal run nano-ui-sdl-bench -j1 -- --list-tests
```

Its explicit IO warm-glyph allocation gate reported **0.282 B/lookup**, below the existing
**1.0 B/lookup** budget. This is a regression gate, not a before/after benchmark.
