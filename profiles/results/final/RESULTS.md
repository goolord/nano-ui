# nano-ui optimization results (papers: GHC Optimization Guide + Data Structure Optimization)

Date: 2026-09-10. Toolchain: GHC 9.14.1 / cabal 3.16, Linux.
Baseline: this session, recorded in `profiles/results/baseline/` (note: the
repo-root `nano-ui-sdl-profile.prof` was generated on Windows and is not
directly comparable to Linux runs; wall-clock comparisons below are all
same-machine same-session).

## Deterministic gate: allocations per benchmark iteration

| Benchmark            | Baseline | Final   | Delta   |
|----------------------|----------|---------|---------|
| ui/runFrame.small    | 13 KB    | 13 KB   | =       |
| ui/runFrame.medium   | 367 KB   | 356 KB  | -3.0%   |
| ui/runFrame.large    | 758 KB   | 734 KB  | -3.2%   |
| id/nextId burst4096  | 590 KB   | 590 KB  | =       |
| id/scopedWidgets     | 574 KB   | 572 KB  | =       |

Wall-clock during low-load windows: within noise (medium 163 -> 163-171 us,
large 348 -> 359-378 us vs +/-8% run variance). Late-session runs measured
193-204 us under external machine load (load average 2.5 from an unrelated
build); re-bench on an idle machine for clean timings.

## Changes applied

Batch A (mechanical):
- nano-ui.cabal: -O2 added to library/demo/profile (uniformity; standalone
  builds previously built the library at default optimization).
- UNPACK on strict scalar fields: DrawText ops, WindowResizeDrag,
  TextInputDrag, TextFieldClickCell, OverlayState, AnimationState,
  SpanCacheEntry (incl. newtype Word32 Color), CustomDrawContext,
  InteractionState, Style/Theme/Base16 Color fields, Input record,
  DebugSampler, DamageInflated.
- Strictness: Atlas.hs records bang'd; Table colSizes/colOrder bang'd;
  ClickTracker tuple -> strict ClickTrack record.
- Lazy folds: Context foldl -> foldl'; foldInputKeys V.foldl -> V.foldl'.

Batch B (hot-path pragmas; profile-guided):
- Paint.hs: INLINE lowerNode/walkChildren/walkChildrenWithOccluders.
- Layout/Arena.hs + Solve.hs: `>>= pure . toEnum` accessor closures removed.
- Draw.hs: cornerQuadrant/cornerCosSin NOINLINE -> INLINE (was called
  per-segment in the 9.2% corner-fan cost centres).
- Solve.hs: INLINE on scratch sweeps (copyScratchRange, sum*, mark/scan/
  lockGrow, applyGrowShares/applyShrink, reverseScratchTriple, resolveSize).
- Spans.hs: INLINE collectClippedSpans/walkChildSpans/findAncestorMaxW.
- Draw.hs groupCmdsByLayer: 4 boxed IORef cursors -> prim-array cursors.
- Draw.hs pushText/pushTextStyled: per-char kerning closure eliminated.
- SPECIALIZE: clamp @Float/@Int/@Double, swapPrim @Int/@Float, growP
  @Float/@Word32.

Batch C (structural):
- SpanArena: SpanArenaArrays snapshot + withSpanArenaSnap (mirrors
  withArenaArraysSnap); pushSpan/fold fetch the 11 columns once per batch
  instead of 11 IORef derefs per span; growth refreshes an active snapshot.
- Drawing-cache tuples -> strict records: PopupConfig, DrawOpCacheEntry,
  CustomDrawOpCacheEntry (strict containers only force WHNF; tuple fields
  stayed lazy thunks).
- SDL: persistent RenderBatch in SdlEnv (was calloc/free per presented
  frame; flush with `finally`); AVX2 8-wide damage-cull kernel wired into
  nano_ui_batch.c (compile-time guarded, scalar fallback kept); dead
  nano_ui_scale_floats_avx2 removed.
- Store.setStore: `$!` on write (record-update thunks no longer parked in
  the long-lived IORef).

Batch D (guardrails, per Paper 1 mandate):
- New test-suite nano-ui-inspection (inspection-testing 0.6.3): 5
  obligations pass -- no dictionaries in hashWidgetId/fnv1a/mixId patterns;
  SIMD vertex poke pattern free of boxed pairs/IORef (NoAllocation is
  unusable on GHC 9.14: unboxed-tuple construction is counted as
  allocation by the plugin).
- New integration case `no-thunks` (nothunks): checks every stored value in
  the widget store for retained thunks after frames. Verified values are
  clean; container-spine checks produce false positives on GHC 9.14 (empty
  maps flagged) and are documented as skipped in Cases/NoThunks.hs.

## Investigated and deliberately skipped (documented)

- IntMap insert batching in the widget store (8% alloc cost centre): the
  store has read-after-write semantics within a frame; safe batching is an
  architectural change (Paper 2's migration matrix row), not a drop-in.
- unpackAppendCString# (3.9% alloc): traced to the debug overlay's
  `T.pack (printf ...)` rows -- instrumentation cost, paid only while the
  overlay is visible.
- Per-frame `newIORef ClipState` + float `alloca`s in SDL queries: single
  small allocations per frame; not worth the readability cost.
- settleGrow O(n^2) worst case: passes bounded by grow-child count (n+1),
  each sweep locks >= 1 child; realistic UIs converge in 2-3 sweeps.
- -fexpose-all-unfoldings trial: built and measured; no alloc/time gain ->
  reverted (kept out of cabal.project).
- Deferred (2e): unboxing Vector Key, IntMap [Float]/[Int] store fields,
  sceSpans boxed tuples -- revisit only if post-optimization profiles show
  them.

## Verification

- cabal test all: 7/7 suites PASS (incl. new nano-ui-inspection with 5
  compile-time obligations and the no-thunks runtime case).
- Benchmarks recorded per batch under profiles/results/{baseline,batchA,
  batchB,batchC,final}/.
- NOT verifiable in this VM: the sdl3/draw benchmark group fails with
  `SDL_SetRenderTarget(retain) failed` (software renderer environment
  issue, pre-existing -- same failure in the Phase 0 baseline). The SDL C
  changes (AVX2 cull, persistent batch) compile clean and were
  logic-reviewed but need a GPU renderer host for runtime verification.
