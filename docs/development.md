# Working on nano-ui

## Build and test

The workspace uses GHC 9.14 and Cabal. `nix develop` supplies the native
dependencies as well as the Haskell toolchain. The current `cabal.project`
also expects a checkout of `ditto` at `../ditto`.

```sh
cabal build -j1 all
cabal test -j1 all --test-show-details=failures
```

Start with a single build job: concurrent optimized GHC builds can exhaust
memory even on a desktop workstation. If a compiler heap limit is needed,
use `GHCRTS=-M4G cabal build -j1 <target>`; exceeding the limit then fails
the build rather than consuming all available memory. Increase parallelism
only after measuring the memory use of the affected targets.
Apply `GHCRTS` to the build command, then run tests without it: some test
executables intentionally do not enable runtime-option overrides.

Warnings are enabled in each package's Cabal file. The workspace adds
`-Werror` for the core, SDL, diagrams, and demo packages; published packages
do not force downstream builds to treat compiler warnings as errors.

For a shorter feedback cycle, select the affected suite:

```sh
cabal test nano-ui-test --test-show-details=failures
cabal test text-buffer-spec --test-show-details=failures
cabal test nano-ui-form-test --test-show-details=failures
cabal test nano-ui-diagrams-test --test-show-details=failures
cabal test nano-ui-rgfw-test --test-show-details=failures
cabal test nano-ui-font-search-test --test-show-details=failures
```

The integration suites run headlessly. Native presentation changes also need
a check in the affected window backend; compiling SDL code does not exercise
its texture lifecycle or display behavior. The core inspection suite checks
compiler-level invariants and is included in `cabal test all`.

The SDL demo has an additional self-test that draws into a hidden native window
and exercises font rendering and widget interaction:

```sh
cabal run nano-ui-sdl-demo -- --selftest
```

## Source release checks

Run `cabal check` in each package directory, then `cabal sdist all` from the
workspace. The core, SDL examples, and demo deliberately use `-O2`; Cabal
reports an advisory warning for that option.

Verify the generated archives as well as the checkout. In particular, SDL's
private C headers belong in `extra-source-files`: `include-dirs` alone does
not include them in a source distribution. A clean build of the archives
catches omitted headers, embedded fonts, and other checkout-only assets.
The workspace's local ditto dependency also needs to be available when
building the form package from an archive.

## Where changes belong

Paths below are relative to `packages/`; Haskell modules live under `lib/`.

| Concern | Starting point |
| --- | --- |
| Public UI API | `nano-ui/lib/NanoUI.hs` |
| UI effect, keys, and scopes | `nano-ui/lib/NanoUI/Monad.hs`, `NanoUI/Id.hs` |
| Widget declarations and local state | `nano-ui/lib/NanoUI/Widgets.hs`, `NanoUI/Widgets/`, `NanoUI/State.hs` |
| Local hook storage and invalidation | `nano-ui/lib/NanoUI/Hooks.hs` |
| Frame processing | `nano-ui/lib/NanoUI/Frame.hs`, `NanoUI/Frame/` |
| Layout storage and solving | `nano-ui/lib/NanoUI/Layout/` |
| Context and persistent widget storage | `nano-ui/lib/NanoUI/Context.hs`, `NanoUI/Context/`, `NanoUI/Store.hs` |
| Shared host event loop | `nano-ui/lib/NanoUI/Runner.hs` |
| SDL events and presentation | `nano-ui-sdl/lib/NanoUI/Sdl/Session.hs`, `NanoUI/Sdl/Runner.hs` |
| RGFW session and rendering | `nano-ui-rgfw/lib/NanoUI/Rgfw/Session.hs`, `NanoUI/Rgfw/Gl.hs`, `nano-ui-rgfw/cbits/nano_ui_gl.c` |
| RGFW software rasterizer (tests, profiler) | `nano-ui-rgfw/lib/NanoUI/Rgfw/Render.hs`, `NanoUI/Rgfw/Surface.hs` |
| Native RGFW bindings | `nano-ui-rgfw-bindings/lib/RGFW.hs`, `RGFW/Raw.hs` |
| Diagram conversion and chart construction | `nano-ui-diagrams/lib/NanoUI/Diagrams/`, `NanoUI/Plot/` |
| Form naming, adapters, and evaluation | `nano-ui-form/lib/NanoUI/Form/Named.hs`, `Unnamed.hs`, `Field.hs`, `Runner.hs` |

## Extension conventions

- Keep backend-independent event sequencing in `NanoUI.Runner`. Backends
  supply a `SessionDriver` for event translation, display synchronization,
  and presentation. Loop-local state belongs in recursive arguments;
  shared callback state needs explicit mutable storage.
- In SDL, hook-based and reducer-based apps share `drawFrameWith`. Atlas
  maintenance and retain preparation happen before UI evaluation; damage
  consumption and presentation happen afterwards.
- Form adapters let ditto own naming and validation. `Form.Field` owns stable
  widget scopes, input persistence, and conversion between values and widget
  indices. A labelled control must render its label inside the field scope.
- Give independent forms distinct prefixes. `runNanoForm` binds the prefix
  during evaluation and to the returned view, so delayed and nested views
  retain their owner. `withFormPrefix` restores only the prefix slot; it must
  never restore an old snapshot of the whole widget store.
- Keep form mutation equality and redraw policy in `Form.Backend`. Unchanged
  writes are no-ops. Submission history controls error visibility; the submit
  runner returns a value only on a submission frame.
- Form reset renews the widget scope as well as clearing the value map. The
  private stored-form record keeps that generation with the values; ordinary
  `setFormStore` calls preserve it. Otherwise cached controls can immediately
  write their pre-reset values back into an empty form.
- Preserve widget identity when changing layout or error views. Check several
  frames, including the appearance and disappearance of conditional content.
- Controlled wrappers synchronize the upcoming widget's slot using
  `currentId`; the widget itself consumes it with `nextId`. Composite hooks
  need their own scope before assigning keys to their components.
- Implement local state hooks through `NanoUI.Hooks`. Tab and radio selection
  and packed table sorting use the same integer hook, including its comparison
  against the latest store when a setter runs more than once in a frame.
- Use `Widgets.Behavior.keyboardFocused` before processing keyboard input in
  controls, including text editors. A retained focus ID does not override
  disabled state or modal blocking.
- Table sizing is a pure calculation over rows encoded once with colonnade.
  Use `gridColumnsLay` for keyed table rows, and `sortOn` for stable sorting
  with cached keys in both directions.
- Chart series and legends share resolved colors in `Plot.Chrome`. Legend
  placement only determines coordinates; entry rendering has one path.
- Tab header results use the private `Header` record for selection, closing,
  and scroll geometry. Keep those consumers on the same rendered response.
- Ordinary buttons, menu items, and close controls use `buttonStyledEx` for
  pointer and keyboard activation. Explicitly disabled buttons and tabs keep
  their geometry and identity but skip focus registration and activation.
- Radio options use `selectableItem` for both singleton and multi-option
  groups. Keep ID allocation and scroll-aware hit geometry in the shared
  widget path rather than reproducing them in a control-specific loop.
- Handle host text commits as batches. `TextInput` converts its state once per
  commit, and `TextBuffer` delegates insertion to text-zipper's `insertMany`
  while adapting tabs and updating the preferred column once. Empty commits
  preserve selection and preferred-column state.
- Diagram text extraction is a rendering mode of `NanoUIBackend`; it shares
  sizing, transforms, and styles with the full render and skips path emission.
  A diagram carries backend-specific primitive dictionaries, so never use
  `unsafeCoerce` to switch its backend.
- Dropdown ownership, cached bounds, and outside-click checks share
  `Frame.Select.findOpenDropdown`. Its predicate distinguishes menu-only hits
  from hits on the anchor or menu; reverse-order pointer routing also checks
  overlay eligibility.
- Plan image-atlas placement before allocating a larger pixel buffer. Failed
  placement preserves the existing pixels, UVs, and generation; successful
  growth copies the old rows once before publishing the new state.
- Decimators return points in input order. LTTB's budget counts points and
  preserves endpoints; min/max's budget counts buckets and retains each
  bucket's extrema. Both accept empty input and non-positive budgets.
- SDL font discovery scans the standard directories once per request and
  matches all fallback families against that snapshot. Preserve root order
  so user-installed faces win equal-score ties.
- SDL glyph metrics have one native reader and one record representation.
  Build the ASCII advance and geometry caches from the same measurements;
  metric preparation must remain independent of atlas rasterization.
- Replace native resources only after their replacements have been allocated
  successfully; mask the ownership transfer against asynchronous exceptions.
  Keep an SDL image texture's dimensions and generation in the same state
  record as its pointer, so upload and cleanup publish one coherent value.
- Layout's `textNodeMeasurer` resolves metrics and both measurement operations
  together. Preserve the distinction between monospaced metrics and the host's
  shaping-aware proportional measurement in every layout pass.
- Compose SIMD quad writers from the inline vertex and index primitives.
  `Cases.SIMD` checks the buffer layout and write boundaries; the inspection
  suite checks that the actual library calls inline without retained tuple or
  typeclass overhead.
- Treat damage tracking as part of rendering correctness. A state change can
  require a follow-up frame even when there are no new host events.
- Prefer the existing list/vector APIs and library combinators over parallel
  implementations. Keep specialized storage and native kernels tied to their
  measured hot paths.
- Add regression tests for observable behavior and failure cases. Core cases
  live in `nano-ui/test/integration/Cases/` and are registered in `Main.hs`
  and the package's Cabal `other-modules` list.

Use the repository's `fourmolu.yaml` when formatting Haskell changes.
