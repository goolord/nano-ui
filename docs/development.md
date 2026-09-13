# Working on nano-ui

## Build and test

The workspace uses GHC 9.14 and Cabal. `nix develop` supplies the native
dependencies as well as the Haskell toolchain. The current `cabal.project`
also expects a checkout of `ditto` at `../ditto`.

```sh
cabal build all
cabal test all --test-show-details=failures
```

For a shorter feedback cycle, select the affected suite:

```sh
cabal test nano-ui-test --test-show-details=failures
cabal test text-buffer-spec --test-show-details=failures
cabal test nano-ui-form-test --test-show-details=failures
cabal test nano-ui-diagrams-test --test-show-details=failures
cabal test nano-ui-rgfw-test --test-show-details=failures
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
| RGFW session and rendering | `nano-ui-rgfw/lib/NanoUI/Rgfw/Session.hs`, `NanoUI/Rgfw/Render.hs` |
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
- Preserve widget identity when changing layout or error views. Check several
  frames, including the appearance and disappearance of conditional content.
- Controlled wrappers synchronize the upcoming widget's slot using
  `currentId`; the widget itself consumes it with `nextId`. Composite hooks
  need their own scope before assigning keys to their components.
- Treat damage tracking as part of rendering correctness. A state change can
  require a follow-up frame even when there are no new host events.
- Prefer the existing list/vector APIs and library combinators over parallel
  implementations. Keep specialized storage and native kernels tied to their
  measured hot paths.
- Add regression tests for observable behavior and failure cases. Core cases
  live in `nano-ui/test/integration/Cases/` and are registered in `Main.hs`
  and the package's Cabal `other-modules` list.

Use the repository's `fourmolu.yaml` when formatting Haskell changes.
