# AGENTS.md

Guidance for AI coding agents working **on** nano-ui, the immediate-mode GUI
toolkit for Haskell in this repository: building it, changing the library,
keeping the book and demos honest.

**Writing view code?** Demos, examples, book snippets, tests and app code are
all nano-ui user code. Read the nano-ui skill first:
[`skills/nano-ui/SKILL.md`](skills/nano-ui/SKILL.md) covers the frame model,
widget conventions, `Var` state, identity, layout, styling, input, overlays,
tasks and testing. Its `references/` hold the HTML/CSS/React-to-nano-ui
mapping, the recurring bugs found in real apps, and custom drawing, images,
text editing and windows.

## Repository layout

| Package | What it is |
| --- | --- |
| `packages/nano-ui` | Core: widgets, layout, input, theming, draw list. `lib/NanoUI.hs` is the full public export list |
| `packages/nano-ui-sdl` | SDL3 backend: TrueType fonts, installed-font lookup, native file dialogs, custom title bars |
| `packages/nano-ui-rgfw` | RGFW + OpenGL 3.2 backend with a bundled bitmap font (no system deps) |
| `packages/nano-ui-emit` | Elm-style typed messages: `NanoUIE msg a` views + reducer runners |
| `packages/nano-ui-diagrams` | Line/bar/scatter/area charts (`NanoUI.Plot`) and diagrams-lib drawing |
| `packages/nano-ui-form` | Validated applicative forms on ditto |
| `packages/nano-ui-markdown` | Markdown rendering, incrementally appendable for streamed chat replies |
| `packages/nano-ui-svg` | `DrawOp`-to-SVG export (headless screenshots of any view) |
| `packages/nano-ui-demo` | Example apps, single-topic tutorials, and the book's compiled snippets |
| `book/` | mdBook documentation, deployed by `.github/workflows/deploy-book.yml` |
| `skills/nano-ui/` | The agent skill for writing nano-ui UIs, published with the repo |

`nano-ui` is on Hackage, but this source is usually ahead of the release and
some companion packages are unpublished. Downstream apps (ned, stronghead,
winget-gui) pin this repository as a Cabal `source-repository-package`.

## Building and testing

- GHC 9.10 to 9.14 (development uses 9.14), `default-language: GHC2024`.
  `nix develop` provides GHC, Cabal, SDL3, SDL3_ttf and pkg-config. On Windows,
  install SDL3 and SDL3_ttf 3.2+ through MSYS2 UCRT64 and point
  `PKG_CONFIG_PATH` at its `ucrt64\lib\pkgconfig`
  (`C:\msys64\ucrt64\lib\pkgconfig` on a default install).
- `cabal build -j1 all` and `cabal test -j1 all`. Keep `-j1`: parallel
  optimizing GHC processes exhaust memory.
- One suite with failure details:
  `cabal test nano-ui-test --test-show-details=failures`. The full list of
  suites is in `book/src/getting-started/development.md`.
- GHC 9.10 must keep building. Before using a language extension, syntax or
  `base` API, check it exists in GHC 9.10 / base 4.20 (`OrPatterns` and
  `import M (data P)` do not), and verify with
  `cabal build all -w ghc-9.10.3 --builddir=dist-newstyle-910`.
- `cabal.project` adds `-Werror` for this repository's builds only, and the
  `.cabal` files enable `-Wunused-packages`: remove a stale `build-depends`
  entry along with its last import.

Tests:

- In the integration suite, `simSpec name w h view script` wraps
  `NanoUI.Testing.Sim.runSim`. Import `NanoUI.Testing.Sim` explicitly or
  qualified; its short names clash with locals under `-Wname-shadowing`.
- Merge checks that share a setup into one script rather than adding a spec
  per assertion.
- Headless tests cover layout, state and input. When touching rendering, fonts,
  dialogs or DPI, also run the native suites (`nano-ui-render-test`,
  `nano-ui-demo-test`, the font suites) and the demos.
- Someone is usually using this machine. Do not capture the screen; prefer
  hidden windows (`cabal run nano-ui-sdl-idle -- <scene> hidden`, the
  `--selftest` modes, SDL renderer read-back).

## Library rules

These are decisions already made; do not re-propose the alternatives.

- **Idle costs nothing.** A window left alone must use no CPU or GPU. Never
  keep the loop running by marking the window dirty each frame; use
  `wakeAfter`/`requestWakeAt` and per-frame leases (`keepAnimating`). Diagnose
  with `NANO_LOOP_TRACE=1` (passes per second and why; idle prints nothing).
  Frame times and throughput must not regress.
- **Users never add damage.** Application code never calls `damageWidget` or
  `damageRect`, and docs never tell it to. A change no rect or text diff
  describes repaints the whole window. Only custom-widget authors and backends
  touch damage.
- **Public API stays.** "No caller in this repo" does not make a function dead:
  nano-ui is a library. The line is `exposed-modules` in each `.cabal` file,
  including names an exposed module re-exports from an `other-modules` one.
  Keep the `foo`/`foo'`/`fooWith`/`fooConfigured` families complete.
- **Explicit export lists.** `module X` re-exports only where a public module
  re-exports another public module (the `NanoUI` facade). Internal modules list
  their exports.
- **Do not merge small modules** to save header and import lines; separate
  modules compile in parallel.
- **Do not rebuild existing widgets as custom widgets or adornments.** Custom
  widgets carry fixed per-frame overhead (measured 2 to 3 times slower for
  checkbox, radio and tree). To fold one widget into another, add a style flag
  on the existing node type, as tabs, menus and table headers do, and
  benchmark still frames against `main`.
- **Views never take a backend handle.** Component setup returns a plain state
  record (`newDemo :: IO DemoState`) and `main` runs `runSdlApp opts (demoUi state)`.
  SDL view-side operations find the session on the `Context`; `askSdlEnv` is
  opt-in.
- **Demos read as ordinary Haskell.** Demos and examples never import
  `Effectful`: use concrete `NanoUI a` signatures (`NanoUIEs` for
  row-parameterised configs) and `liftIO`, not `uiIO`.

## Book code

Every Haskell snippet in `book/src` is an `{{#include path:anchor}}` of
compiled code, never written inline; `nano-ui-book-test` fails on inline
Haskell. Chapter snippets live in `packages/nano-ui-demo/book/Book/*.hs` (the
`book-examples` library), and code drawn to a picture lives in
`packages/nano-ui-svg/app/*Examples.hs`, which the glossary renders. mdBook
does not dedent an anchored region, so anchor whole top-level definitions;
put stand-ins for application code below the anchors.

The skill's snippets are plain Markdown and not compiled. When a public name
changes, update `skills/nano-ui/` along with `book/src`, and typecheck any
snippet you touch (loading a scratch module in
`cabal repl nano-ui-demo:lib:book-examples` works).

## Writing style

The book, the skill and UI copy read as if a person wrote them. Generated
prose has a recognizable accent; do not reproduce it.

- No em dashes or en dashes anywhere in the repository. Use a colon, a comma,
  parentheses, or a second sentence. This includes the book strings in
  `Glossary.hs`, which generate `book/theme/nano-ui-banks.js`. A docs edit is
  not done until `rg '[\x{2013}\x{2014}]' book/src skills AGENTS.md` finds
  nothing.
- Drop the "not X, but Y" and "isn't just X, it's Y" cadence, rhetorical
  question openings, and a closing paragraph that restates the section.
- Avoid the stock vocabulary: delve, leverage, seamless, robust, landscape,
  realm, tapestry, testament, elevate, unlock, streamline, game changer,
  cutting edge, at its core, it's worth noting, here's the thing, let's dive
  in.
- Do not pad with three-item lists of abstractions or stack parallel clauses
  to sound thorough. One concrete example beats three vague ones.
- Prefer short sentences, active voice, measurable claims and code. Let a
  section end when its last point is made.
- Vary paragraph and sentence length. Uniform rhythm reads as generated.

Code comments are written for a reader who has never seen this repository:
what the code does and why, in the present tense. History (what it replaced,
which bug led to it) goes in the commit message. Every module has a header and
every exported name a Haddock comment (`cabal haddock` lists what is missing).

## Where to look things up

- `packages/nano-ui/lib/NanoUI.hs`: the public API, grouped and documented.
- `book/src/`: the documentation book. `getting-started/development.md`
  (suites, haddocks, module conventions) and `architecture/index.md` (the
  frame lifecycle and application patterns) matter most for library work.
- `packages/nano-ui-demo/examples/`: one concept per file, plus the full apps
  `Demo.hs`, `Notepad.hs`, `Logs.hs`, `Terminal.hs`.
- Real consumers, for how the API is used in anger:
  [ned](https://github.com/goolord/ned),
  [stronghead](https://github.com/goolord/stronghead),
  [winget-gui](https://github.com/goolord/winget-gui).
