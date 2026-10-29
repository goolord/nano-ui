# Development Setup

Working on nano-ui itself. To use it in an application, see the [Quick Start](quickstart.md).

## Setup

This repository's `nix develop` shell provides GHC 9.14, Cabal, SDL3, SDL3_ttf, and pkg-config. Outside it, install the same tools listed under [Quick Start prerequisites](quickstart.md#prerequisites).

## Building and testing

```sh
cabal build -j1 all
cabal test -j1 all
```

> [!WARNING]
> Build with `-j1`. Parallel optimizing GHC processes can exhaust memory. For a hard cap, prefix with `GHCRTS=-M4G`.

### Test suites

| Suite | Covers |
| --- | --- |
| `nano-ui-test` | Widgets, layout, input, focus, damage, and draw lists, frame by frame and headless |
| `text-buffer-spec` | Text buffer and document edits: lines, graphemes, carets, undoable edits |
| `nano-ui-inspection` | Hot paths (quad writers, store and focus access, canvas) still optimize as written |
| `nano-ui-emit-test` | Emitting views, adapters, and reducers |
| `nano-ui-svg-test` | `DrawOp`-to-SVG: clips, gradients, escaping, rasterization |
| `nano-ui-diagrams-test` | Chart and diagrams-lib rendering |
| `nano-ui-form-test` | Form validation, reset, and submission |
| `nano-ui-markdown-test` | Parsing, incremental append against a full parse (QuickCheck), layout |
| `nano-ui-rgfw-test`, `nano-ui-rgfw-bindings-test` | RGFW input translation, bitmap font, frame delivery |
| `nano-ui-font-search-test`, `nano-ui-font-effects-test` | SDL font discovery, shaping, caches, handle lifetimes |
| `nano-ui-render-test` | Native SDL readback: partial damage, clipping, screenshots |
| `nano-ui-demo-test`, `nano-ui-terminal-test` | The demos and the terminal in hidden SDL windows |
| `nano-ui-book-test` | This book: no inline Haskell, `#include` anchors, specimen tags, and an `mdbook` build |

One suite, with failure details:

```sh
cabal test nano-ui-test --test-show-details=failures
```

## Haddocks and Hoogle

Released packages have Haddock pages on Hackage. To build documentation for the current checkout:

```sh
cabal haddock-project --output=haddocks --hoogle
```

The index is `haddocks/index.html`; `--hoogle` also writes Hoogle input files there. One package, with less memory:

```sh
cabal haddock nano-ui --haddock-hoogle
```

Its output is under `dist-newstyle/.../doc/html/nano-ui/`.

To search locally, point `hoogle generate --local=...` at the directory holding the generated `.txt` files. See [Installing Hoogle locally](https://github.com/ndmitchell/hoogle/blob/master/docs/Install.md).

## Book code

The book's snippets are anchored regions of compiled modules: `packages/nano-ui-demo/book/Book/*.hs`, or `packages/nano-ui-svg/app/*Examples.hs` for code drawn to a picture. Include them with an anchored `#include`; `nano-ui-book-test` rejects Haskell written inline.

## Module conventions

The companion packages are listed in [Ecosystem Packages](../packages/index.md). In `packages/nano-ui/lib`, the public API is `NanoUI` and `NanoUI.Widgets.*`; `packages/nano-ui/lib/NanoUI.hs` is the full export list. `NanoUI.Internal.*` is exposed for backends and tests and may change between minor releases.
