# Development Setup

Guidance for building, testing, and developing within the `nano-ui` monorepo. If you only want to *use* nano-ui in an application, the [Quick Start](quickstart.md) is the shorter path.

## Setup

This repository's `nix develop` shell provides GHC 9.14, Cabal, SDL3, SDL3_ttf, and pkg-config. Outside it, install the same tools listed under [Quick Start prerequisites](quickstart.md#prerequisites).

## Building and Testing

```sh
cabal build -j1 all
cabal test -j1 all
```

> [!WARNING]
> Always build with `-j1`. Running multiple optimizing GHC processes in parallel can easily exhaust physical memory on developer machines. For a strict memory cap, prefix with `GHCRTS=-M4G cabal build -j1 ...`.

### Test Suite Catalog

| Test suite | Purpose & Coverage |
| --- | --- |
| `nano-ui-test` | Widgets, layout solver, input handling, focus ring, damage calculation, and draw list generation run headlessly frame-by-frame. |
| `nano-ui-emit-test` | Elm-style typed emissions, widget adapters, reducers, and message queue isolation. |
| `text-buffer-spec` | Multi-line text buffer, piece tables, and undo history. |
| `nano-ui-svg-test` | `DrawOp`-to-SVG headless rendering: precision, clipping masks, gradients, escaping, and rasterization. |
| `nano-ui-inspection` | Compiler-level inspection test ensuring unboxed commands, SIMD writers, and memory alignment. |
| `nano-ui-rgfw-test` | RGFW backend input translation, bitmap font atlas, and OpenGL frame delivery. |
| `nano-ui-font-search-test` | Font discovery, HarfBuzz font shaping, and handle lifetimes under SDL. |
| `nano-ui-render-test` | Native SDL readback of partial-damage triangles, clipping, and screenshot captures. |
| `nano-ui-diagrams-test` | Diagrams-lib conversion, tessellation, and chart rendering. |
| `nano-ui-form-test` | Form validation scopes, reset mechanisms, and submission states. |
| `nano-ui-markdown-test` | Markdown parsing, incremental append vs. full-text parsing (QuickCheck), and rich text layout. |
| `nano-ui-demo-test` | The SDL demo, notepad, log viewer, and IME composition in hidden windows. |

To run a single test suite with detailed failure reports:
```sh
cabal test nano-ui-test --test-show-details=failures
```

## Haddocks and Hoogle

Released packages have Haddock pages on Hackage. To build documentation for the current checkout:

```sh
cabal haddock-project --output=haddocks --hoogle
```

This writes the HTML documentation under `haddocks/` with an index at `haddocks/index.html`. The `--hoogle` flag also writes Hoogle input files in that directory.

To build one package instead:

```sh
cabal haddock nano-ui --haddock-hoogle
```

Its output is under `dist-newstyle/.../doc/html/nano-ui/`. Building every package at once can use a lot of memory; the one-package command is the lighter option.

To search the generated Hoogle data locally, point `hoogle generate --local=...` at the directory containing the generated `.txt` files, then search with the same database. See [Installing Hoogle locally](https://github.com/ndmitchell/hoogle/blob/master/docs/Install.md) for the local database options.

## Repository Architecture

```
packages/
  nano-ui/             # Core toolkit: widgets, layout, state, drawing, text arena
  nano-ui-sdl/         # SDL3 + HarfBuzz + native dialogs backend
  nano-ui-rgfw/        # RGFW + OpenGL 3.2 backend
  nano-ui-emit/        # Elm architecture (NanoUIE msg a)
  nano-ui-diagrams/    # Charts and diagrams-lib integration
  nano-ui-form/        # Validated applicative forms (ditto)
  nano-ui-markdown/    # Incremental Markdown renderer
  nano-ui-svg/         # DrawOp-to-SVG headless exporter
  nano-ui-demo/        # Example applications and tutorials
```

### Module Conventions

Core modules reside under `packages/nano-ui/lib`:
- Public API modules are directly under `NanoUI` and `NanoUI.Widgets.*`.
- Implementation internals live under `NanoUI.Internal.*`. Internal modules are exposed for backends and tests, but are not guaranteed to be stable across minor releases.
