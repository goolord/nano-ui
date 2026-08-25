# nano-ui

Purely functional immediate-mode GUI core for Haskell. Backend-agnostic: emits batched vertex/index draw lists in pinned off-heap memory.

## Features

- **SrcLoc IDs** — `HasCallStack` hashing for stable widget identity without manual ID stacks
- **Two-pass flex layout** — measure/position over struct-of-arrays node arena
- **Zero-allocation draw path** — pinned `ForeignPtr` vertex/index arenas reused each frame
- **Damage tracking** — `needsRedraw` gates on input change, `markDirty`, or active animation
- **Headless verification** — ASCII renderer + golden-style tests, no window/GL dependency

## Build

```bash
cabal build
cabal test
cabal run nano-ui-demo
```

## Quickstart

```haskell
import NanoUI

main :: IO ()
main = do
  ctx <- newContext
  let inp = emptyInput { inputWindowSize = Size 800 600 }
  (_, msgs, drawData) <-
    runFrame ctx inp $
      column (defaultLayout { layoutWidth = Grow 1, layoutHeight = Grow 1 }) $
        do
          button "OK"
          label "Hello"
  mapM_ putStrLn (renderASCII 80 24 drawData)
```

## RTS flags

For interactive 60fps apps, use the non-moving GC and latency-tuned flags:

```bash
+RTS -N --nonmoving-gc -qb0
```

The demo and TUI set these via cabal `-with-rtsopts`.

### Terminal backend

```bash
cabal run nano-ui-tui
```

Uses `nano-ui-term` (`runTermApp`, `newTerminalContext`) with 1-cell font metrics.

The backend talks to the platform console directly instead of using vty, because
vty cannot report pointer motion: it rejects the any-motion mouse report and
discards the rest of the pending input when it does, which loses hover and any
click that arrives in the same read. Only GHC boot libraries are needed — `Win32`
on Windows, `unix` elsewhere.

Windows reads native console records, so hover works even on terminals that do
not implement DECSET 1003. POSIX puts the terminal in raw mode and decodes SGR
mouse reports from the byte stream. Frames are diffed cell by cell, so a pointer
movement only rewrites what changed.

## Architecture

```
UI monad → node arena → layout solver → shape lowering → DrawData
                ↑                              ↓
         persistent Context (hot/active/focus, prev rects, store)
```

## Modules

| Module | Role |
|--------|------|
| `NanoUI` | Public API re-export |
| `NanoUI.Monad` | `UI` monad, `emit`, `withKey`, `currentId` |
| `NanoUI.Widgets` | `button`, `checkbox`, `slider`, `textInput`, containers |
| `NanoUI.Frame` | `runFrame`, `needsRedraw` |
| `NanoUI.Draw` | Pinned vertex arena, draw command batching |
| `NanoUI.Layout.Solve` | Two-pass flexbox constraint solver |
| `NanoUI.Render.ASCII` | Headless ASCII rasterizer |
| `NanoUI.Backend.Term` | `runTermApp`, terminal event loop |
| `NanoUI.Term.Cells` | Draw commands and spans to a terminal cell grid |
| `NanoUI.Term.Ansi` | Cell grid to ANSI bytes, diffed against the last frame |
| `NanoUI.Term.Vt` | Incremental VT input decoder, including mouse motion |

## License

MIT
