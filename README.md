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

The demo and test suite set these via cabal `-with-rtsopts`.

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

## License

MIT
