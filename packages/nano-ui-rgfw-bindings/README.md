# nano-ui-rgfw-bindings

Haskell bindings to [RGFW](https://github.com/ColleagueRiley/RGFW), a single-header C library for native windows, input handling, and OpenGL contexts. The C source is bundled and compiled directly with this package.

`RGFW` provides the higher-level wrapper used by `nano-ui-rgfw`, while `RGFW.Raw` exposes the direct C FFI. Coverage focuses on what `nano-ui` needs: OpenGL windows, events, window geometry and display scale, cursors, and clipboard access.

## Window lifetime

Create, drive, and close every `Window` on the same bound OS thread (where `createWindowGL` makes the OpenGL context current). `Window` values do not attach GC finalizers: close each window exactly once and avoid using it afterward.

```haskell
import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import RGFW

main :: IO ()
main = runInBoundThread $
  bracket (createWindowGL "RGFW" 0 0 640 480 0 3 2)
    (maybe (pure ()) closeWindow) $ \window ->
      case window of
        Nothing -> ioError (userError "Could not create an OpenGL window")
        Just w -> withEventBuffer $ \events -> loop w events
  where
    loop w events = do
      event <- pollEvent w events
      case event of
        EventWindowClose -> pure ()
        EventNone -> waitForEvent 16 >> loop w events
        _ -> loop w events
```

`withEventBuffer` allocates temporary storage valid only inside its callback. `pollEvent` copies each pending event into a Haskell value; drain events until `EventNone`, then wait or render and call `swapBuffersGL`.

Prefer `RGFW` over `RGFW.Raw` unless you need an unchecked C operation that the wrapper does not expose. Event offsets and constants are generated from `RGFW.h` via `hsc2hs`, avoiding FFI calls for field reads and matching each platform's exact C struct layout. Always check an event's tag before reading its union payload. Existing raw accessor signatures are preserved for compatibility, including widening RGFW's byte-sized physical key to `CUInt`.

## Linked system libraries

- **Linux:** X11, Xcursor, Xrandr, Xi
- **macOS:** Cocoa
- **Windows:** `gdi32`, `user32`, `shell32`

## Documentation

- [The nano-ui Book](https://goolord.github.io/nano-ui/book/): Overview, tutorials, and backend documentation.

## License

MIT, except the bundled RGFW header, which is under the zlib license (`LICENSE-RGFW`).
