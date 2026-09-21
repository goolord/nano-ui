# nano-ui-rgfw-bindings

Haskell bindings to [RGFW](https://github.com/ColleagueRiley/RGFW), a
single-header C library for windows, input, and OpenGL contexts. The C source
is bundled and compiled with the package.

`RGFW` is the wrapped API that `nano-ui-rgfw` uses; `RGFW.Raw` is the direct
native layer. The bindings cover what the backend needs: OpenGL windows, events,
window size and scale, cursors, and the clipboard.

## Window lifetime

Create, use, and close a window on the same bound OS thread. The OpenGL
context is current on the thread that calls `createWindowGL`. A `Window`
has no finalizer: close it exactly once and do not use it afterwards.

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

`withEventBuffer` owns the temporary event storage. Its pointer is valid only
inside the callback. `pollEvent` copies event data into a Haskell value; keep
polling until `EventNone`, then wait. The example handles events without
drawing; a renderer must draw and call `swapBuffersGL` to present a frame.

`RGFW.Raw` exposes unchecked pointers and C constants. Prefer `RGFW` unless
you need a native operation that the wrapper does not provide.

Event accessors and constants are derived from `RGFW.h` by `hsc2hs`, so field
reads do not cross the FFI and offsets follow the platform's C layout. Inspect
an event's tag before reading a union member. Existing raw accessor signatures
are retained, including widening RGFW's byte-sized physical key to `CUInt`.

## Requirements

Linked system libraries: X11, Xcursor, Xrandr, and Xi on Linux; Cocoa on
macOS; gdi32, user32, and shell32 on Windows.
