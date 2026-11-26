# Changelog

## 0.2.0.0 -- Unreleased

Initial release.

- High-level `RGFW` and low-level `RGFW.Raw` Haskell bindings to the bundled single-header RGFW windowing library, using `hsc2hs` for platform-accurate constants and event fields.
- Covers window creation and OpenGL 3.2 context setup, sizing/positioning/limits/icons/fullscreen, mouse/keyboard/scroll/repeat/enter/leave/refresh events, cursor visibility and standard shapes, clipboard read/write, and cross-thread event wait wakeup (`stopWaitForEvent`).
- Bundles targeted fixes over upstream RGFW (marked `nano-ui:` in `cbits/RGFW.h`) for cross-platform timed event waits, mouse capture on Windows, 1024-entry event queue capacity, safe fullscreen mode handling, and clipboard NUL termination.
- Dual-licensed `MIT AND Zlib` (`LICENSE-RGFW` ships alongside `LICENSE`). Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
