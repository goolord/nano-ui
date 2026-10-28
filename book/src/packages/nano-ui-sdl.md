# nano-ui-sdl: SDL3 Backend

`nano-ui-sdl` is the primary desktop backend for `nano-ui`. It uses modern SDL3, SDL3_ttf, FreeType, and HarfBuzz for high-quality font rendering, subpixel anti-aliasing, and platform integration.

## Key Features

- **TrueType Font Shaping**: HarfBuzz shaping with kerning, ligatures, and font fallback.
- **System Font Discovery**: Automatic lookup of installed platform fonts (`FontSearch`).
- **Non-Blocking Native File Dialogs**: Asynchronous file open/save dialogs via `useFileDialog` or `askOpenFileDialog`.
- **Custom Window Chrome**: Build custom client-side window frames and title bars (`DecorationsNone` / `DecorationsFrame`).
- **High-DPI Awareness**: Automatic DPI scaling on Windows, macOS, and Linux.

## Running an SDL Application

```haskell
main :: IO ()
main = runSdlApp opts appView
  where
    opts = defaultSdlOptions
      { sdlWindowSettings = defaultWindowSettings
          { wsTitle = "My Application"
          , wsSize  = Size 1024 768
          }
      , sdlAppFont = FontSearch ["Segoe UI", "SF Pro Text", "Inter"]
      , sdlAppFontSize = 14
      , sdlAppTheme = Just defaultTheme
      }
```

## Native File Dialogs

File dialogs are non-blocking so the GUI never freezes:

```haskell
-- useFileDialog handles polling and restores focus automatically
useFileDialog fileDialogVar $ \selectedPath -> do
  liftIO $ loadFile selectedPath
```

## Window chrome and screenshots

`windowCaption` draws the caption buttons, registers the drag region, and reports a close click for a custom title bar; `sdlWindowDecorations = DecorationsNone` removes the platform frame. `saveScreenshot` and `captureScreenshot` are the SDL-specific pixel helpers. See [Windows & Screenshots](../advanced/window-management.md) for the full pattern, including `wsExitOnCloseRequest` and the confirm-on-close modal.
