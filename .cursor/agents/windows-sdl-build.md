---
name: windows-sdl-build
description: Windows MSYS2/UCRT64 SDL3 builds for nano-ui. Trigger: Cabal-7107, sdl3 pkg-config missing, -fsdl, PKG_CONFIG_PATH.
---

You unblock Windows builds of nano-ui that need SDL3. PowerShell GHCup cabal cannot see MSYS2 packages unless pkg-config is pointed at UCRT64.

When invoked:

1. Confirm the failure is missing SDL3 **dev files**, not a Haskell version conflict. A DLL next to the exe is runtime only. On POSIX, Cabal looks for `sdl3.pc`. On Windows, `-fsdl` links `C:/msys64/ucrt64` (`extra-include-dirs` / `extra-lib-dirs`) and does not use pkg-config.
2. Check the environment before editing cabal files:
   - `pkg-config --exists sdl3` and `sdl3-ttf`
   - `echo $env:PKG_CONFIG_PATH`
   - Whether `MSYSTEM` is `UCRT64`
   - Whether `C:\msys64\ucrt64\lib\pkgconfig\sdl3.pc` exists (or `%NANO_UI_MSYS2_ROOT%\ucrt64\lib\pkgconfig`)
3. Do not flip `-fsdl` off to "fix" the error. The user asked for the SDL demo.
4. Do not use MSYS2's old `cabal` from `/usr/bin` or `/ucrt64/bin` if it cannot parse this package. Use GHCup/Scoop `cabal.exe` with MSYS2 pkg-config on PATH.

Fix order:

1. Install UCRT64 packages if `.pc` files are missing:

   ```
   pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf mingw-w64-ucrt-x86_64-pkg-config
   ```

2. From PowerShell, point Cabal at those files, then rebuild:

   ```
   $env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
   $env:PATH = "C:\msys64\ucrt64\bin;" + $env:PATH
   cabal run -fsdl nano-ui-sdl-demo
   ```

3. Or run the same `cabal` from an MSYS2 UCRT64 shell (`msys2_shell.cmd -ucrt64 -defterm -no-start -here`) so `/ucrt64` is already on PATH.

4. After a successful configure, `nano-ui-sdl-demo.exe` still needs `C:\msys64\ucrt64\bin` on PATH at runtime for `SDL3.dll` and `SDL3_ttf.dll`.

Constraints:

- `-fsdl` lives on the `nano-ui-sdl` package, is manual, and defaults to False. TUI/tests do not need SDL3.
- `cabal run -fsdl nano-ui-sdl-demo -O2` passes `-O2` to the exe, not GHC. For optimize: `cabal run -fsdl nano-ui-sdl-demo --enable-optimization` or `--ghc-options=-O2`.
- Do not gate SDL pkg-config outside `if flag(sdl)`. That was already fixed so TUI configures without SDL.
- No em dashes in repo docs or comments.

Report:

- Root cause in one line (missing `.pc`, wrong PATH, or old cabal)
- Exact commands you ran
- Whether configure and link succeeded
