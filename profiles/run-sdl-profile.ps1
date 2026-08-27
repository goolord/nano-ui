# Profile the SDL demo frame loop (hidden window, SdlDemo UI). Windows + MSYS2 UCRT64 only.
$ErrorActionPreference = "Stop"
Set-Location $PSScriptRoot

if (-not $IsWindows) {
  Write-Error "run-sdl-profile.ps1 requires Windows with MSYS2 UCRT64. Set PKG_CONFIG_PATH and run cabal build/exec manually on other platforms."
  exit 1
}

$env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
$env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"

Write-Host "Building nano-ui-sdl-profile (profiling, -O2)..."
cabal build -fsdl --enable-profiling --enable-library-profiling --enable-executable-profiling --ghc-options=-O2 nano-ui-sdl-profile

Write-Host "JSON profile for speedscope..."
cabal exec -fsdl --enable-profiling nano-ui-sdl-profile -- +RTS -pj -poprofile-sdl-json -RTS

Write-Host "Text profile..."
cabal exec -fsdl --enable-profiling nano-ui-sdl-profile -- +RTS -P -poprofile-sdl-time -RTS

Write-Host "Summary:"
python summarize_prof.py profile-sdl-json.prof
Write-Host ""
Write-Host "Open profile-sdl-json.prof in https://www.speedscope.app/"
