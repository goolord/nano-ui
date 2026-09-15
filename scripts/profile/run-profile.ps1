# Re-run the headless frame benchmark and write profiles into this folder.
$ErrorActionPreference = "Stop"
Set-Location $PSScriptRoot

Write-Host "Building nano-ui-profile (late cost centres, -O2)..."
cabal build --enable-profiling --enable-executable-profiling nano-ui-profile

Write-Host "JSON profile for speedscope..."
cabal exec --enable-profiling nano-ui-profile -- +RTS -pj -poprofile-json -RTS

Write-Host "Text profile for hp2ps / reading..."
cabal exec --enable-profiling nano-ui-profile -- +RTS -P -poprofile-time -RTS

Write-Host "Summary:"
python summarize_prof.py
Write-Host ""
Write-Host "Open profile-json.prof in https://www.speedscope.app/"
