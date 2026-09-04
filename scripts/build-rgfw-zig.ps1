param(
    [string]$Optimize = "-O3"
)

$ErrorActionPreference = "Stop"
$root = Resolve-Path "$PSScriptRoot\..\packages\nano-ui-rgfw-bindings"
Write-Host "Building RGFW static library with Zig compiler..." -ForegroundColor Cyan

& zig cc -c "$root\cbits\RGFW.c" -I"$root\cbits" $Optimize -o "$root\cbits\RGFW.o"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

& zig ar rcs "$root\cbits\librgfw.a" "$root\cbits\RGFW.o"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host "Successfully built $root\cbits\librgfw.a with Zig!" -ForegroundColor Green
