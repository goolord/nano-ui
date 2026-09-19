#!/usr/bin/env sh
# Record the README video: runs the SDL demo's scripted tour in a hidden
# window and encodes the frames into OUT/demo.mp4 (default: a temporary
# directory). Upload the video to GitHub by dropping it into an issue or PR
# comment box, and put the user-attachments URL it gives in README.md; the
# video is not committed. Needs ffmpeg with libx264.
set -eu
cd "$(dirname "$0")/.."
out=${1:-$(mktemp -d)}
mkdir -p "$out"
frames=$(mktemp -d)
trap 'rm -rf "$frames"' EXIT
cabal run -v0 nano-ui-sdl-demo -- --record "$frames"
ffmpeg -v error -y -f concat -i "$frames/frames.txt" -r 30 \
  -c:v libx264 -preset slow -crf 24 -pix_fmt yuv420p -movflags +faststart "$out/demo.mp4"
ls -l "$out/demo.mp4"
