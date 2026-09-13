# Range-loop allocation audit

Inspected the remaining literal `forM_ [` sites using GHC 9.14.1 and the
project's optimized build configuration (`-O2`). The initial search found
nine executable library sites across seven functions; the other matches
were in tests, examples/profiling programs, or a comment.

## Findings

| Function | Optimized Core before changes | Action |
| --- | --- | --- |
| `NanoUI.Rgfw.Render.fillTrianglePx` | The outer y range fuses; the inner x range remains an `eftInt` list shared across rows. | Replace both ranges with strict numeric loops. |
| `NanoUI.Widgets.ColorPicker.drawChecker` | The outer row range fuses; the shared column range explicitly constructs `: (I# ...)` cells. | Replace both ranges with strict numeric loops. |
| `NanoUI.Layout.Arena.ensureAxisSnapshot` | Numeric loop using `readArray#` and an unboxed counter. | Keep the range syntax. Array allocation inside the body is the capacity-growth operation itself. |
| `NanoUI.Frame.Window.forFloatingNode` | Numeric loop over node indices. | Keep the range syntax. |
| `NanoUI.Frame.Input.finalizePointerRelease` | Numeric loop over node indices. | Keep the range syntax. The full arena scan is a separate algorithmic cost. |
| `NanoUI.Frame.TextEdit.drawTextAreaSelectionLines` | Numeric loop over selected rows. | Keep the range syntax. |
| `NanoUI.Monad.burstNextIds` | Numeric loop; repeated `readMutVar#`/`writeMutVar#` operations and `IdContext` updates remain in its body. | Keep this benchmark workload; its range list is not the allocation source. |

The surviving inner lists are shared once per triangle/checkerboard, not
allocated once per pixel. Each row still traverses the shared list. Replacing
them removes list construction and traversal without changing drawing order,
pixel coverage, or clipping. The RGFW counters stop before incrementing an
inclusive upper bound, preserving termination even at `maxBound :: Int`.

Tests and example case lists remain ordinary lists. In particular, a small
literal collection of test cases is not the same problem as a retained pixel
coordinate range.

## Verification

Core was dumped from the actual libraries, rather than from a simplified
standalone example:

```sh
cabal build lib:nano-ui lib:nano-ui-rgfw \
  --builddir=/tmp/opencode/range-loop-audit/build -j1 -v0 \
  --ghc-options="-ddump-simpl -ddump-to-file -dsuppress-all -dumpdir /tmp/opencode/range-loop-audit/"
```

After rebuilding the changed modules, both rendering loops use numeric
workers (`goRows`/`goCols`), with no corresponding range-list producers.
This verifies removal of the identified allocations; no timing speedup is
claimed.

The core and RGFW test suites pass. RGFW now also checks triangle coverage
pixel-for-pixel for normal and reversed winding, partial clipping, an empty
clip, a clip outside the triangle, and degenerate geometry.
