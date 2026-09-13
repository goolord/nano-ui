# Interaction cleanup follow-up

This pass consolidates keyboard eligibility, radio/tab selection writes, and
fallback click dispatch. Frame input handling now uses the monadic helpers
already defined in `NanoUI.Monad`. Modal membership checks reuse a resolved
root, including when filtering the entire focus list.

The expanded disabled-control regression failed before the fix: a text input
or text area with retained focus could still accept typing after being
disabled. It now also checks search and combo fields, including the stored
live text rather than only a combo's returned committed value.

## Verification

- Full workspace build passed.
- All seven suites passed, including compiler inspection.
- SDL hidden-window self-test passed.
- Three interleaved stock-profiler runs used the same setup and `479a655`
  baseline as [the main report](report.md).

Selected medians from that profile:

| Workload | Baseline ms/frame | Current ms/frame | Baseline KiB/frame | Current KiB/frame |
| --- | ---: | ---: | ---: | ---: |
| Full demo, CPU frame | 0.218 | 0.212 | 234.2 | 234.1 |
| 100 text inputs | 0.349 | 0.349 | 468.5 | 468.5 |
| 20 text areas | 0.073 | 0.070 | 109.6 | 109.9 |
| 20 color pickers | 0.746 | 0.752 | 734.9 | 734.9 |
| Table, 200 rows x 5 columns | 0.265 | 0.259 | 557.1 | 556.9 |

No material new runtime regression was identified in these short samples.
The text-area workload shows a small allocation increase of about 0.3%; it
is not allocation-neutral. Timing differences of a few percent should not
be treated as established speedups or slowdowns. These are frame workloads,
not a dedicated measurement of keyboard-event latency.

Raw samples and the summary are under
`/tmp/opencode/nano-quality-perf-results/interaction-*`.
