# Form reset follow-up

The reset regression test failed before the fix: clearing the form value map
allowed a cached checkbox to write its old value back on the next frame.
Text areas had the same issue through their cached editing buffer.

The form backend now keeps a private widget generation with the form values.
Reset renews the widget scope; ordinary form-data writes preserve the
generation. The public `FormStateStore` representation is unchanged. Tests
cover checkbox reset, text-area reset, isolation of another form, and writing
form data after reset without reviving the previous widget state.

## Steady-state allocation check

Same headless probe, compiler and CPU affinity as [the main report](report.md).
Three interleaved runs against `479a655`, 20 warmups and 200 measured frames.

| Workload | Pre-refactor KiB/frame | `6d96969` KiB/frame | Reset follow-up KiB/frame |
| --- | ---: | ---: | ---: |
| One form, one field | 16.3 | 16.9 | 17.0 |
| One form, 100 fields | 725.0 | 725.6 | 725.7 |
| 100 independent forms | 577.8 | 641.6 | 647.1 |

The first implementation deferred payload lookup and projection, adding about
32 bytes per field. Forcing those getters within IO removed that cost. The
remaining increase over `6d96969` is roughly 56 bytes per form, rather than
per field, for the generation lookup and scoped key.

The 100-field form measured 0.373 ms/frame versus the baseline's 0.371 ms;
100 independent forms measured 0.291 ms versus 0.275 ms. These are short-run
timings, not precise speedup/slowdown guarantees. The scope overhead described
in the main report still applies.

This check measures steady-state rendering, not repeated-reset retention of
retired widget keys by the core context. Raw results are in
`/tmp/opencode/nano-quality-perf-results/reset-strict-*`.
