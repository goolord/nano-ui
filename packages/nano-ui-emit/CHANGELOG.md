# Changelog

## 0.1.0.0 -- Unreleased

- Extracted `NanoUI.Emit` from core with `NanoUIE msg a` and statically typed
  emission, replacing the runtime-typed context queue.
- Widget adapters, UI lifting/scopes, typed frame and reducer runners, and
  reducer testing helpers.
- Ordered per-frame messages survive rebuilds and are isolated on exceptions.
