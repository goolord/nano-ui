# Changelog

## 0.1.0.0 -- Unreleased

- `emitChanged` and `emitEdited` take an input of type `Var a -> NanoUI r`,
  such as `slider 0 100`, with the current value; they run it on a temporary
  `Var` and emit the value it leaves. Call sites written against the
  value-style widgets compile unchanged.

- `mapMessages` embeds child components' messages in a parent's message type.

- Extracted `NanoUI.Emit` from core with `NanoUIE msg a` and statically typed
  emission, replacing the runtime-typed context queue.
- Widget adapters, UI lifting/scopes, typed frame and reducer runners, and
  reducer testing helpers.
- Ordered per-frame messages survive rebuilds and are isolated on exceptions.
