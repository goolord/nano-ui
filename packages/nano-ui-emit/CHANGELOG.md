# Changelog

## 0.1.0.0 -- Unreleased

- Complete wrapped widget API in `NanoUI.Emit.Widgets` (re-exported by `NanoUI.Emit`),
  providing direct `NanoUIE msg` actions for all containers, inputs, buttons,
  text, overlays, and scopes exported by core `nano-ui`. Views no longer need
  to manually `liftNanoUI` or wrap with `withNanoUI`.
- Added `emitWhenE`, `emitChangedE`, and `emitEditedE` adapters operating directly
  on wrapped `NanoUIE` widgets.

- `emitChanged` and `emitEdited` take an input of type `Var a -> NanoUI r`,
  such as `slider 0 100`, with the current value; they run it on a temporary
  `Var` and emit the value it leaves.

- `mapMessages` embeds child components' messages in a parent's message type.

- Extracted `NanoUI.Emit` from core with `NanoUIE msg a` and statically typed
  emission, replacing the runtime-typed context queue.
- Widget adapters, UI lifting/scopes, typed frame and reducer runners, and
  reducer testing helpers.
- Ordered per-frame messages survive rebuilds and are isolated on exceptions.
- The library imports `NanoUI.Internal` modules, which change without
  notice, so it allows only nano-ui 0.2.0.x (`>=0.2.0 && <0.2.1`).
