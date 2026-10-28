# Changelog

## 0.1.0.0 -- Unreleased

- `NanoUIE msg a` is now a `NanoUI a` action with the view's message sink in
  scope as the implicit parameter `?emit`, so every core widget, container,
  overlay and callback-taking widget runs in an emitting view unchanged.
  `NanoUI.Emit` re-exports `NanoUI`; `NanoUI.Emit.Widgets` is a deprecated
  re-export of it. `liftNanoUI`, `withNanoUI`, `withRunInNanoUIE`,
  `emitWhenE`, `emitChangedE` and `emitEditedE` are deprecated identities or
  aliases. Views passed to the runners need a `model -> NanoUIE msg ()`
  signature.

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
