# Changelog

## 0.2.0.0 -- Unreleased

Initial release.

- Extracted typed Elm-style message emission from `nano-ui` into `NanoUI.Emit`.
- `NanoUIE msg a` is a `NanoUI a` view with a typed message sink (`?emit`) in scope, so every core widget, container, overlay, and callback-taking widget runs inside an emitting view without wrappers or `Typeable`.
- Message adapters `emit`, `emitWhen`, `emitChanged`, `emitEdited`, and `mapMessages` for composing child components into a parent message type.
- Typed frame and reducer runners (`runFrameE`, `reduceFrame`, `reduceMessages`, `reduceUpdates`) and headless reducer testing helpers (`runClickReduce`).
- Per-frame messages stay ordered across view rebuilds, retain queued background messages on view exceptions, and discard messages from a failed pass.
