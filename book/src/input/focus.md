# Focus, Navigation & IME

Focus decides which widget receives keyboard input. nano-ui tracks one focused widget per window and resolves it against the frame's layout, after the view has run.

## Moving focus with the keyboard and mouse

- **Click** a text field, select, slider, or focusable custom widget to focus it. Clicking elsewhere unfocuses the current field.
- **Tab** moves to the next focusable widget; **Shift+Tab** moves to the previous one.
- **Programmatic focus** uses the widget's id:

  ```haskell
  resp <- searchInput' "Find" query
  whenM (shortcut (cmdOrCtrl <> key 'f')) (requestFocus (respId resp))
  ```

- **`focusNext` / `focusPrevious`** behave like Tab. **`clearFocus`** drops focus at the end of the frame. **`holdFocus wid`** claims focus every frame for a view that handles input itself (it also consumes Tab, so an editor can indent with it).
- **`releaseFocus wid`** (from `NanoUI.Monad`) clears focus immediately, mid-frame, preserving text selection and menu state. Most applications want `clearFocus` instead.

`requestFocus`, `focusNext`, `focusPrevious`, and `clearFocus` are **requests applied at the end of the frame**, against that frame's layout. The last request wins.

## Focus timing

Because focus is applied at frame end, a focus move is visible on the next frame:

```haskell
-- Frame N: the click or shortcut requests focus.
resp <- textInput' draft
whenM (shortcut (cmdOrCtrl <> key 'l')) (requestFocus (respId resp))

-- Frame N+1: the focus ring is drawn and isFocused answers True.
```

The request can name a widget declared later in the same pass — for example, `currentId` just before the widget:

```haskell
whenM (shortcut (cmdOrCtrl <> key 'l')) (requestFocus =<< currentId)
textInput address
```

But it **cannot** name a widget that is not declared on this pass at all. A widget that appears only after a state change needs a delayed request:

```haskell
editing <- useVar False
focusPending <- useVar False

showField <- readVar editing
scope $
  if showField
    then do
      fieldR <- textInput' draft
      whenM (readVar focusPending) $ do
        requestFocus (respId fieldR)
        writeVar focusPending False
    else label "Not editing"

whenM (button "Edit") $ do
  writeVar editing True
  writeVar focusPending True      -- request it on the pass that declares the field
```

The same pattern appears in [Build a Todo List](../tutorials/todo-list.md).

> [!WARNING]
> Query focus at the **end** of the view, not the top. The node arena is reset before the view runs, so `isFocused` at the top always answers `False`. Keyboard shortcuts that depend on focus belong after the widgets have executed. This one mistake caused real bugs in `winget-gui` (Ctrl+A select-all firing while typing) and `ned`.

## What cannot be focused

A focus request is ignored for a widget that Tab would not stop at on that frame:

- a disabled widget (inside `disabledWhen`);
- a widget behind an open `modal`;
- a widget that is not declared on this pass;
- a group response that is not itself a Tab stop (for example, a radio group's response names its last option, not the group).

Focus is not preserved for a widget that stops being declared. A widget in an inactive tab is not called on that frame, so it cannot keep focus; give the tab's editable state a `Var` or `VarMap` entry if it must survive, and request focus again when the tab is shown.

## Focus rings and reachability

The focus ring is themed by `themeFocusRing`. Built-in widgets draw it themselves; custom widgets that are focusable (`widgetFocusable = True` in `CustomWidgetSpec`) should draw their own or use the standard helpers.

`disabledWhen cond subtree` dims the subtree and blocks interaction without changing layout or state. A disabled widget can still be visible and can still hold state; it cannot be clicked, focused, or activated by a shortcut.

## IME composition

Input method editors (Japanese, Chinese, Korean, and others) compose text before committing it. Built-in text fields handle the whole cycle: they display the active composition at the caret, accept the commit, and suppress key events until composition finishes, so shortcuts do not fire mid-composition.

A custom widget that reads committed characters from `inputChars` must opt in every frame it accepts text:

```haskell
-- wid: the widget's id; caretRect: where the candidate window should open.
preedit <- useInputMethod wid InputNormal caretRect
for_ preedit drawCompositionAtCaret
```

The signature is `useInputMethod :: WidgetId -> InputPurpose -> Rect -> NanoUI (Maybe Composition)`. Pass the caret's rectangle, not the whole widget; the backend places the OS candidate window there. The returned `Composition` is the in-progress text the widget should draw at its caret (typically underlined). `useInputMethod` checks focus itself, so a separate `isFocused` guard is unnecessary. Without the call, composed text may appear at the wrong place, the candidate window may not follow the caret, and the IME's keys are not routed to the widget.

When reading raw input for a custom text widget:

- `inputChars` — committed text for this frame;
- `inputKeys` — pressed keys, including repeats (suppressed during composition);
- `inputKeysNew` — initial presses only;
- `inputKeysHeld` / `inputKeysReleased` — held and released keys;
- `inputComposition` — the active composition, if any.

A custom text widget that implements its own focus handling should call `holdFocus wid` each frame while it is active. The terminal demo in `packages/nano-ui-demo/examples/Terminal.hs` is a complete example of a custom widget that owns text input.

## Testing focus

Headless frame tests can drive Tab and clicks like a user:

```haskell
ctx <- newContext
(_, _, _) <- runFrame ctx input1 view
(_, _, _) <- runFrame ctx input2 view      -- focus changes apply on the next frame
```

Use `warmup2` from `NanoUI.Testing.Harness` before targeting a widget by `respRect`; geometry is from the previous frame. Focus is part of the frame result, so assert on the second frame, not the frame that requested the move.

For the keyboard side — chords, suppression, and menu bindings — see [Keyboard Shortcuts](shortcuts.md).
