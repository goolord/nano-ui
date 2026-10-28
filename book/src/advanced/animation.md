# Animations & Timers

Animation in nano-ui is driven by the frame loop, not by a separate clock thread. A tween or spring advances only while the view keeps drawing it, and the loop sleeps when nothing is animating.

## Tweens and springs

`animateTo` moves a value toward a target and returns the current value:

```haskell
import NanoUI

currentWidth <- animateTo (Tween EaseOutQuad 0.25 0) targetWidth
```

A `Transition` is either:

- **`Tween ease duration delay`**, in seconds. `EaseOutQuad`, `EaseInOutCubic`, `EaseOutBack`, and `EaseCubicBezier` are some of the available curves.
- **`Spring params`**, a damped spring. `presetSmooth`, `presetBouncy`, and `presetStiff` are the standard presets:

  ```haskell
  currentX <- animateTo (Spring presetSmooth) targetX
  ```

Springs retarget mid-flight from their current position and velocity, so a moving target does not cause a hitch. `animateToA` does the same for any `Animatable` value — a `V2`, a `Color`, or a numeric type — channel by channel:

```haskell
pos <- animateToA (Spring presetSmooth) (V2 targetX targetY)
```

`animate` (without `To`) animates from one value to another and restarts when it completes, which is useful for a one-shot demonstration but rarely for UI state. Most UI animation is `animateTo`.

## Continuous animation and `keepAnimating`

`pulse` returns a value oscillating in `[0, 1]` with the given period in seconds:

```haskell
opacity <- pulse 1.2       -- one full sweep every 1.2 s
```

Clock-driven values (`pulse`, or anything read from `uiTime`) do not, by themselves, keep the frame loop awake. A widget that draws from them must call `keepAnimating` **with its own response**, every frame it is on screen:

```haskell
pulsingBadge :: NanoUI ()
pulsingBadge = do
  t <- pulse 1.0
  (resp, ()) <- customWidget defaultCustomWidgetSpec
    { widgetLayout = fixedWH 24 24
    , widgetContent = contentKey [t * 100]
    , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $
        drawCircle (V2 (x + w / 2) (y + h / 2)) (6 + 3 * t) (themeAccent (cdcTheme cdc))
    }
  keepAnimating resp
```

The loop goes back to sleep on the first frame that leaves out `keepAnimating`.

> [!CAUTION]
> Never force continuous redraws by marking the context dirty every frame. That creates an unpaced busy loop that pins a CPU and drains a laptop battery. Call `keepAnimating` only in the widget that is actually moving.

## Timers with `wakeAfter`

`wakeAfter` requests one frame after a delay in **seconds**:

```haskell
clock :: NanoUI ()
clock = do
  label =<< currentTimeText
  wakeAfter 1          -- next tick in one second
```

Call it on every frame that still needs the future wake; pending wakes are reset each frame. Use `wakeAfter` for clocks, debounces, and polling on a schedule; use `keepAnimating` for continuous motion. `wakeAfter` costs one frame per interval; `keepAnimating` runs a frame per display refresh.

## What starts and stops an animation

- **`animateTo`** runs while the value differs from the target, and requests frames itself.
- **`pulse`** and other clock-driven drawing need explicit `keepAnimating` (or a `wakeAfter` for a lower frame rate).
- **Leaving the widget out of the view** stops it. A conditional widget that disappears and reappears starts from its stored value, not from a new animation, unless the transition is recreated.
- **Idle means idle.** If nothing animates, no timed wake is pending, and no input arrives, the loop blocks in the OS event queue and uses no CPU.

## Testing animation deterministically

`NanoUI.Testing.runFrame` takes an explicit `inputDeltaTime`, so a test can step animation time without waiting:

```haskell
(_, _, _) <- runFrame ctx (inp {inputDeltaTime = 0.016}) view
(_, _, _) <- runFrame ctx (inp {inputDeltaTime = 0.016}) view
```

Drive the same sequence of frames in the test and the app; do not use real sleeps. For task and stream wake-ups, `newWakeSignal` from `NanoUI.Testing.Harness` makes the timing deterministic.
