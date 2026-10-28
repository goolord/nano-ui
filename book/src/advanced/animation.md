# Animations & Timers

`nano-ui` includes continuous animation channels, tweening curves, physical spring simulations, and timed wakes.

## Springs and Tweens

- **`animateTo`**: Tweens a value toward a target using an easing curve (`EaseInOut`, `EaseOutQuad`, etc.).
- **`animateToA`**: Drives physical spring animations (`presetSmooth`, `presetBouncy`, `presetStiff`). Supports mid-flight retargeting without velocity hitches.

```haskell
-- Smoothly animate sidebar width toward target
currentWidth <- animateTo (Tween 0.25 EaseOutQuad) targetWidth
```

## Continuous Animations & `keepAnimating`

When drawing continuous animations (such as spinners or pulsing badges), call `keepAnimating` **inside the animating widget**:

```haskell
pulsingBadge :: NanoUI ()
pulsingBadge = do
  t <- pulse 1.0  -- Oscillates between 0.0 and 1.0
  keepAnimating   -- Requests a frame next tick only while on screen
  drawCircleWithRadius t
```

> [!CAUTION]
> Never force continuous redraws by marking the entire context dirty every frame. That creates an unpaced busy loop that drains batteries and pins CPUs.

---

## Timers and Debounces with `wakeAfter`

To schedule future redraws (e.g. for clocks, timers, or tooltips):

```haskell
-- Schedules a frame in 500 milliseconds
wakeAfter 500
```
Call `wakeAfter` on every frame that still requires a wake; pending wakes reset at the end of each frame.
