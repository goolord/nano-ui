# Custom Canvas Drawing

For custom graphics, visualizers, audio meters, or sparklines, `nano-ui` provides low-level 2D vector path primitives and cached custom widget hooks.

## Vector Paths with `NanoUI.Path`

Import `NanoUI.Path` qualified as `P`:

```haskell
import NanoUI.Path qualified as P

myIcon :: NanoUI ()
myIcon = canvas (fixedWH 48 48) $ \dc -> do
  -- Draw a smooth curved shape
  drawPathWith P.NonZero (P.Solid (colorHex "#38bdf8")) $ do
    P.moveTo 10 10
    P.lineTo 38 10
    P.cubicTo 38 24 24 38 10 38
    P.closePath
```

### Strokes and Caps

```haskell
drawStrokePathWith (P.Stroke 2.0 P.CapRound P.JoinRound Nothing) (P.Solid colorWhite) $ do
  P.circle 24 24 16
```

---

## Content Keys & Caching

Without a content key, custom widget draw operations are rebuilt and diffed every single frame. Providing a `widgetContent` key allows unchanged frames to skip rebuild and repaint completely:

```haskell
-- Hash numbers or data values that the drawing depends on
let key = contentKey [round angle, round volume]

drawingVersioned key $ \dc -> do
  drawGaugeMeter dc angle volume
```

> [!CAUTION]
> A content key must hash **everything the drawing reads** (values, colors, theme). An incomplete key will display stale pixels when unhashed state changes!

---

## Moving Overlays & Drag Ghosts

If an overlay's layout rectangle never changes (e.g. an element pinned at cursor coordinates via `pinAt mx my`), give it an explicit version that incorporates the pointer coordinates:

```haskell
drawingVersioned (contentKey [round mx, round my]) $ do
  drawDragGhost
```
Otherwise, the engine assumes its geometry and ops have not changed since it first appeared.
