module Cases.Theming (tests) where

import Spec
import Data.Text (Text)
import NanoUI.Internal.Context (Context (..))

tests :: [Spec]
tests =
  [ spec "warning-text" runWarningTextTest
  , spec "warning-button" runWarningButtonTest
  , spec "warning-rich-text" runWarningRichTextTest
  , spec "warning-contrast" runWarningContrastTest
  , spec "follow-system-theme" runFollowSystemThemeTest
  , spec "set-theme-stops-following" runSetThemeStopsFollowingTest
  , spec "system-appearance-view" runSystemAppearanceViewTest
  , spec "follow-system-theme-ui" runFollowSystemThemeUiTest
  , spec "follow-system-restyles-warning" runFollowSystemRestylesWarningTest
  ]

inp :: Input
inp = withInput 400 300

-- | The foreground of the span showing @txt@.
spanFg :: Text -> [(Rect, Text, Color, Color, Rect)] -> Maybe Color
spanFg txt spans = lookup txt [(t, fg) | (_, t, fg, _, _) <- spans]

-- | The colours of the quads that overlap @r@.
fillsIn :: Rect -> [(Rect, Color)] -> [Color]
fillsIn r quads = [c | (q, c) <- quads, rectIntersect q r /= Nothing]

-- | The last of @n@ frames of @ui@: its result, the quads drawn, and the text spans.
frames :: Int -> Context -> NanoUI a -> IO (a, [(Rect, Color)], [DemoSpan])
frames n ctx ui = do
  replicateM_ (n - 1) (warmup ctx inp ui)
  (a, _, draw, _) <- runFrame ctx inp ui
  (,,) a <$> drawQuads draw <*> collectTextSpans ctx

-- | Warning text takes the theme's warning colour, as danger text its red, and fades disabled.
runWarningTextTest :: Context -> IORef Int -> IO ()
runWarningTextTest ctx failed = do
  theme <- getTheme ctx
  let amber = themeWarning theme
  (_, quads, spans) <- frames 2 ctx . column $
    labelWith fontWarning "Careful" >> danger "Broken" >> disabledWhen True (labelWith fontWarning "Faded")
  assertJust failed (spanRect "Careful" spans) $ \careful -> assert failed (amber `elem` fillsIn careful quads)
  assertEq failed (map (`spanFg` spans) ["Careful", "Broken", "Faded"]) (map Just [amber, themeRed theme, themeWarning (disabledTheme theme)])
  assert failed (amber `notElem` [themeRed theme, themeWarning (disabledTheme theme)])

-- | 'warning' fills buttons with the warning colour, under a label that reads on it.
runWarningButtonTest :: Context -> IORef Int -> IO ()
runWarningButtonTest ctx failed = do
  theme <- getTheme ctx
  let amber = themeWarning theme
  ((w, d, p), quads, spans) <- frames 2 ctx . column $
    (,,) <$> styled warning (button' "Careful") <*> styled destructive (button' "Delete") <*> button' "Plain"
  let fills r = fillsIn (respRect r) quads
  assertEq failed (True, True, False) (amber `elem` fills w, themeRed theme `elem` fills d, amber `elem` fills p)
  assertEq failed (Just (readableOn theme amber), amber) (spanFg "Careful" spans, styleBg (themeButton (warning theme)))
  -- Disabled, it fades like any tinted button.
  offTheme <- warmup2 ctx inp (column (disabledWhen True (styled warning uiTheme)))
  assertEq failed (styleBg (themeButton offTheme)) (styleBg (themeButton (disabledTheme (warning theme))))

-- | A rich-text piece in the warning font variant takes the warning colour.
runWarningRichTextTest :: Context -> IORef Int -> IO ()
runWarningRichTextTest ctx failed = do
  amber <- themeWarning <$> getTheme ctx
  forM_ [(["Plain text"], False), (["Plain ", inlineWith fontWarning "text"], True)] $ \(pieces, warned) -> do
    (_, quads, _) <- frames 2 ctx (column (richText pieces))
    assertEq failed warned (amber `elem` map snd quads)

-- | Each built-in theme's warning colour, and a label on it, has 4.5:1 contrast.
runWarningContrastTest :: Context -> IORef Int -> IO ()
runWarningContrastTest _ failed =
  forM_ themes $ \(name, t) -> do
    let amber = themeWarning t
    assertEq failed (name, True, True, True) (name, contrastRatio amber (themeWindow t) >= 4.5, contrastRatio (readableOn t amber) amber >= 4.5, amber /= themeRed t)
  where
    themes =
      zip ["default", "default light", "tomorrow night", "tomorrow light", "tomorrow midnight", "base16 night", "base16 light" :: String]
        [defaultTheme, defaultLightTheme, tomorrowNightMinDarkTheme, tomorrowMinLightTheme, tomorrowMidnightMinDarkTheme, themeFromBase16 base16TomorrowNight, themeFromBase16 base16TomorrowLight]

-- | Following the system switches theme on a change, waking and repainting; a repeat is free.
runFollowSystemThemeTest :: Context -> IORef Int -> IO ()
runFollowSystemThemeTest ctx failed = do
  let ui = column (button' "Go")
      buttonIn theme (resp, quads, _) = assert failed (styleBg (themeButton theme) `elem` fillsIn (respRect resp) quads)
  followSystemTheme ctx defaultLightTheme defaultTheme
  getTheme ctx >>= assertEq failed defaultLightTheme
  frames 2 ctx ui >>= buttonIn defaultLightTheme
  wakes <- newIORef (0 :: Int)
  setWakeLoop ctx (modifyIORef' wakes (+ 1))
  setSystemAppearance ctx (Just AppearanceDark)
  getTheme ctx >>= assertEq failed defaultTheme
  getSystemAppearance ctx >>= assertEq failed (Just AppearanceDark)
  readIORef wakes >>= assert failed . (> 0)
  checkIdleFullDamage failed ctx inp inp ui
  frames 1 ctx ui >>= buttonIn defaultTheme
  -- The same report again: no frame, no wake.
  writeIORef wakes 0
  setSystemAppearance ctx (Just AppearanceDark)
  isDirty ctx >>= assert failed . not
  readIORef wakes >>= assertEq failed 0
  -- Back to light, and to a system that cannot tell, which is light too.
  forM_ [[Just AppearanceLight], [Just AppearanceDark, Nothing]] $ \reports ->
    mapM_ (setSystemAppearance ctx) reports >> getTheme ctx >>= assertEq failed defaultLightTheme

-- | After 'setTheme' a system switch keeps the theme, but still repaints.
runSetThemeStopsFollowingTest :: Context -> IORef Int -> IO ()
runSetThemeStopsFollowingTest ctx failed = do
  let ui = column (label "x")
  setSystemAppearance ctx (Just AppearanceDark)
  followSystemTheme ctx defaultLightTheme defaultTheme
  getTheme ctx >>= assertEq failed defaultTheme
  setTheme ctx tomorrowMidnightMinDarkTheme
  warmup ctx inp ui
  setSystemAppearance ctx (Just AppearanceLight)
  getTheme ctx >>= assertEq failed tomorrowMidnightMinDarkTheme
  checkIdleFullDamage failed ctx inp inp ui

-- | A view reads the appearance, and sees a change on the frame after it.
runSystemAppearanceViewTest :: Context -> IORef Int -> IO ()
runSystemAppearanceViewTest ctx failed = do
  let ui = column (systemAppearance >>= label . maybe "unknown" (\a -> if a == AppearanceDark then "dark" else "light"))
  warmup ctx inp ui
  collectTextSpans ctx >>= assertSpansHas failed "unknown"
  setSystemAppearance ctx (Just AppearanceDark)
  needsRedraw ctx inp inp >>= assert failed
  warmup ctx inp ui
  collectTextSpans ctx >>= assertSpansHas failed "dark"

-- | A view following the system every frame settles; 'setUiTheme' stops following.
runFollowSystemThemeUiTest :: Context -> IORef Int -> IO ()
runFollowSystemThemeUiTest ctx failed = do
  let following = column (followSystemThemeUi defaultLightTheme defaultTheme >> button "Go")
  -- The frame that switches the theme asks for a full repaint after it.
  _ <- warmup2 ctx inp following
  getTheme ctx >>= assertEq failed defaultLightTheme
  warmup ctx inp following
  isDirty ctx >>= assert failed . not
  takeDamage ctx >>= assert failed . damageIsEmpty
  setSystemAppearance ctx (Just AppearanceDark)
  getTheme ctx >>= assertEq failed defaultTheme
  warmup ctx inp following
  getTheme ctx >>= assertEq failed defaultTheme
  isDirty ctx >>= assert failed . not
  -- A view that sets a theme of its own takes it, and keeps it.
  warmup ctx inp (setUiTheme tomorrowMinLightTheme >> button "Go")
  setSystemAppearance ctx (Just AppearanceLight)
  getTheme ctx >>= assertEq failed tomorrowMinLightTheme
  readIORef (ctxSystemThemes ctx) >>= assertEq failed Nothing

-- | A system switch restyles warning labels, rich text and buttons on the next frame.
runFollowSystemRestylesWarningTest :: Context -> IORef Int -> IO ()
runFollowSystemRestylesWarningTest ctx failed = do
  let ui = column (labelWith fontWarning "Careful" >> richText [inlineWith fontWarning "Rich"] >> styled warning (button' "Go"))
      amberIn theme (resp, quads, spans) = do
        assertEq failed (Just (themeWarning theme)) (spanFg "Careful" spans)
        assert failed (themeWarning theme `elem` fillsIn (respRect resp) quads)
        pure quads
  followSystemTheme ctx defaultLightTheme defaultTheme
  _ <- frames 2 ctx ui >>= amberIn defaultLightTheme
  setSystemAppearance ctx (Just AppearanceDark)
  quads <- frames 1 ctx ui >>= amberIn defaultTheme
  assert failed (themeWarning defaultLightTheme `notElem` map snd quads)
