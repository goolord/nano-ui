module Cases.Theming (tests) where

import Spec
import Data.Text (Text)
import Data.Maybe (isNothing)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (NodeType (NodeText), arenaCount, getNodeType, getStyleIdx)

tests :: [Spec]
tests =
  [ spec "warning-text" runWarningTextTest
  , spec "warning-button" runWarningButtonTest
  , spec "warning-rich-text" runWarningRichTextTest
  , spec "tone-contrast" runToneContrastTest
  , spec "tones" runTonesTest
  , spec "tone-with-face" runToneWithFaceTest
  , spec "theme-appearance" runThemeAppearanceTest
  , spec "follow-system-theme" runFollowSystemThemeTest
  , spec "set-theme-stops-following" runSetThemeStopsFollowingTest
  , spec "system-appearance-view" runSystemAppearanceViewTest
  , spec "view-picks-theme" runViewPicksThemeTest
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

-- | Warning and danger text use the theme's warning and danger colours, and fade when disabled.
runWarningTextTest :: Context -> IORef Int -> IO ()
runWarningTextTest ctx failed = do
  theme <- getTheme ctx
  let amber = themeWarning theme
  (_, quads, spans) <- frames 2 ctx . column $
    labelWith (fontTone Warning) "Careful" >> danger "Broken" >> disabledWhen True (labelWith (fontTone Warning) "Faded")
  assertJust failed (spanRect "Careful" spans) $ \careful -> assert failed (amber `elem` fillsIn careful quads)
  assertEq failed (map (`spanFg` spans) ["Careful", "Broken", "Faded"]) (map Just [amber, themeDanger theme, themeWarning (disabledTheme theme)])
  assert failed (amber `notElem` [themeDanger theme, themeWarning (disabledTheme theme)])

-- | 'warning' fills buttons with the warning colour, with a label readable on it.
runWarningButtonTest :: Context -> IORef Int -> IO ()
runWarningButtonTest ctx failed = do
  theme <- getTheme ctx
  let amber = themeWarning theme
  ((w, d, p), quads, spans) <- frames 2 ctx . column $
    (,,) <$> styled warning (button' "Careful") <*> styled destructive (button' "Delete") <*> button' "Plain"
  let fills r = fillsIn (respRect r) quads
  assertEq failed (True, True, False) (amber `elem` fills w, themeDanger theme `elem` fills d, amber `elem` fills p)
  assertEq failed (Just (readableOn theme amber), amber) (spanFg "Careful" spans, styleBg (themeButton (warning theme)))
  -- Disabled, it fades like any tinted button.
  offTheme <- warmup2 ctx inp (column (disabledWhen True (styled warning uiTheme)))
  assertEq failed (styleBg (themeButton offTheme)) (styleBg (themeButton (disabledTheme (warning theme))))

-- | A rich-text piece in the warning tone takes the warning colour.
runWarningRichTextTest :: Context -> IORef Int -> IO ()
runWarningRichTextTest ctx failed = do
  amber <- themeWarning <$> getTheme ctx
  forM_ [(["Plain text"], False), (["Plain ", inlineWith (fontTone Warning) "text"], True)] $ \(pieces, warned) -> do
    (_, quads, _) <- frames 2 ctx (column (richText pieces))
    assertEq failed warned (amber `elem` map snd quads)

-- | The built-in themes, by name.
builtInThemes :: [(String, Theme)]
builtInThemes =
  zip ["default", "default light", "tomorrow night", "tomorrow light", "tomorrow midnight", "base16 night", "base16 light"]
    [defaultTheme, defaultLightTheme, tomorrowNightMinDarkTheme, tomorrowMinLightTheme, tomorrowMidnightMinDarkTheme, themeFromBase16 base16TomorrowNight, themeFromBase16 base16TomorrowLight]

-- | Each built-in theme's success, warning and danger colours reach 4.5:1
-- contrast on its window colour, and a label reaches it on the warning colour.
runToneContrastTest :: Context -> IORef Int -> IO ()
runToneContrastTest _ failed =
  forM_ builtInThemes $ \(name, t) -> do
    let amber = themeWarning t
        readable c = contrastRatio c (themeWindow t) >= 4.5
    assertEq failed (name, True, True, True, True) (name, all readable [themeSuccess t, amber, themeDanger t], contrastRatio (readableOn t amber) amber >= 4.5, amber /= themeDanger t, themeSuccess t /= themeDanger t)

-- | Each tone's colour comes from the theme, and its buttons match the old tinted button names.
runTonesTest :: Context -> IORef Int -> IO ()
runTonesTest _ failed = do
  let t = tomorrowMinLightTheme
  assertEq failed [themeAccent t, themeMuted t, themeSuccess t, themeWarning t, themeDanger t] (map (toneColor t) [minBound .. maxBound])
  assert failed (and [f t == tone c t | (f, c) <- [(primary, Accent), (destructive, Danger), (success, Success), (warning, Warning)]])
  assert failed (styleBg (themeButton (tone Danger t)) == themeDanger t)
  assert failed (fontMuted defaultLayout == fontTone Muted defaultLayout && fontDanger defaultLayout == fontTone Danger defaultLayout)
  -- Fading a theme fades its tones.
  assert failed (and [toneColor (disabledTheme t) c /= toneColor t c | c <- [Success, Warning, Danger]])

-- | A tone combines with a face in either order, and a colour-only variant
-- keeps the regular face: the node stores face and tone separately.
runToneWithFaceTest :: Context -> IORef Int -> IO ()
runToneWithFaceTest ctx failed = do
  theme <- getTheme ctx
  (_, _, spans) <- frames 2 ctx . column $ do
    labelWith (fontMono . fontTone Warning) "mono warning"
    labelWith (fontTone Warning . fontMono) "warning mono"
    labelWith fontMuted "muted"
    labelWith (\l -> l {layoutFontVariant = FontDanger}) "legacy danger"
    labelWith (fontTone Danger . fontColor (colorRGBA 1 2 3 255)) "own colour"
  assertEq failed (map Just [themeWarning theme, themeWarning theme, themeMuted theme, themeDanger theme, colorRGBA 1 2 3 255]) (map (`spanFg` spans) ["mono warning", "warning mono", "muted", "legacy danger", "own colour"])
  assert failed (fontMono (fontTone Warning defaultLayout) == fontTone Warning (fontMono defaultLayout))
  let na = ctxNodeArena ctx
  n <- arenaCount na
  styles <- forM [0 .. n - 1] $ \i -> (,) <$> getNodeType na i <*> getStyleIdx na i
  let faces = [(textNodeFontVariant si, textNodeFontTone si) | (NodeText, si) <- styles]
  assertEq failed [(FontMono, Just Warning), (FontMono, Just Warning), (FontRegular, Just Muted), (FontRegular, Just Danger), (FontRegular, Just Danger)] faces

-- | A theme is light or dark by its window colour; 'lightDark' picks dark when the system appearance is unknown.
runThemeAppearanceTest :: Context -> IORef Int -> IO ()
runThemeAppearanceTest _ failed = do
  assertEq failed [AppearanceDark, AppearanceLight, AppearanceDark, AppearanceLight, AppearanceDark, AppearanceDark, AppearanceLight] (map (themeAppearance . snd) builtInThemes)
  assertEq failed [defaultLightTheme, defaultTheme, defaultTheme] (map (lightDark defaultLightTheme defaultTheme) [Just AppearanceLight, Just AppearanceDark, Nothing])
  assert failed (all (\a -> defaultThemeFor a == lightDark defaultLightTheme defaultTheme a) [Nothing, Just AppearanceLight, Just AppearanceDark])

-- | Following the system switches theme on a change, waking the loop and repainting; a repeated report does nothing.
runFollowSystemThemeTest :: Context -> IORef Int -> IO ()
runFollowSystemThemeTest ctx failed = do
  let ui = column (button' "Go")
      buttonIn theme (resp, quads, _) = assert failed (styleBg (themeButton theme) `elem` fillsIn (respRect resp) quads)
  setSystemAppearance ctx (Just AppearanceLight)
  followSystemTheme ctx (lightDark defaultLightTheme defaultTheme)
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
  -- Back to light, then to an unknown appearance, for which 'lightDark' picks dark.
  forM_ [([Just AppearanceLight], defaultLightTheme), ([Just AppearanceLight, Nothing], defaultTheme)] $ \(reports, want) ->
    mapM_ (setSystemAppearance ctx) reports >> getTheme ctx >>= assertEq failed want

-- | After 'setTheme', a system switch keeps the theme but still repaints.
runSetThemeStopsFollowingTest :: Context -> IORef Int -> IO ()
runSetThemeStopsFollowingTest ctx failed = do
  let ui = column (label "x")
  setSystemAppearance ctx (Just AppearanceDark)
  followSystemTheme ctx (lightDark defaultLightTheme defaultTheme)
  getTheme ctx >>= assertEq failed defaultTheme
  setTheme ctx tomorrowMidnightMinDarkTheme
  warmup ctx inp ui
  setSystemAppearance ctx (Just AppearanceLight)
  getTheme ctx >>= assertEq failed tomorrowMidnightMinDarkTheme
  checkIdleFullDamage failed ctx inp inp ui

-- | A view reads the appearance and sees a change on the next frame.
runSystemAppearanceViewTest :: Context -> IORef Int -> IO ()
runSystemAppearanceViewTest ctx failed = do
  let ui = column (systemAppearance >>= label . maybe "unknown" (\a -> if a == AppearanceDark then "dark" else "light"))
  warmup ctx inp ui
  collectTextSpans ctx >>= assertSpansHas failed "unknown"
  setSystemAppearance ctx (Just AppearanceDark)
  needsRedraw ctx inp inp >>= assert failed
  warmup ctx inp ui
  collectTextSpans ctx >>= assertSpansHas failed "dark"

-- | A view that sets its theme from the system appearance every frame
-- settles, switching one frame after the system does. So does a view that
-- sets two themes per frame (the last wins), and one that sets a theme over
-- a context that follows the system.
runViewPicksThemeTest :: Context -> IORef Int -> IO ()
runViewPicksThemeTest ctx failed = do
  let picking extra = column ((setUiTheme . lightDark defaultLightTheme defaultTheme =<< systemAppearance) >> extra >> button "Go")
      settles ui = do
        warmup ctx inp ui
        isDirty ctx >>= assert failed . not
        takeDamage ctx >>= assert failed . damageIsEmpty
  setSystemAppearance ctx (Just AppearanceLight)
  -- The frame that switches theme requests a full repaint after it.
  _ <- warmup2 ctx inp (picking (pure ()))
  getTheme ctx >>= assertEq failed defaultLightTheme
  settles (picking (pure ()))
  setSystemAppearance ctx (Just AppearanceDark)
  _ <- warmup2 ctx inp (picking (pure ()))
  getTheme ctx >>= assertEq failed defaultTheme
  settles (picking (pure ()))
  -- Two themes per frame: the last wins, with one repaint.
  _ <- warmup2 ctx inp (picking (setUiTheme tomorrowMinLightTheme))
  getTheme ctx >>= assertEq failed tomorrowMinLightTheme
  settles (picking (setUiTheme tomorrowMinLightTheme))
  -- A view's theme overrides following the system.
  followSystemTheme ctx (lightDark defaultLightTheme defaultTheme)
  _ <- warmup2 ctx inp (setUiTheme tomorrowMinLightTheme >> button "Go")
  setSystemAppearance ctx (Just AppearanceLight)
  getTheme ctx >>= assertEq failed tomorrowMinLightTheme
  readIORef (ctxThemeFor ctx) >>= assert failed . isNothing

-- | A system switch restyles warning labels, rich text and buttons on the next frame.
runFollowSystemRestylesWarningTest :: Context -> IORef Int -> IO ()
runFollowSystemRestylesWarningTest ctx failed = do
  let ui = column (labelWith (fontTone Warning) "Careful" >> richText [inlineWith (fontTone Warning) "Rich"] >> styled warning (button' "Go"))
      amberIn theme (resp, quads, spans) = do
        assertEq failed (Just (themeWarning theme)) (spanFg "Careful" spans)
        assert failed (themeWarning theme `elem` fillsIn (respRect resp) quads)
        pure quads
  setSystemAppearance ctx (Just AppearanceLight)
  followSystemTheme ctx (lightDark defaultLightTheme defaultTheme)
  _ <- frames 2 ctx ui >>= amberIn defaultLightTheme
  setSystemAppearance ctx (Just AppearanceDark)
  quads <- frames 1 ctx ui >>= amberIn defaultTheme
  assert failed (themeWarning defaultLightTheme `notElem` map snd quads)
