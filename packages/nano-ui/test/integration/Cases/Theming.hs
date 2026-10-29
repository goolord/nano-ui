{-# OPTIONS_GHC -O0 #-}
module Cases.Theming (tests) where

import Spec
import Data.Text (Text)
import Data.Maybe (isNothing)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (arenaCount, getNodeType, getStyleIdx)

tests :: [Spec]
tests =
  [ spec "popup-style" runPopupStyleTest
  , spec "warning-tone" runWarningToneTest
  , spec "theme-values" runThemeValuesTest
  , spec "follow-system-theme" runFollowSystemThemeTest
  , spec "view-picks-theme" runViewPicksThemeTest
  ]

inp :: Input
inp = withInput 400 300

-- | The foreground of the span showing @txt@.
spanFg :: Text -> [(Rect, Text, Color, Color, Rect)] -> Maybe Color
spanFg txt spans = lookup txt [(t, fg) | (_, t, fg, _, _) <- spans]

-- | The last of @n@ frames of @ui@: its result, the quads drawn, and the text spans.
frames :: Int -> Context -> NanoUI a -> IO (a, [(Rect, Color)], [DemoSpan])
frames n ctx ui = do
  replicateM_ (n - 1) (warmup ctx inp ui)
  (a, draw, _) <- runFrame ctx inp ui
  (,,) a <$> drawQuads draw <*> collectTextSpans ctx

-- | Tones colour text, rich text pieces and buttons from the theme, fading when
-- disabled; a tone combines with a face in either order, stored apart in the node.
runWarningToneTest :: Context -> IORef Int -> IO ()
runWarningToneTest ctx failed = do
  theme <- getTheme ctx
  let amber = themeWarning theme
      warned = labelWith (fontTone Warning)
      own = colorRGBA 1 2 3 255
  ((w, d, p), quads, spans) <- frames 2 ctx . column $ do
    warned "Careful" >> danger "Broken" >> disabledWhen True (warned "Faded")
    labelWith (fontMono . fontTone Warning) "mono warning"
    labelWith (fontTone Warning . fontMono) "warning mono"
    labelWith fontMuted "muted"
    labelWith (\l -> l {layoutFontVariant = FontDanger}) "legacy danger"
    labelWith (fontTone Danger . fontColor own) "own colour"
    (,,) <$> styled warning (button' "Warn") <*> styled destructive (button' "Delete") <*> button' "Plain"
  let fills r = fillsIn (respRect r) quads
      faded = themeWarning (disabledTheme theme)
      fgs =
        [ ("Careful", amber), ("Broken", themeDanger theme), ("Faded", faded), ("mono warning", amber)
        , ("warning mono", amber), ("muted", themeMuted theme), ("legacy danger", themeDanger theme)
        , ("own colour", own)
        ]
      regular = (,) FontRegular . Just
      faces = map regular [Warning, Danger, Warning] ++ replicate 2 (FontMono, Just Warning)
        ++ map regular [Muted, Danger, Danger]
  assertJust failed (spanRectOf "Careful" spans) $ \r -> assert failed (amber `elem` fillsIn r quads)
  assert failed (amber `notElem` [themeDanger theme, faded])
  assertEq failed (map (Just . snd) fgs) (map ((`spanFg` spans) . fst) fgs)
  let na = ctxNodeArena ctx
  n <- arenaCount na
  styles <- forM [0 .. n - 1] $ \i -> (,) <$> getNodeType na i <*> getStyleIdx na i
  assertEq failed faces [(textNodeFontVariant si, textNodeFontTone si) | (NodeText, si) <- styles]
  assertEq failed [True, True, False] [elem amber (fills w), elem (themeDanger theme) (fills d), elem amber (fills p)]
  assertEq failed (Just (readableOn theme amber), amber) $
    (spanFg "Warn" spans, styleBg (themeButton (warning theme)))
  offTheme <- warmup2 ctx inp (column (disabledWhen True (styled warning uiTheme)))
  assertEq failed (styleBg (themeButton offTheme)) (styleBg (themeButton (disabledTheme (warning theme))))
  let richCases = [(["Plain text"], False), (["Plain ", inlineWith (fontTone Warning) "text"], True)]
  forM_ richCases $ \(pieces, warns) -> do
    (_, rich, _) <- frames 2 ctx (column (richText pieces))
    assertEq failed warns (amber `elem` map snd rich)

-- | The built-in themes, by name.
builtInThemes :: [(String, Theme)]
builtInThemes =
  [ ("default", defaultTheme)
  , ("default light", defaultLightTheme)
  , ("tomorrow night", tomorrowNightMinDarkTheme)
  , ("tomorrow light", tomorrowMinLightTheme)
  , ("tomorrow midnight", tomorrowMidnightMinDarkTheme)
  , ("base16 night", themeFromBase16 base16TomorrowNight)
  , ("base16 light", themeFromBase16 base16TomorrowLight)
  ]

-- | Built-in themes' tones reach 4.5:1 contrast, and their page sets light or dark.
-- The page is the window unless set; 'lightDark' picks dark for an unknown system.
runThemeValuesTest :: Context -> IORef Int -> IO ()
runThemeValuesTest _ failed = do
  forM_ builtInThemes $ \(name, t) -> do
    let amber = themeWarning t
        readable c = contrastRatio c (pageBackground t) >= 4.5
        readable' c = contrastRatio (readableOn t c) c >= 4.5
    assertEq failed (name, True, True) (name, all readable [themeSuccess t, amber, themeDanger t], readable' amber)
    assertEq failed (name, True, True) (name, amber /= themeDanger t, themeSuccess t /= themeDanger t)
  assertEq failed [dark, light, dark, light, dark, dark, light] (map (themeAppearance . snd) builtInThemes)
  let t = tomorrowMinLightTheme
  assert failed (styleBg (themeButton (tone Danger t)) == themeDanger t)
  assert failed (and [toneColor (disabledTheme t) c /= toneColor t c | c <- [Success, Warning, Danger]])
  let panelBg = styleBg (themePanel defaultTheme)
      red = colorRGBA 1 2 3 255
  forM_
    [ (themeWindow defaultTheme, id)
    , (panelBg, panelPage)
    , (red, pageColor red)
    , (panelBg, panelPage . pageColor red)
    , -- A translucent panel surface leaves an unset page alone.
      (themeWindow defaultTheme, panelStyle (background colorTransparent))
    ]
    $ \(want, f) -> assertEq failed want (pageBackground (f defaultTheme))
  let picks = [Just light, Just dark, Nothing]
      picked = map (lightDark defaultLightTheme defaultTheme) picks
  assertEq failed [defaultLightTheme, defaultTheme, defaultTheme] picked
  assertEq failed picked (map defaultThemeFor picks)
  where
    (dark, light) = (AppearanceDark, AppearanceLight)

-- | Following the system switches and restyles on a change, waking the loop; a repeat
-- does nothing. After 'setTheme', a switch keeps the theme but still repaints.
runFollowSystemThemeTest :: Context -> IORef Int -> IO ()
runFollowSystemThemeTest ctx failed = do
  let ui = column $ do
        labelWith (fontTone Warning) "Careful"
        (,) <$> button' "Go" <*> styled warning (button' "Warn")
      painted theme ((go, warn), quads, spans) = do
        assertEq failed (Just (themeWarning theme)) (spanFg "Careful" spans)
        assert failed (styleBg (themeButton theme) `elem` fillsIn (respRect go) quads)
        assert failed (themeWarning theme `elem` fillsIn (respRect warn) quads)
        pure quads
  setSystemAppearance ctx (Just AppearanceLight)
  followSystemTheme ctx (lightDark defaultLightTheme defaultTheme)
  getTheme ctx >>= assertEq failed defaultLightTheme
  _ <- frames 2 ctx ui >>= painted defaultLightTheme
  wakes <- newIORef (0 :: Int)
  setWakeLoop ctx (modifyIORef' wakes (+ 1))
  setSystemAppearance ctx (Just AppearanceDark)
  getTheme ctx >>= assertEq failed defaultTheme
  getSystemAppearance ctx >>= assertEq failed (Just AppearanceDark)
  readIORef wakes >>= assert failed . (> 0)
  checkIdleFullDamage failed ctx inp inp ui
  quads <- frames 1 ctx ui >>= painted defaultTheme
  assert failed (themeWarning defaultLightTheme `notElem` map snd quads)
  writeIORef wakes 0
  setSystemAppearance ctx (Just AppearanceDark)
  isDirty ctx >>= assert failed . not
  readIORef wakes >>= assertEq failed 0
  -- Back to light, then to an unknown appearance, for which 'lightDark' picks dark.
  setSystemAppearance ctx (Just AppearanceLight)
  getTheme ctx >>= assertEq failed defaultLightTheme
  setSystemAppearance ctx (Just AppearanceLight) >> setSystemAppearance ctx Nothing
  getTheme ctx >>= assertEq failed defaultTheme
  setTheme ctx tomorrowMidnightMinDarkTheme
  warmup ctx inp ui
  setSystemAppearance ctx (Just AppearanceLight)
  getTheme ctx >>= assertEq failed tomorrowMidnightMinDarkTheme
  checkIdleFullDamage failed ctx inp inp ui

-- | A view sees an appearance change next frame; setting its theme from it every frame
-- (twice, or over a context following the system) settles a frame after a switch.
runViewPicksThemeTest :: Context -> IORef Int -> IO ()
runViewPicksThemeTest ctx failed = do
  let shown = maybe "unknown" (\a -> if a == AppearanceDark then "dark" else "light")
      picking extra = column $ do
        appearance <- systemAppearance
        label (shown appearance)
        setUiTheme (lightDark defaultLightTheme defaultTheme appearance)
        extra >> button "Go"
      settles ui = do
        warmup ctx inp ui
        isDirty ctx >>= assert failed . not
        takeDamage ctx >>= assert failed . damageIsEmpty
      switchTo appearance theme extra = do
        setSystemAppearance ctx (Just appearance)
        needsRedraw ctx inp inp >>= assert failed
        _ <- warmup2 ctx inp (picking extra)
        collectTextSpans ctx >>= assertSpansHas failed (shown (Just appearance))
        getTheme ctx >>= assertEq failed theme
        settles (picking extra)
  warmup ctx inp (picking (pure ()))
  collectTextSpans ctx >>= assertSpansHas failed "unknown"
  switchTo AppearanceLight defaultLightTheme (pure ())
  switchTo AppearanceDark defaultTheme (pure ())
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

-- | 'popupStyle' colours a modal's surface and not the panels inside it.
runPopupStyleTest :: Context -> IORef Int -> IO ()
runPopupStyleTest ctx failed = do
  let sea = colorRGBA 12 60 90 255
      ui = styled (popupStyle (background sea)) $ do
        let body = panelWith (fixedWH 120 40 . tight) (snd <$> mouseArea (fillW . fillH) (pure ()))
        fmap respRect . snd <$> modal' (constVar True) "Dialog" body
  (inner, draw) <- warmupDraw ctx inp ui
  quads <- drawQuads draw
  panelBg <- styleBg . themePanel <$> getTheme ctx
  assert failed (sea `elem` map snd quads)
  assertJust failed inner $ \r -> assert failed (panelBg `elem` fillsIn r quads)
