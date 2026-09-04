module Cases.Demo
  ( runControlsTabHeightTest
  , runColorPickerPreviewTest
  , runColorPickerCommitTest
  , runColorPickerKeyCommitTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness (pressAt, releaseAt, warmup2, withInputOff)

data DemoTab
  = Controls
  | List
  | Diagnostics
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data DemoTheme
  = Light
  | Dark
  | System
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

runControlsTabHeightTest :: Context -> IORef Int -> IO ()
runControlsTabHeightTest _ failed = do
  let
    inp0 =
      withInputOff 1280 800
    controlsBody dumpRef = do
      heading "Controls"
      (cb, checked) <- checkbox "Feature" False
      (_, vol) <- slider "Volume" 0 100 50
      (_, qualityIdx) <- select "Quality" ["Low", "Medium", "High"] 1
      (cp, _) <- colorPicker "Accent" (colorRGBA 204 102 102 255)
      (_, theme) <- boundedRadioFieldset "Theme" Dark (T.pack . show)
      (ti, name) <- textInput "Name" ""
      sep
      uiIO $ writeIORef dumpRef (Just (cb, cp, ti, checked, vol, qualityIdx, theme, name))
      pure cb
    demoPage dumpRef = do
      (checked, setChecked) <- useFlag False
      (vol, setVol) <- useText "50"
      (quality, setQuality) <- useText "Medium"
      (theme, setTheme) <- useText (T.pack (show Dark))
      (name, setName) <- useText ""
      scrollWith (tight . grow) $
        columnWith (padAll 8 . gap 8 . fillW) $ do
          rowWith (tight . gap 8 . fillW) $ do
            columnWith (tight . gap 8 . fillW) $ do
              card $ do
                heading "State"
                kv "Feature" (if checked then "on" else "off")
                kv "Volume" vol
                kv "Quality" quality
                kv "Theme" theme
                kv "Name" (if T.null name then "-" else name)
                kv "Clicked" "-"
              card $ do
                heading "Gallery"
                mapM_ (\i -> void (label (T.pack ("thumb line " <> show (i :: Int))))) [1 .. 8]
            card $
              boundedTabs Controls (T.pack . show) $ \demoTab ->
                case demoTab of
                  Controls -> do
                    cb <- controlsBody dumpRef
                    cVal <- uiIO $ do
                      m <- readIORef dumpRef
                      pure $ maybe False (\(_, _, _, c, _, _, _, _) -> c) m
                    setChecked cVal
                    (_, _, _, _, vVal, qualityIdx, tVal, nVal) <-
                      uiIO $ do
                        m <- readIORef dumpRef
                        case m of
                          Just dumped -> pure dumped
                          Nothing ->
                            pure (cb, cb, cb, False, 50, 1, Dark, T.empty)
                    setVol (T.pack (show (round vVal :: Int)))
                    setQuality (["Low", "Medium", "High"] !! qualityIdx)
                    setTheme (T.pack (show tVal))
                    setName nVal
                  List -> heading "Tree"
                  Diagnostics -> heading "Diagnostics"
    rectHOf ctx wid = do
      m <- getPrevRect ctx wid
      pure $ fmap (\(Rect _ _ _ h) -> h) m
    spanOf lbls spans =
      let
        match txt =
          any
            (\lbl -> txt == lbl || T.drop 1 txt == lbl || T.isSuffixOf lbl txt)
            lbls
        ys =
          [ (y, y + h)
          | (Rect _ y _ h, txt, _, _, _) <- spans
          , match txt
          ]
       in
        case ys of
          [] -> 0
          _ -> maximum (map snd ys) - minimum (map fst ys)
  dumpLone <- newIORef Nothing
  ctxLone <- newPixelContext
  _ <- runFrame ctxLone inp0 (columnWith (tight . fillW) (controlsBody dumpLone))
  mLone <- readIORef dumpLone
  (loneCbH, loneCpH, loneTiH) <-
    case mLone of
      Just (cb, cp, ti, _, _, _, _, _) -> do
        h1 <- rectHOf ctxLone (respId cb)
        h2 <- rectHOf ctxLone (respId cp)
        h3 <- rectHOf ctxLone (respId ti)
        pure (h1, h2, h3)
      Nothing -> pure (Nothing, Nothing, Nothing)
  dumpPage <- newIORef Nothing
  ctxPage <- newPixelContext
  let page = demoPage dumpPage
  _ <- runFrame ctxPage inp0 page
  _ <- runFrame ctxPage inp0 page
  mPage0 <- readIORef dumpPage
  spans0 <- collectTextSpans ctxPage
  (hCb0, hCp0, hTi0, cbRect0) <-
    case mPage0 of
      Just (cb, cp, ti, _, _, _, _, _) -> do
        a <- rectHOf ctxPage (respId cb)
        b <- rectHOf ctxPage (respId cp)
        c <- rectHOf ctxPage (respId ti)
        r <- getPrevRect ctxPage (respId cb)
        pure (a, b, c, r)
      Nothing -> pure (Nothing, Nothing, Nothing, Nothing)
  hover <-
    case cbRect0 of
      Just (Rect rx ry rw rh) ->
        pure (inp0 {inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)})
      Nothing -> pure inp0
  _ <- runFrame ctxPage hover page
  mPageH <- readIORef dumpPage
  spansH <- collectTextSpans ctxPage
  (hCbH, hCpH, hTiH) <-
    case mPageH of
      Just (cb, cp, ti, _, _, _, _, _) -> do
        a <- rectHOf ctxPage (respId cb)
        b <- rectHOf ctxPage (respId cp)
        c <- rectHOf ctxPage (respId ti)
        pure (a, b, c)
      Nothing -> pure (Nothing, Nothing, Nothing)
  let
    left0 = spanOf ["State", "Clicked", "Gallery"] spans0
    body0 = spanOf ["Controls", "Accent"] spans0
    leftH = spanOf ["State", "Clicked", "Gallery"] spansH
    bodyH = spanOf ["Controls", "Accent"] spansH
  let
    tooTall pageH loneH = case (pageH, loneH) of
      (Just p, Just l) -> l >= 8 && p > l * 1.35
      _ -> True
    jumped a b = case (a, b) of
      (Just x, Just y) -> abs (x - y) > 1
      _ -> True
  assert failed (not (tooTall hCb0 loneCbH || tooTall hCp0 loneCpH || tooTall hTi0 loneTiH))
  assert failed (not (tooTall hCbH loneCbH || tooTall hCpH loneCpH || tooTall hTiH loneTiH))
  assert failed (not (jumped hCb0 hCbH || jumped hCp0 hCpH || jumped hTi0 hTiH))
  -- Body must not fill the wrap-line height of the left column.
  assert failed (not (left0 > 80 && body0 > left0 * 0.92))
  assert failed (not (leftH > 80 && bodyH > leftH * 0.92))

runColorPickerPreviewTest :: Context -> IORef Int -> IO ()
runColorPickerPreviewTest _ failed = do
  ctx <- newPixelContext
  let inp0 = withInputOff 400 420
      ui = void (colorPicker "Accent" (colorRGBA 204 102 102 255))
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  let findLabel needle =
        listToMaybe [(r, t) | (r, t, _, _, _) <- spans, needle `T.isInfixOf` t]
  case (findLabel "Current Color", findLabel "New Color") of
    (Just (Rect cx cy _ _, _), Just (Rect nx ny _ _, _)) -> do
      assert failed (nx > cx + 20)
      assert failed (abs (cy - ny) < 4)
    _ -> assert failed False

runColorPickerCommitTest :: Context -> IORef Int -> IO ()
runColorPickerCommitTest ctx failed = do
  let inp0 = withInput 400 420
      initial = colorRGBA 204 102 102 255
      packed c = colorToWord32 c
      ui = colorPicker "Accent" initial
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect x y w h = respRect resp
      wid = respId resp
      geom = colorPickerGeom (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h
      sv = cpgSv geom
      pt = V2 (rectX sv + rectW sv * 0.9) (rectY sv + 2)
      press = pressAt inp0 pt
      release = releaseAt press
  _ <- runFrame ctx press ui
  storeDrag <- getStore ctx
  assertEq failed (packed (widgetStoreBaseColor storeDrag wid initial)) (packed initial)
  assert failed (packed (widgetStoreColor storeDrag wid initial) /= packed initial)
  _ <- runFrame ctx release ui
  storeDone <- getStore ctx
  assertEq
    failed
    (packed (widgetStoreBaseColor storeDone wid initial))
    (packed (widgetStoreColor storeDone wid initial))

runColorPickerKeyCommitTest :: Context -> IORef Int -> IO ()
runColorPickerKeyCommitTest ctx failed = do
  let inp0 = withInput 400 420
      initial = colorRGBA 204 102 102 255
      packed c = colorToWord32 c
      ui = colorPicker "Accent" initial
  (resp, _) <- warmup2 ctx inp0 ui
  let wid = respId resp
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyRight]}) ui
  store <- getStore ctx
  let base = packed (widgetStoreBaseColor store wid initial)
      neu = packed (widgetStoreColor store wid initial)
  assert failed (neu /= packed initial)
  assertEq failed base neu
