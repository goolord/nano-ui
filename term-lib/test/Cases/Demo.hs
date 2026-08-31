module Cases.Demo
  ( runControlsTabHeightTest
  ) where

import Control.Monad (void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (bump)
import NanoUI.Testing.Harness (withInputOff)

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
      (readChecked, setChecked) <- useFlag False
      (readVol, setVol) <- useText "50"
      (readQuality, setQuality) <- useText "Medium"
      (readTheme, setTheme) <- useText (T.pack (show Dark))
      (readName, setName) <- useText ""
      scroll (tight (grow defaultLayout)) $
        column (padAll 8 . gap 8 . fillW $ defaultLayout) $ do
          row (tight . gap 8 . wrap . fillW $ defaultLayout) $ do
            column (tight . gap 8 . fillW $ defaultLayout) $ do
              card $ do
                heading "State"
                checked <- readChecked
                vol <- readVol
                quality <- readQuality
                theme <- readTheme
                name <- readName
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
                    checked <- uiIO $ do
                      m <- readIORef dumpRef
                      pure $ maybe False (\(_, _, _, c, _, _, _, _) -> c) m
                    setChecked checked
                    (_, _, _, _, vol, qualityIdx, theme, name) <-
                      uiIO $ do
                        m <- readIORef dumpRef
                        case m of
                          Just dumped -> pure dumped
                          Nothing ->
                            pure (cb, cb, cb, False, 50, 1, Dark, T.empty)
                    setVol (T.pack (show (round vol :: Int)))
                    setQuality (["Low", "Medium", "High"] !! qualityIdx)
                    setTheme (T.pack (show theme))
                    setName name
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
  _ <- runFrame ctxLone inp0 (column (tight . fillW $ defaultLayout) (controlsBody dumpLone))
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
  when (tooTall hCb0 loneCbH || tooTall hCp0 loneCpH || tooTall hTi0 loneTiH) $ bump failed
  when (tooTall hCbH loneCbH || tooTall hCpH loneCpH || tooTall hTiH loneTiH) $ bump failed
  when (jumped hCb0 hCbH || jumped hCp0 hCpH || jumped hTi0 hTiH) $ bump failed
  -- Body must not fill the wrap-line height of the left column.
  when (left0 > 80 && body0 > left0 * 0.92) $ bump failed
  when (leftH > 80 && bodyH > leftH * 0.92) $ bump failed
