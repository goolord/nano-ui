module Cases.Tabs
  ( runTabsClosableTest
  , runTabsContentDamageTest
  , runTabsDamageTest
  , runTabsEmitTest
  , runTabsInteractionTest
  , runTabsLazinessTest
  , runTabsStatePersistenceTest
  ) where

import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (bump, withInput)
import NanoUI.Testing.Harness (clickPair, runClick, runClickPair, withInputOff)

data DummyTab = TabA | TabB | TabC
  deriving (Eq, Show)

runTabsLazinessTest :: Context -> IORef Int -> IO ()
runTabsLazinessTest ctx failed = do
  evalCountA <- newIORef (0 :: Int)
  evalCountB <- newIORef (0 :: Int)
  evalCountC <- newIORef (0 :: Int)
  let
    inp = withInput 200 100
    ui = do
      _ <-
        tabs
          TabB
          [ tab TabA "A" (uiIO (modifyIORef' evalCountA (+ 1)) >> label_ "Body A")
          , tab TabB "B" (uiIO (modifyIORef' evalCountB (+ 1)) >> label_ "Body B")
          , tab TabC "C" (uiIO (modifyIORef' evalCountC (+ 1)) >> label_ "Body C")
          ]
      pure ()
  _ <- runFrame ctx inp ui
  cntA <- readIORef evalCountA
  cntB <- readIORef evalCountB
  cntC <- readIORef evalCountC
  when (cntA /= 0) $ bump failed
  when (cntB /= 1) $ bump failed
  when (cntC /= 0) $ bump failed

runTabsInteractionTest :: Context -> IORef Int -> IO ()
runTabsInteractionTest ctx failed = do
  let
    inp0 = withInput 300 100
    ui curTab =
      tabs
        curTab
        [ tab TabA "Alpha" (label_ "Body A")
        , tab TabB "Beta" (label_ "Body B")
        ]
  ((_, active0), _, _, _) <- runFrame ctx inp0 (ui TabA)
  when (active0 /= TabA) $ bump failed
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (Rect bx by bw bh : _) -> do
      let
        clickPos = V2 (bx + bw / 2) (by + bh / 2)
      (resp1, active1) <- runClickPair ctx inp0 (ui TabA) clickPos
      when (not (respChanged resp1) || active1 /= TabB) $ bump failed
    [] -> bump failed

data TabMsg = MsgSelect DummyTab | MsgClose DummyTab
  deriving (Eq, Show)

runTabsEmitTest :: Context -> IORef Int -> IO ()
runTabsEmitTest ctx failed = do
  let
    inp0 = withInput 300 100
    ui curTab =
      tabsEmit
        MsgSelect
        curTab
        [ tab TabA "Alpha" (label_ "Body A")
        , tab TabB "Beta" (label_ "Body B")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (Rect bx by bw bh : _) -> do
      let
        clickPos = V2 (bx + bw / 2) (by + bh / 2)
        press =
          inp0 {inputMousePos = clickPos, inputMouseDown = True, inputMousePressed = True}
        release =
          press
            { inputMouseDown = False
            , inputMousePressed = False
            , inputMouseReleased = True
            }
      _ <- runFrame ctx press (ui TabA)
      (_, msgs, _, _) <- runFrame ctx release (ui TabA)
      let
        decoded = decodeMessages msgs :: [TabMsg]
      when (decoded /= [MsgSelect TabB]) $ bump failed
    [] -> bump failed

runTabsClosableTest :: Context -> IORef Int -> IO ()
runTabsClosableTest ctx failed = do
  let
    inp0 = withInput 300 100
    ui curTab =
      tabsEx
        TabUnderline
        TabTop
        curTab
        [ closableTab TabA "Alpha" (label_ "Body A")
        , closableTab TabB "Beta" (label_ "Body B")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Alpha" `T.isInfixOf` txt] of
    (Rect ax ay aw ah : _) -> do
      let
        clickPos = V2 (ax + aw + 16) (ay + ah / 2)
      (tResp, activeTab) <- runClickPair ctx inp0 (ui TabA) clickPos
      when (tabClosed tResp /= Just TabA) $ bump failed
      when (activeTab /= TabA) $ bump failed
    [] -> putStrLn "No Alpha span" >> bump failed

runTabsStatePersistenceTest :: Context -> IORef Int -> IO ()
runTabsStatePersistenceTest ctx failed = do
  let
    inp0 = withInput 300 100
    ui curTab =
      tabs
        curTab
        [ tab TabA "A" $ do
            (readFlag, setFlag) <- useFlag False
            clickButton "ToggleA" (readFlag >>= \f -> setFlag (not f))
            flag <- readFlag
            label_ (if flag then "FlagIsOn" else "FlagIsOff")
        , tab TabB "B" $ do
            label_ "OtherTab"
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans0 <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans0, "ToggleA" `T.isInfixOf` txt] of
    (Rect cx cy cw ch : _) -> do
      let
        clickPos = V2 (cx + cw / 2) (cy + ch / 2)
      runClick ctx inp0 (ui TabA) clickPos

      -- Frame after click on TabA: Verify FlagIsOn
      _ <- runFrame ctx inp0 (ui TabA)
      spans1 <- collectTextSpans ctx
      when (not (any (\(_, t, _, _, _) -> "FlagIsOn" `T.isInfixOf` t) spans1)) $
        bump failed

      -- Switch to TabB for a frame:
      _ <- runFrame ctx inp0 (ui TabB)
      spans2 <- collectTextSpans ctx
      when (not (any (\(_, t, _, _, _) -> "OtherTab" `T.isInfixOf` t) spans2)) $
        bump failed
      when (any (\(_, t, _, _, _) -> "FlagIsOn" `T.isInfixOf` t) spans2) $ bump failed

      -- Switch back to TabA: Verify FlagIsOn is STILL preserved!
      _ <- runFrame ctx inp0 (ui TabA)
      spans3 <- collectTextSpans ctx
      when (not (any (\(_, t, _, _, _) -> "FlagIsOn" `T.isInfixOf` t) spans3)) $
        bump failed
    [] -> bump failed

runTabsDamageTest :: Context -> IORef Int -> IO ()
runTabsDamageTest _ failed = do
  ctx <- newContext
  let
    inp0 = withInputOff 300 100
    ui curTab =
      tabs
        curTab
        [ tab TabA "Alpha" (label_ "Body A with some text")
        , tab TabB "Beta" (label_ "Body B different widgets")
        ]
  -- Frame 1: initial layout on TabA
  _ <- runFrame ctx inp0 (ui TabA)
  _ <- takeDamage ctx

  -- Frame 2: settle on TabA, mouse off-screen -> should not be full damage
  _ <- runFrame ctx inp0 (ui TabA)
  dIdle <- takeDamage ctx
  when (dIdle == DamageFull) $ bump failed
  spansIdle <- collectTextSpans ctx
  when (not (any (\(_, t, _, _, _) -> "Body A" `T.isInfixOf` t) spansIdle)) $
    bump failed

  -- Frame 3: click on Tab Beta
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (Rect bx by bw bh : _) -> do
      let
        clickPos = V2 (bx + bw / 2) (by + bh / 2)
        press =
          inp0 {inputMousePos = clickPos, inputMouseDown = True, inputMousePressed = True}
        release =
          press
            { inputMouseDown = False
            , inputMousePressed = False
            , inputMouseReleased = True
            }
      _ <- runFrame ctx press (ui TabA)
      -- Frame 4: tab release switches body same frame (no one-frame lag).
      ((resp, newTab), _, _, _) <- runFrame ctx release (ui TabA)
      when (not (respChanged resp) || newTab /= TabB) $ bump failed
      spansSwitch <- collectTextSpans ctx
      when (not (any (\(_, t, _, _, _) -> "Body B" `T.isInfixOf` t) spansSwitch)) $
        bump failed
      when (any (\(_, t, _, _, _) -> "Body A" `T.isInfixOf` t) spansSwitch) $
        bump failed
      dSwitch <- takeDamage ctx
      when (dSwitch /= DamageFull) $ bump failed

      -- Frame 5: parent curTab caught up; prior dirty forces Full.
      _ <- runFrame ctx inp0 (ui TabB)
      dTabB <- takeDamage ctx
      when (dTabB /= DamageFull) $ bump failed

      -- Frame 6: Settle on TabB (idle) -> no longer DamageFull
      _ <- runFrame ctx inp0 (ui TabB)
      dSettled <- takeDamage ctx
      when (dSettled == DamageFull) $ bump failed
    [] -> bump failed

runTabsContentDamageTest :: Context -> IORef Int -> IO ()
runTabsContentDamageTest _ failed = do
  ctx <- newContext
  let
    inp0 = withInputOff 320 200
    ui = do
      (readClick, setClick) <- useText ""
      row defaultLayout $ do
        btn <- button "OK"
        onClick btn (setClick "OK")
        _ <-
          tabs
            ("Controls" :: T.Text)
            [ tab "Controls" "Controls" $ do
                click <- readClick
                kv "Clicked" (if T.null click then "-" else click)
            ]
        pure btn
  (_, _, _, _) <- runFrame ctx inp0 ui
  (btn, _, _, _) <- runFrame ctx inp0 ui
  let
    Rect bx by bw bh = respRect btn
    clickPos = V2 (bx + bw / 2) (by + bh / 2)
    (press, release) = clickPair inp0 clickPos
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  spans1 <- collectTextSpans ctx
  when (not (any (\(_, t, _, _, _) -> "OK" `T.isInfixOf` t) spans1)) $ bump failed
  _ <- runFrame ctx inp0 ui
  spans2 <- collectTextSpans ctx
  when (not (any (\(_, t, _, _, _) -> "OK" `T.isInfixOf` t) spans2)) $ bump failed
