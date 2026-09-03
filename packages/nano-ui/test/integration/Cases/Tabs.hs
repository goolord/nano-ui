module Cases.Tabs
  ( runTabsClosableTest
  , runTabsContentDamageTest
  , runTabsDamageTest
  , runTabsEmitTest
  , runTabsInteractionTest
  , runTabsLazinessTest
  , runTabsStatePersistenceTest
  ) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness
  ( assertSpansHas
  , centerOf
  , clickPair
  , runClick
  , runClickPair
  , spansHas
  , withInputOff
  )

data DummyTab = TabA | TabB | TabC
  deriving (Eq, Show)

runTabsLazinessTest :: Context -> IORef Int -> IO ()
runTabsLazinessTest ctx failed = do
  evalCountA <- newIORef (0 :: Int)
  evalCountB <- newIORef (0 :: Int)
  evalCountC <- newIORef (0 :: Int)
  let inp = withInput 200 100
      ui = tabs TabB
        [ tab TabA "A" (uiIO (modifyIORef' evalCountA (+ 1)) >> label_ "Body A")
        , tab TabB "B" (uiIO (modifyIORef' evalCountB (+ 1)) >> label_ "Body B")
        , tab TabC "C" (uiIO (modifyIORef' evalCountC (+ 1)) >> label_ "Body C")
        ]
  _ <- runFrame ctx inp ui
  cntA <- readIORef evalCountA
  cntB <- readIORef evalCountB
  cntC <- readIORef evalCountC
  assertEq failed cntA 0
  assertEq failed cntB 1
  assertEq failed cntC 0

runTabsInteractionTest :: Context -> IORef Int -> IO ()
runTabsInteractionTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabs curTab
        [ tab TabA "Alpha" (label_ "Body A")
        , tab TabB "Beta" (label_ "Body B")
        ]
  ((_, active0), _, _, _) <- runFrame ctx inp0 (ui TabA)
  assertEq failed active0 TabA
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (Rect bx by bw bh : _) -> do
      (resp1, active1) <- runClickPair ctx inp0 (ui TabA) (V2 (bx + bw / 2) (by + bh / 2))
      assert failed (respChanged resp1 && active1 == TabB)
    [] -> assert failed False

data TabMsg = MsgSelect DummyTab | MsgClose DummyTab
  deriving (Eq, Show)

runTabsEmitTest :: Context -> IORef Int -> IO ()
runTabsEmitTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabsEmit MsgSelect curTab
        [ tab TabA "Alpha" (label_ "Body A")
        , tab TabB "Beta" (label_ "Body B")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (Rect bx by bw bh : _) -> do
      let (press, release) = clickPair inp0 (V2 (bx + bw / 2) (by + bh / 2))
      _ <- runFrame ctx press (ui TabA)
      (_, msgs, _, _) <- runFrame ctx release (ui TabA)
      assertEq failed (decodeMessages msgs :: [TabMsg]) [MsgSelect TabB]
    [] -> assert failed False

runTabsClosableTest :: Context -> IORef Int -> IO ()
runTabsClosableTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabsEx TabUnderline TabTop curTab
        [ closableTab TabA "Alpha" (label_ "Body A")
        , closableTab TabB "Beta" (label_ "Body B")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Alpha" `T.isInfixOf` txt] of
    (Rect ax ay aw ah : _) -> do
      (tResp, activeTab) <- runClickPair ctx inp0 (ui TabA) (V2 (ax + aw + 16) (ay + ah / 2))
      assertEq failed (tabClosed tResp) (Just TabA)
      assertEq failed activeTab TabA
    [] -> assert failed False

runTabsStatePersistenceTest :: Context -> IORef Int -> IO ()
runTabsStatePersistenceTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabs curTab
        [ tab TabA "A" $
            withKey ("tab-a" :: T.Text) $
              withKey ("flag" :: T.Text) $ do
                (flag, setFlag) <- useFlag False
                clickButton "ToggleA" (setFlag (not flag))
                label_ (if flag then "FlagIsOn" else "FlagIsOff")
        , tab TabB "B" (label_ "OtherTab")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans0 <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans0, "ToggleA" `T.isInfixOf` txt] of
    (Rect cx cy cw ch : _) -> do
      runClick ctx inp0 (ui TabA) (V2 (cx + cw / 2) (cy + ch / 2))
      _ <- runFrame ctx inp0 (ui TabA)
      spans1 <- collectTextSpans ctx
      assertSpansHas failed "FlagIsOn" spans1

      _ <- runFrame ctx inp0 (ui TabB)
      spans2 <- collectTextSpans ctx
      assertSpansHas failed "OtherTab" spans2
      assert failed (not (spansHas "FlagIsOn" spans2))

      _ <- runFrame ctx inp0 (ui TabA)
      spans3 <- collectTextSpans ctx
      assertSpansHas failed "FlagIsOn" spans3
    [] -> assert failed False

runTabsDamageTest :: Context -> IORef Int -> IO ()
runTabsDamageTest _ failed = do
  ctx <- newContext
  let inp0 = withInputOff 300 100
      ui curTab = tabs curTab
        [ tab TabA "Alpha" (label_ "Body A with some text")
        , tab TabB "Beta" (label_ "Body B different widgets")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  _ <- takeDamage ctx
  _ <- runFrame ctx inp0 (ui TabA)
  dIdle <- takeDamage ctx
  assert failed (dIdle /= DamageFull)
  spansIdle <- collectTextSpans ctx
  assertSpansHas failed "Body A" spansIdle

  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (Rect bx by bw bh : _) -> do
      let (press, release) = clickPair inp0 (V2 (bx + bw / 2) (by + bh / 2))
      _ <- runFrame ctx press (ui TabA)
      ((resp, newTab), _, _, _) <- runFrame ctx release (ui TabA)
      assert failed (respChanged resp && newTab == TabB)
      spansSwitch <- collectTextSpans ctx
      assertSpansHas failed "Body B" spansSwitch
      assert failed (not (spansHas "Body A" spansSwitch))
      dSwitch <- takeDamage ctx
      assertEq failed dSwitch DamageFull

      _ <- runFrame ctx inp0 (ui TabB)
      dTabB <- takeDamage ctx
      assertEq failed dTabB DamageFull

      _ <- runFrame ctx inp0 (ui TabB)
      dSettled <- takeDamage ctx
      assert failed (dSettled /= DamageFull)
    [] -> assert failed False

runTabsContentDamageTest :: Context -> IORef Int -> IO ()
runTabsContentDamageTest _ failed = do
  ctx <- newContext
  let inp0 = withInputOff 320 200
      ui = do
        (click, setClick) <- useText ""
        row defaultLayout $ do
          btn <- button "OK"
          onClick btn (setClick "OK")
          _ <- tabs ("Controls" :: T.Text)
            [ tab "Controls" "Controls" $
                kv "Clicked" (if T.null click then "-" else click)
            ]
          pure btn
  _ <- runFrame ctx inp0 ui
  (btn, _, _, _) <- runFrame ctx inp0 ui
  let (press, release) = clickPair inp0 (centerOf btn)
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  spans1 <- collectTextSpans ctx
  assertSpansHas failed "OK" spans1
  _ <- runFrame ctx inp0 ui
  spans2 <- collectTextSpans ctx
  assertSpansHas failed "OK" spans2
