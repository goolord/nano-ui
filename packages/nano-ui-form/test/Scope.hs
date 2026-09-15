module Scope (check, runScopeTests) where

import Control.Exception (IOException, try)
import Control.Monad (forM, forM_, unless, void)
import Data.IORef (writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Ditto.Core qualified as Ditto
import Ditto.Types qualified as Ditto
import NanoUI
  ( Input (..)
  , Key (KeyEnter)
  , NanoUI
  , Rect (..)
  , columnWith
  , fillW
  , inputKeysFromList
  , runNanoUI
  , uiIO
  )
import NanoUI.Context (ctxFocusId, ctxNodeArena)
import NanoUI.Form
import NanoUI.Form.Backend
  ( FormStateStore (..)
  , emptyFormStateStore
  , getActiveFormPrefix
  , getFormStore
  , markFormSubmitted
  , resetFormState
  , setActiveFormPrefix
  , setFormStore
  , updateFieldInput
  , withFormPrefix
  )
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena
  ( NodeType (NodeCheckbox, NodeTextArea)
  , arenaCount
  , getNodeType
  , getRect
  , getWidgetId
  )
import NanoUI.Testing (Context, clearDirty, isDirty, newPixelContext, runFrame)
import NanoUI.Testing.Harness (clickPair, spanCenter, warmup2, withInputOff)

check :: String -> Bool -> IO ()
check message ok = unless ok (fail message)

runScopeTests :: IO ()
runScopeTests = do
  testDeferredViews
  testNestedViews
  testPrefixRestoration
  testFormInvalidation
  testSubmitPulse
  testResetWidgets
  testResetTextArea

checkboxes :: Context -> IO [(WidgetId, Rect)]
checkboxes = controlsOf NodeCheckbox

controlsOf :: NodeType -> Context -> IO [(WidgetId, Rect)]
controlsOf wanted ctx = do
  let
    arena = ctxNodeArena ctx
  count <- arenaCount arena
  concat
    <$> forM
      [0 .. count - 1]
      ( \index -> do
          nodeType <- getNodeType arena index
          if nodeType /= wanted
            then pure []
            else do
              wid <- getWidgetId arena index
              (x, y, w, h) <- getRect arena index
              pure [(wid, Rect x y w h)]
      )

enabledForm :: Form Text Bool
enabledForm = inputCheckbox "enabled" False

readEnabled :: Context -> Text -> IO Bool
readEnabled ctx prefix = do
  (_, result) <-
    runNanoUI ctx (withInputOff 400 240) (runNanoForm prefix enabledForm)
  case result of
    Ditto.Ok (Ditto.Proved _ value) -> pure value
    Ditto.Error _ -> fail "checkbox form unexpectedly failed validation"

testDeferredViews :: IO ()
testDeferredViews = do
  ctx <- newPixelContext
  let
    input = withInputOff 400 240
    ui :: NanoUI ()
    ui = columnWith fillW $ do
      (left, _) <- runNanoForm "left" enabledForm
      (right, _) <- runNanoForm "right" enabledForm
      runFormView (Ditto.unView left [] <> Ditto.unView right [])
  _ <- warmup2 ctx input ui
  controls <- checkboxes ctx
  case controls of
    [(leftId, leftRect), (rightId, _)] -> do
      check
        "same-named fields in different forms share a widget ID"
        (leftId /= rightId)
      let
        (press, release) = clickPair input (spanCenter leftRect)
      void (runFrame ctx press ui)
      void (runFrame ctx release ui)
      _ <- warmup2 ctx input ui
      check "deferred view wrote to the wrong form" =<< readEnabled ctx "left"
      check "editing one form changed another form" . not =<< readEnabled ctx "right"
    _ -> fail "expected two deferred checkbox fields"

testNestedViews :: IO ()
testNestedViews = do
  ctx <- newPixelContext
  setActiveFormPrefix ctx "host"
  let
    input = withInputOff 400 240
    nested = Ditto.view (FormView (void (nanoFormLive "inner" enabledForm)))
    outer = nested *> enabledForm
    ui = nanoFormLive "outer" outer
  _ <- warmup2 ctx input ui
  controls <- checkboxes ctx
  case controls of
    [_, (_, outerRect)] -> do
      let
        (press, release) = clickPair input (spanCenter outerRect)
      void (runFrame ctx press ui)
      void (runFrame ctx release ui)
      _ <- warmup2 ctx input ui
      check "field following a nested form lost its owner" =<< readEnabled ctx "outer"
      check "outer field wrote to the nested form" . not =<< readEnabled ctx "inner"
      check "form evaluation or rendering leaked its prefix" . (== "host")
        =<< getActiveFormPrefix ctx
    _ -> fail "expected nested and outer checkbox fields"

testPrefixRestoration :: IO ()
testPrefixRestoration = do
  ctx <- newPixelContext
  let
    input = withInputOff 400 240
  result <-
    try
      ( runNanoUI ctx input $ withFormPrefix "outer" $ withFormPrefix "inner" $ do
          uiIO (updateFieldInput ctx "inner" "value" (FormInputText "preserved"))
          uiIO (ioError (userError "form failed"))
      ) ::
      IO (Either IOException ())
  check "expected a form exception" (either (const True) (const False) result)
  check "exception leaked the active form prefix" . (== "")
    =<< getActiveFormPrefix ctx
  store <- getFormStore ctx "inner"
  check
    "prefix restoration discarded field updates"
    (Map.lookup "value" (fssInputs store) == Just (FormInputText "preserved"))

testFormInvalidation :: IO ()
testFormInvalidation = do
  ctx <- newPixelContext
  forM_
    [ updateFieldInput ctx "form" "field" (FormInputText "value")
    , markFormSubmitted ctx "form" True
    , resetFormState ctx "form"
    ]
    $ \update -> do
      clearDirty ctx
      update
      check "form mutation did not request a redraw" =<< isDirty ctx
      clearDirty ctx
      update
      check "an unchanged form mutation requested another redraw" . not
        =<< isDirty ctx

testSubmitPulse :: IO ()
testSubmitPulse = do
  ctx <- newPixelContext
  let
    input = withInputOff 400 240
    ui = nanoFormSubmit "submit" "Save" (pure (42 :: Int))
  initial <- warmup2 ctx input ui
  check "form submitted before activation" (initial == Nothing)
  (submitted, _, _, _) <-
    runFrame ctx input {inputKeys = inputKeysFromList [KeyEnter]} ui
  check "valid submission did not return its value" (submitted == Just 42)
  idle <- warmup2 ctx input ui
  check "a submitted form kept emitting values on idle frames" (idle == Nothing)

testResetWidgets :: IO ()
testResetWidgets = do
  ctx <- newPixelContext
  let
    input = withInputOff 400 240
    ui =
      columnWith fillW $
        (,)
          <$> nanoFormLive "reset-left" enabledForm
          <*> nanoFormLive "reset-right" enabledForm
  _ <- warmup2 ctx input ui
  controls <- checkboxes ctx
  case controls of
    [(_, leftRect), (rightId, rightRect)] -> do
      forM_ [leftRect, rightRect] $ \rect -> do
        let
          (press, release) = clickPair input (spanCenter rect)
        void (runFrame ctx press ui)
        void (runFrame ctx release ui)
      edited <- warmup2 ctx input ui
      check "checkboxes did not retain their edits" (edited == (Just True, Just True))
      runNanoUI ctx input (resetForm "reset-left")
      reset <- warmup2 ctx input ui
      check
        "reset did not restore defaults or changed another form"
        (reset == (Just False, Just True))
      after <- checkboxes ctx
      check
        "reset changed another form's widget identity"
        (map fst (drop 1 after) == [rightId])
      setFormStore ctx "reset-left" emptyFormStateStore
      persisted <- warmup2 ctx input ui
      check
        "writing form data revived a retired widget cache"
        (persisted == (Just False, Just True))
    _ -> fail "expected two reset-test checkboxes"

testResetTextArea :: IO ()
testResetTextArea = do
  ctx <- newPixelContext
  let
    input = withInputOff 400 240
    ui = nanoFormLive "reset-editor" (inputTextArea "notes" "initial")
  _ <- warmup2 ctx input ui
  controls <- controlsOf NodeTextArea ctx
  case controls of
    [(wid, _)] -> do
      writeIORef (ctxFocusId ctx) wid
      void (runFrame ctx input {inputChars = "edited"} ui)
      edited <- warmup2 ctx input ui
      check "text area did not retain its edit" (edited == Just "editedinitial")
      runNanoUI ctx input (resetForm "reset-editor")
      reset <- warmup2 ctx input ui
      check "reset retained the text area's cached buffer" (reset == Just "initial")
    _ -> fail "expected one reset-test text area"
