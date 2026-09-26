module Scope (check, runScopeTests) where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM, forM_, unless, void)
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
  , uiIO
  )
import NanoUI.Backend (runNanoUI)
import NanoUI.Internal.Context (ctxFocusId, ctxNodeArena)
import NanoUI.Form
import NanoUI.Form.Internal.Backend
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
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
  ( NodeType (NodeButton, NodeTextArea)
  , arenaCount
  , getNodeType
  , getNodeRect
  , getWidgetId
  )
import NanoUI.Testing (Context, clearDirty, isDirty, newPixelContext, runFrame)
import NanoUI.Testing.Harness (keyInp, runClick, spanCenter, warmup2, withInputOff)

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

-- | A checkbox is a button, and these forms have no other buttons.
checkboxes :: Context -> IO [(WidgetId, Rect)]
checkboxes = controlsOf NodeButton

controlsOf :: NodeType -> Context -> IO [(WidgetId, Rect)]
controlsOf wanted ctx = do
  let
    arena = ctxNodeArena ctx
  count <- arenaCount arena
  matching <- filterM (fmap (== wanted) . getNodeType arena) [0 .. count - 1]
  forM matching $ \index -> do
    wid <- getWidgetId arena index
    Rect x y w h <- getNodeRect arena index
    pure (wid, Rect x y w h)

-- | Event-free input for a 400 by 240 window.
input :: Input
input = withInputOff 400 240

-- | Click the middle of a control.
clickOn :: Context -> NanoUI a -> Rect -> IO ()
clickOn ctx ui rect = void (runClick ctx input ui (spanCenter rect))

enabledForm :: Form Text Bool
enabledForm = inputCheckbox "enabled" False

readEnabled :: Context -> Text -> IO Bool
readEnabled ctx prefix = do
  (_, result) <- runNanoUI ctx input (runNanoForm prefix enabledForm)
  case result of
    Ditto.Ok (Ditto.Proved _ value) -> pure value
    Ditto.Error _ -> fail "checkbox form unexpectedly failed validation"

testDeferredViews :: IO ()
testDeferredViews = do
  ctx <- newPixelContext
  let
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
      clickOn ctx ui leftRect
      _ <- warmup2 ctx input ui
      check "deferred view wrote to the wrong form" =<< readEnabled ctx "left"
      check "editing one form changed another form" . not =<< readEnabled ctx "right"
    _ -> fail "expected two deferred checkbox fields"

testNestedViews :: IO ()
testNestedViews = do
  ctx <- newPixelContext
  setActiveFormPrefix ctx "host"
  let
    nested = Ditto.view (FormView (void (nanoFormLive "inner" enabledForm)))
    outer = nested *> enabledForm
    ui = nanoFormLive "outer" outer
  _ <- warmup2 ctx input ui
  controls <- checkboxes ctx
  case controls of
    [_, (_, outerRect)] -> do
      clickOn ctx ui outerRect
      _ <- warmup2 ctx input ui
      check "field following a nested form lost its owner" =<< readEnabled ctx "outer"
      check "outer field wrote to the nested form" . not =<< readEnabled ctx "inner"
      check "form evaluation or rendering leaked its prefix" . (== "host")
        =<< getActiveFormPrefix ctx
    _ -> fail "expected nested and outer checkbox fields"

testPrefixRestoration :: IO ()
testPrefixRestoration = do
  ctx <- newPixelContext
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
    ui = nanoFormSubmit "submit" "Save" (pure (42 :: Int))
  initial <- warmup2 ctx input ui
  check "form submitted before activation" (initial == Nothing)
  (submitted, _, _, _) <-
    runFrame ctx (keyInp KeyEnter input) ui
  check "valid submission did not return its value" (submitted == Just 42)
  idle <- warmup2 ctx input ui
  check "a submitted form kept emitting values on idle frames" (idle == Nothing)

testResetWidgets :: IO ()
testResetWidgets = do
  ctx <- newPixelContext
  let
    ui =
      columnWith fillW $
        (,)
          <$> nanoFormLive "reset-left" enabledForm
          <*> nanoFormLive "reset-right" enabledForm
  _ <- warmup2 ctx input ui
  controls <- checkboxes ctx
  case controls of
    [(_, leftRect), (rightId, rightRect)] -> do
      forM_ [leftRect, rightRect] (clickOn ctx ui)
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
