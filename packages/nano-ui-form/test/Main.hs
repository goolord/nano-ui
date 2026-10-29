module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, void)
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int8)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Ditto.Core qualified as Ditto
import Ditto.Types qualified as Ditto
import NanoUI
import NanoUI qualified as NUI
import NanoUI.Backend (runNanoUI)
import NanoUI.Form
import NanoUI.Form.Internal.Backend
import NanoUI.Internal.Context (ctxFocusId)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Testing (Context, clearDirty, collectTextSpans, isDirty, newPixelContext, runFrame)
import NanoUI.Var (peekVar)
import NanoUI.Testing.Harness (hasText, keyInp, nodesOfType, runClick, spanCenter, warmup2, withInputOff)

check :: String -> Bool -> IO ()
check message ok = unless ok (fail message)

type Checked a = Ditto.Result Text (Ditto.Proved a)

-- | A form's value, failing on validation errors.
okValue :: String -> Checked a -> IO a
okValue msg = \case
  Ditto.Ok (Ditto.Proved _ value) -> pure value
  Ditto.Error errs -> fail (msg <> ": " <> show errs)

-- | A form's validation errors, failing when it has a value.
errorsOf :: String -> Ditto.Result Text a -> IO [(Ditto.FormRange, Text)]
errorsOf msg = \case
  Ditto.Error errs -> pure errs
  Ditto.Ok _ -> fail (msg <> ": expected validation errors")

-- | Event-free input for a 400 by 240 window.
input :: Input
input = withInputOff 400 240

-- | A fresh context and form owner.
fresh :: IO (Context, FormState)
fresh = (,) <$> newPixelContext <*> newFormState

-- | Run a form once, without its view.
runForm :: FormState -> Context -> Text -> Form Text a -> IO (Ditto.View Text FormView, Checked a)
runForm owner ctx prefix form = runNanoUI ctx input (runNanoForm owner prefix form)

-- | Run a form once, without its view, and return its value.
formValue :: FormState -> Context -> Text -> Form Text a -> IO a
formValue owner ctx prefix form = okValue (T.unpack prefix) . snd =<< runForm owner ctx prefix form

-- | Draw a form's view with the given errors.
drawView :: Context -> Ditto.View Text FormView -> [(Ditto.FormRange, Text)] -> IO ()
drawView ctx v errs = runNanoUI ctx input (runFormView (Ditto.unView v errs))

decodeText :: FormInput -> Either Text Text
decodeText (FormInputText t) = Right t
decodeText _ = Left "Expected text"

enabledForm :: Form Text Bool
enabledForm = inputCheckbox "enabled" False

-- | Focus the frame's one control of a type.
focusOnly :: NodeType -> Context -> IO ()
focusOnly ty ctx =
  nodesOfType ty ctx >>= \case
    [(wid, _)] -> writeIORef (ctxFocusId ctx) wid
    other -> fail ("expected one " <> show ty <> ", got " <> show (length other))

main :: IO ()
main = do
  testCustomFields
  testCommitOnly
  testCaptions
  testValues
  testErrorsKeepSiblings
  testFormOwnership
  testPrefixRestoration
  testFormInvalidation
  testSubmit
  testColorEdit
  testMarkSubmitted
  testReset
  putStrLn "nano-ui-form tests passed."

-- | A custom field publishes value changes without a response flag, is
-- named by its key rather than its label, and passes decoder errors on.
testCustomFields :: IO ()
testCustomFields = do
  (ctx, owner) <- fresh
  let
    customForm :: Form Text (Text, Text)
    customForm =
      (,)
        <$> inputWidget
          (Just "internal-key")
          decodeText
          OnChange
          FormInputText
          ( \v -> do
              NUI.label "Visible caption"
              NUI.modifyVar v (\value -> if value == "initial" then "edited" else value)
              pure mempty
          )
          "initial"
        <*> inputWidget Nothing decodeText (OnChangeOr NUI.respChanged) FormInputText NUI.textInput' "automatic"
    current = formValue owner ctx "custom" customForm
  (customView, _) <- runForm owner ctx "custom" customForm
  drawView ctx customView []
  check "custom fields did not publish their changes" . (== ("edited", "automatic")) =<< current
  updateFieldInput owner ctx "custom" "internal-key" (FormInputText "external")
  check "custom field identity depends on its label" . (== ("external", "automatic")) =<< current
  updateFieldInput owner ctx "custom" "internal-key" (FormInputBool True)
  errs <- errorsOf "custom decoder" . snd =<< runForm owner ctx "custom" customForm
  check "custom field decoder errors did not reach ditto" (map snd errs == ["Expected text"])

-- | A commit-only field holds uncommitted edits as a draft and publishes
-- them only when committed; an outside change replaces the draft.
testCommitOnly :: IO ()
testCommitOnly = do
  (ctx, owner) <- fresh
  commitNow <- newIORef False
  shown <- newIORef ""
  let
    commitForm :: Form Text Text
    commitForm =
      inputWidget
        (Just "draft")
        decodeText
        (OnlyWhen NUI.respSubmitted)
        FormInputText
        ( \v -> do
            value <- peekVar v
            submitted <- liftIO (writeIORef shown value >> readIORef commitNow)
            NUI.modifyVar v (\x -> if x == "a" then "ab" else x)
            pure (NUI.setSubmitted submitted mempty)
        )
        "a"
    runCommit = runForm owner ctx "commit" commitForm
    drawCommit (v, _) = drawView ctx v []
  drawCommit =<< runCommit
  editing <- runCommit
  check "a commit-only field published an uncommitted edit" . (== "a") =<< okValue "editing" (snd editing)
  drawCommit editing
  check "a commit-only field does not show its held edit" . (== "ab") =<< readIORef shown
  writeIORef commitNow True
  drawCommit editing
  check "a commit-only field did not publish on commit" . (== "ab") =<< formValue owner ctx "commit" commitForm
  writeIORef commitNow False
  forM_ ["a", "external"] $ \t -> do
    updateFieldInput owner ctx "commit" "draft" (FormInputText t)
    drawCommit =<< runCommit
  check "an outside change did not replace a held edit" . (== "external") =<< readIORef shown

-- | A named field keeps its value when its caption changes, and built-in
-- inputs draw the current caption.
testCaptions :: IO ()
testCaptions = do
  (ctx, owner) <- fresh
  let
    renderCaption spec = columnWith tight $ do
      (fieldView, result) <- runNanoForm owner "captions" (inputText spec "initial" :: Form Text Text)
      runFormView (Ditto.unView fieldView [])
      pure result
    firstCaption = (named "stable-key") {fieldLabel = Just "First caption"}
  _ <- runFrame ctx input (renderCaption firstCaption)
  updateFieldInput owner ctx "captions" "stable-key" (FormInputText "kept")
  (result, _, _) <- runFrame ctx input (renderCaption firstCaption {fieldLabel = Just "Second caption"})
  check "a caption change lost the named field's value" . (== "kept") =<< okValue "captions" result
  spans <- collectTextSpans ctx
  check "built-in inputs do not draw their caption" $
    hasText "Second caption" spans && not (hasText "First caption" spans)

-- | Initial and decoded values of collection, enum and number fields, and
-- the errors of failing proofs.
testValues :: IO ()
testValues = do
  (ctx, owner) <- fresh
  let
    collectionForm =
      (,,)
        <$> inputSelect "select" (Seq.fromList ["First", "Second"]) 1
        <*> inputRadio "radio" (Seq.fromList ["First", "Second"]) 0
        <*> inputSelect unnamed (Just "Only") 0
  check "foldable options lost their initial indices" . (== (1, 0, 0 :: Int))
    =<< formValue owner ctx "collections" collectionForm
  let
    failing =
      (,)
        <$> (inputText "name" "" `prove` notEmpty "Name is required")
        <*> (inputSlider "age" 0 100 12 `prove` inRange 18 100 (const "Must be at least 18"))
  errs <- errorsOf "failing" . snd =<< runForm owner ctx "failing" failing
  check "failing proofs did not both report" $
    sort (map snd errs) == ["Must be at least 18", "Name is required"]
  inRangeNaN <- proofFunction (inRange 0 1 (const ("Out of range" :: Text))) (0 / 0 :: Double)
  check "inRange accepts NaN" (isLeft inRangeNaN)
  -- ditto's number proofs read a String, as NanoUI.Form.Validation shows.
  age <- formValue owner ctx "age" $
    (T.unpack <$> inputText "age" "42") `prove` decimal (const "Not a number") (0 :: Int)
  check "a number proof did not parse a text field" (age == 42)
  -- Enum fields use zero-based widget indices whatever the enum's bounds,
  -- and clamp indices outside them.
  let
    enumForm :: Form Text (Int8, Int8, Int8)
    enumForm =
      (,,) <$> inputEnumSelect "select" (-42) <*> inputEnumRadio "radio" 42 <*> inputEnumSelect unnamed (-12)
    enums = formValue owner ctx "enums" enumForm
  check "enum fields lost their initial values" . (== (-42, 42, -12)) =<< enums
  forM_ [(0, 255), (-10, 300)] $ \(sel, rad) -> do
    updateFieldInput owner ctx "enums" "select" (FormInputInt sel)
    updateFieldInput owner ctx "enums" "radio" (FormInputInt rad)
    check ("enum indices " <> show (sel, rad)) . (== (minBound, maxBound, -12)) =<< enums

-- | An error appearing and clearing on one field leaves its siblings'
-- values alone, and a long error message is drawn whole.
testErrorsKeepSiblings :: IO ()
testErrorsKeepSiblings = do
  (ctx, owner) <- fresh
  let
    multiForm =
      (,,)
        <$> withFieldErrors (inputText "user" "Ada" `prove` minLength 3 (const "Too short"))
        <*> withFieldErrors (inputSlider "age" 10 100 25 `prove` inRange 18 100 (const "Must be 18+"))
        <*> withFieldErrors (inputText "bio" "Bio text" `prove` notEmpty "Bio required")
    valid msg want = do
      (v, r) <- runForm owner ctx "multi" multiForm
      check msg . (== want) =<< okValue msg r
      drawView ctx v []
  valid "initial values" ("Ada", 25 :: Float, "Bio text")
  updateFieldInput owner ctx "multi" "age" (FormInputFloat 15)
  (v2, r2) <- runForm owner ctx "multi" multiForm
  errs <- errorsOf "age 15" r2
  check "age did not fail alone" (length errs == 1)
  drawView ctx v2 errs
  updateFieldInput owner ctx "multi" "bio" (FormInputText "Bio modified")
  updateFieldInput owner ctx "multi" "age" (FormInputFloat 30)
  valid "fixing the age reset a sibling" ("Ada", 30, "Bio modified")
  let
    message = "Invalid email address format (e.g. name@domain.com)"
    emailForm = withFieldErrors (inputText "email" "bad-email" `prove` validEmail (const message))
  (v, r) <- runForm owner ctx "longError" (emailForm :: Form Text Text)
  emailErrs <- errorsOf "email" r
  check "the email error did not report once" (length emailErrs == 1)
  _ <- runFrame ctx (input {inputWindowSize = NUI.Size 60 20}) $
    columnWith (tight . minW 300 . maxW 360) (runFormView (Ditto.unView v emailErrs))
  shown <- T.unwords . map (\(_, t, _, _, _) -> t) <$> collectTextSpans ctx
  check "the long email error lost text" (all (`T.isInfixOf` shown) ["Invalid email", "name@domain.com"])

-- | Clicking a checkbox writes only to the form that owns it: same-named
-- fields of forms whose views are drawn later, and a field after a form
-- nested in another.
testFormOwnership :: IO ()
testFormOwnership = do
  let
    deferred owner = columnWith fillW $ do
      (left, _) <- runNanoForm owner "left" enabledForm
      (right, _) <- runNanoForm owner "right" enabledForm
      runFormView (Ditto.unView left [] <> Ditto.unView right [])
    nested owner = void . nanoFormLive owner "outer" $
      Ditto.view (FormView (void (nanoFormLive owner "inner" enabledForm))) *> enabledForm
  forM_ [(deferred, 0, "left", "right"), (nested, 1, "outer", "inner")] $ \(mkUi, clicked, own, other) -> do
    (ctx, owner) <- fresh
    let ui = mkUi owner
    _ <- warmup2 ctx input ui
    nodesOfType NodeButton ctx >>= \case
      controls@[(a, _), (b, _)] -> do
        check "same-named fields in different forms share a widget ID" (a /= b)
        void (runClick ctx input ui (spanCenter (snd (controls !! clicked))))
      found -> fail ("expected two checkboxes, got " <> show (length found))
    _ <- warmup2 ctx input ui
    check ("the click did not reach " <> T.unpack own) =<< formValue owner ctx own enabledForm
    check ("the click wrote to " <> T.unpack other) . not =<< formValue owner ctx other enabledForm

-- | A form scope that throws restores the enclosing one and keeps the
-- field updates made before the throw.
testPrefixRestoration :: IO ()
testPrefixRestoration = do
  (ctx, owner) <- fresh
  (prefix, result) <- runNanoUI ctx input $ runFormUI owner "outer" $ do
    result <- liftNanoUI . liftIO . try $ runNanoUI ctx input $ runFormUI owner "inner" $ do
      FormScope innerOwner innerPrefix <- askFormScope
      liftNanoUI $ liftIO $ do
        updateFieldInput innerOwner ctx innerPrefix "value" (FormInputText "preserved")
        ioError (userError "form failed")
    FormScope _ prefix <- askFormScope
    pure (prefix, result :: Either IOException ())
  check "expected a form exception" (isLeft result)
  check "exception changed the enclosing form owner" (prefix == "outer")
  store <- getFormStore owner "inner"
  check "prefix restoration discarded field updates" $
    Map.lookup "value" (fssInputs store) == Just (FormInputText "preserved")

-- | Each form mutation requests one redraw, and repeating it none.
testFormInvalidation :: IO ()
testFormInvalidation = do
  (ctx, owner) <- fresh
  forM_
    [ updateFieldInput owner ctx "form" "field" (FormInputText "value")
    , markFormSubmitted owner ctx "form" True
    , resetFormState owner ctx "form"
    ]
    $ \update -> forM_ [True, False] $ \dirties -> do
      clearDirty ctx
      update
      check "a form mutation's redraw request is wrong" . (== dirties) =<< isDirty ctx

-- | Enter submits a form's value once, including text typed in the same
-- frame, for a field that publishes each edit and one that publishes on
-- Enter. Enter in a text area inserts a newline instead.
testSubmit :: IO ()
testSubmit = do
  (ctx, owner) <- fresh
  let ui = nanoFormSubmit owner "submit" "Save" (pure (42 :: Int))
  check "form submitted before activation" . (== Nothing) =<< warmup2 ctx input ui
  (submitted, _, _) <- runFrame ctx (keyInp KeyEnter input) ui
  check "valid submission did not return its value" (submitted == Just 42)
  check "a submitted form kept emitting values on idle frames" . (== Nothing) =<< warmup2 ctx input ui
  let onEnter = inputWidget (Just "query") decodeText (OnlyWhen respSubmitted) FormInputText textInput' ""
  forM_
    [ ("submit-typed", inputText "query" "", NodeTextInput, Just "abc")
    , ("submit-on-enter", onEnter, NodeTextInput, Just "abc")
    , ("submit-notes", inputTextArea "notes" "", NodeTextArea, Nothing)
    ]
    $ \(prefix, field, ty, want) -> do
      (fctx, fowner) <- fresh
      let fieldUi = nanoFormSubmit fowner prefix "Save" (field `prove` notEmpty "Required")
      _ <- warmup2 fctx input fieldUi
      focusOnly ty fctx
      _ <- warmup2 fctx input fieldUi
      (got, _, _) <- runFrame fctx (keyInp KeyEnter input {inputChars = "abc"}) fieldUi
      check ("Enter submitted the wrong value: " <> show prefix) (got == want)

-- | A colour field's value is the colour its picker shows: a picker without
-- an alpha bar makes an edited translucent colour opaque.
testColorEdit :: IO ()
testColorEdit = do
  (ctx, owner) <- fresh
  let
    overlay = colorRGBA 0 0 0 128
    ui = nanoFormLive owner "colour" (inputColor "Overlay" overlay)
  _ <- warmup2 ctx input ui
  -- The picker's first part is its saturation / value field.
  nodesOfType NodeColorPicker ctx >>= \case
    (wid, _) : _ -> writeIORef (ctxFocusId ctx) wid
    [] -> fail "expected a colour picker"
  _ <- warmup2 ctx input ui
  void (runFrame ctx (keyInp KeyUp input) ui)
  edited <- warmup2 ctx input ui
  check "Up did not brighten the colour" (edited /= Just overlay)
  check "the field's colour is not the opaque one its picker shows" (fmap colorA edited == Just 255)

-- | The app's own button submits a 'FormOnSubmit' form through
-- 'markSubmitted', and the form then shows its errors.
testMarkSubmitted :: IO ()
testMarkSubmitted = do
  (ctx, owner) <- fresh
  let
    form = withFieldErrors (inputText "name" "" `prove` notEmpty "Name is required")
    ui = nanoFormEx owner defaultFormConfig {fcMode = FormOnSubmit} "on-submit" form
    -- The message wraps, so look for its last word.
    errorShown = hasText "required" <$> collectTextSpans ctx
  _ <- warmup2 ctx input ui
  check "a FormOnSubmit form showed errors before submission" . not =<< errorShown
  runNanoUI ctx input (markSubmitted owner "on-submit")
  _ <- warmup2 ctx input ui
  check "markSubmitted did not show the form's errors" =<< errorShown

-- | Resetting a form restores its defaults, including a text area's cached
-- buffer, and leaves other forms' values and widget ids alone.
testReset :: IO ()
testReset = do
  (ctx, owner) <- fresh
  let
    ui =
      columnWith fillW $
        (,)
          <$> nanoFormLive owner "reset-left" enabledForm
          <*> nanoFormLive owner "reset-right" enabledForm
    values = warmup2 ctx input ui
  _ <- values
  controls <- nodesOfType NodeButton ctx
  forM_ controls $ \(_, r) -> void (runClick ctx input ui (spanCenter r))
  check "checkboxes did not retain their edits" . (== (Just True, Just True)) =<< values
  runNanoUI ctx input (resetForm owner "reset-left")
  check "reset did not restore defaults or changed another form" . (== (Just False, Just True)) =<< values
  after <- nodesOfType NodeButton ctx
  check "reset changed another form's widget identity" (map fst (drop 1 after) == map fst (drop 1 controls))
  setFormStore owner ctx "reset-left" emptyFormStateStore
  check "writing form data revived a retired widget cache" . (== (Just False, Just True)) =<< values
  (actx, aowner) <- fresh
  let editor = nanoFormLive aowner "reset-editor" (inputTextArea "notes" "initial")
  _ <- warmup2 actx input editor
  focusOnly NodeTextArea actx
  void (runFrame actx input {inputChars = "edited"} editor)
  check "text area did not retain its edit" . (== Just "editedinitial") =<< warmup2 actx input editor
  runNanoUI actx input (resetForm aowner "reset-editor")
  check "reset retained the text area's cached buffer" . (== Just "initial") =<< warmup2 actx input editor
