module Cases.Adornment (tests) where

import Spec
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Adornment qualified as A
import NanoUI.Internal.Context (Context (..), getFocusables)
import NanoUI.Internal.Layout.Arena (flowChildrenInOrder, getNodeRect, lookupNodeByWidgetId)
import NanoUI.Testing.Sim (Target (..), check, checkEq, click, moveTo, rectOf, result, runSim, typeText)
import Cases.SimExtra (focusId, pointerCursor)

tests :: [Spec]
tests =
  [ spec "adorned-field" runAdornedFieldTest
  , spec "adorned-button" runAdornedButtonTest
  , spec "adornment-ids" runAdornmentIdsTest
  , spec "adorned-views" runAdornedViewsTest
  , spec "adorned-field-control" runAdornedFieldControlTest
  , spec "adornment-keyboard-inert" runAdornmentKeyboardInertTest
  , spec "adornment-clip" runAdornmentClipTest
  ]

squareIcon :: Svg
squareIcon = either error id (parseSvg "<svg viewBox='0 0 24 24'><rect x='2' y='2' width='20' height='20'/></svg>")

-- | The rects of a widget's adornments, leading then trailing, each side in
-- the order it was given. Each side is a row inside the widget.
adornmentRects :: Context -> Response -> IO [Rect]
adornmentRects ctx resp = do
  let na = ctxNodeArena ctx
      rowsOf = flowChildrenInOrder na
  lookupNodeByWidgetId na (respId resp) >>= \case
    Nothing -> pure []
    Just idx -> rowsOf idx >>= fmap concat . mapM (rowsOf >=> mapM (getNodeRect na))

rightOf :: Rect -> Float
rightOf (Rect x _ w _) = x + w

-- | The clip rects of the spans whose text is @txt@.
spanClips :: Text -> [(Rect, Text, a, b, Rect)] -> [Rect]
spanClips txt spans = [clip | (_, t, _, _, clip) <- spans, t == txt]

-- | A config with these adornments and a fixed width.
adornedInput :: Float -> A.Adornments -> TextInputConfig
adornedInput w adornments =
  defaultTextInputConfig {ticAdornments = adornments, ticLayout = fixedW w (ticLayout defaultTextInputConfig)}

-- | A field lays an icon and an affix out inside its box, its text between them. A
-- press on the icon, or left of a plain field's text, puts the caret at the start.
runAdornedFieldTest :: Context -> IORef Int -> IO ()
runAdornedFieldTest ctx failed = do
  let inp0 = withInput 400 200
      cfg = adornedInput 240 (A.leading (A.iconSized 16 squareIcon) <> A.trailing (A.affix "kg"))
      ui = column ((,) <$> withValue "12" (textInputConfigured' cfg) <*> withValue "12" textInput')
      typed i = evalUi ctx inp0 {inputChars = "3"} ui >>= assertEq failed "312" . snd . i
  ((resp, _), (plain, _)) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  [iconR@(Rect _ _ iw ih), unitR] <- adornmentRects ctx resp
  let field = respRect resp
  [(valueR, valueClip)] <- pure [(r, clip) | (r, "12", _, _, clip) <- spans, covers field r]
  Just unitSpan <- pure (spanRectOf "kg" spans)
  assertEq failed (16, 16) (iw, ih)
  assert failed (covers field iconR && covers field unitR)
  assert failed (abs (v2Y (spanCenter iconR) - v2Y (spanCenter field)) <= 1)
  -- The text starts past the icon and its clip stops short of the affix.
  assert failed (rectX valueR >= rightOf iconR + 1 && rightOf valueClip <= rectX unitR)
  assert failed (rectX unitSpan >= rightOf valueR && rightOf unitSpan <= rightOf field)
  _ <- runClick ctx inp0 ui (spanCenter iconR)
  typed fst
  let Rect x y _ h = respRect plain
  _ <- runClick ctx inp0 ui (V2 (x + 2) (y + h / 2))
  typed snd

-- | A button centres its icon and label as one group in the label's colour, and a
-- press on the icon is the button's. A control adornment clicks alone, even pressed
-- and released in one frame. A content button sizes and clicks as a button does,
-- and a button inside its content is display.
runAdornedButtonTest :: Context -> IORef Int -> IO ()
runAdornedButtonTest ctx failed = do
  theme <- getTheme ctx
  removes <- newIORef (0 :: Int)
  inner <- newIORef False
  let inp0 = withInputOff 400 300
      remove = A.trailing (A.control (whenM (buttonWith tight "x") (liftIO (modifyIORef' removes (+ 1)))))
      ui = column $ do
        -- Side by side, so both heights snap from the same y.
        (a, d) <- row ((,) <$> iconButton' squareIcon "Save" <*> button' "Save")
        b <- buttonConfigured' defaultButtonConfig {bcAdornments = A.trailing (A.iconSized 16 squareIcon)} "Next"
        c <- iconButton' squareIcon ""
        chip <- buttonConfigured' defaultButtonConfig {bcAdornments = remove} "Chip"
        (content, plain) <- row ((,) <$> buttonContent' (label "Keep") <*> button' "Keep")
        nested <- buttonContent' (whenM (button "inner") (liftIO (writeIORef inner True)))
        pure [a, b, c, d, chip, content, plain, nested]
  ([a, b, c, d, _, content, plain, nested], draw) <- warmupDraw ctx inp0 ui
  spans <- collectTextSpans ctx
  [iconA] <- adornmentRects ctx a
  [iconB] <- adornmentRects ctx b
  [iconC] <- adornmentRects ctx c
  let labelIn resp lbl = [r | (r, txt, _, _, _) <- spans, txt == lbl, covers (respRect resp) r]
      iconInk (r, col) = col == styleFg (themeButton theme) && covers (rectInflate 1 iconA) r
      tinted' dd = any iconInk <$> drawQuads dd
      -- Whether a click at @p@ clicks the view's @i@th button.
      clicks i p = respClicked . (!! i) <$> runClick ctx inp0 ui p
      ra@(Rect ax _ aw ah) = respRect a
      size r = (rectW (respRect r), rectH (respRect r))
  [labelA] <- pure (labelIn a "Save")
  [labelB] <- pure (labelIn b "Next")
  [keepR] <- pure (labelIn content "Keep")
  Just xR <- pure (spanRectOf "x" spans)
  Just chipR <- pure (spanRectOf "Chip" spans)
  assert failed (covers ra iconA && rectX labelA >= rightOf iconA + 4)
  assert failed (abs ((rectX iconA - ax) - (ax + aw - rightOf labelA)) <= 2)
  assert failed (aw > rectW (respRect d))
  assertEq failed (rectH (respRect d)) ah
  assert failed (rightOf labelB + 4 <= rectX iconB && rightOf iconB <= rightOf (respRect b))
  assert failed (abs (rectW (respRect c) - rectH (respRect c)) <= 1 && covers (respRect c) iconC)
  assertEq failed (size plain) (size content)
  tinted' draw >>= assert failed
  (_, hoverDraw, _) <- runFrame ctx inp0 {inputMousePos = centerOf a} ui
  tinted' hoverDraw >>= assert failed
  clicks 0 (spanCenter iconA) >>= assert failed
  let both = buttonsFromList [MouseLeft]
      tap = inp0 {inputMousePos = spanCenter xR, inputButtonsPressed = both, inputButtonsReleased = both}
  (tapped, _, _) <- runFrame ctx tap ui
  assert failed (not (respClicked (tapped !! 4)))
  clicks 4 (spanCenter xR) >>= assert failed . not
  readIORef removes >>= assertEq failed 2
  clicks 4 (spanCenter chipR) >>= assert failed
  readIORef removes >>= assertEq failed 2
  clicks 5 (spanCenter keepR) >>= assert failed
  clicks 7 (centerOf nested) >>= assert failed
  readIORef inner >>= assert failed . not

-- | Adornments take no ids from a widget's siblings.
runAdornmentIdsTest :: Context -> IORef Int -> IO ()
runAdornmentIdsTest ctx failed = do
  let ui adorned = column $ do
        _ <- if adorned then iconButton' squareIcon "A" else button' "A"
        let adornments = if adorned then A.leading (A.icon squareIcon) <> A.trailing (A.affix "kg") else mempty
        _ <- textInputConfigured' defaultTextInputConfig {ticAdornments = adornments} (constVar "")
        currentId
  plain <- warmup2 ctx (withInputOff 400 300) (ui False)
  assertEq failed plain =<< warmup2 ctx (withInputOff 400 300) (ui True)

-- | Any view adorns either side, in order and clear of the text, its label in the
-- adornment colour; a press on a button's spinner is the button's.
runAdornedViewsTest :: Context -> IORef Int -> IO ()
runAdornedViewsTest ctx failed = do
  theme <- getTheme ctx
  let inp0 = withInput 400 200
      value = T.replicate 60 "9"
      spinning = A.view (void (spinnerWith' id 14))
      sides = [A.leading (A.iconSized 16 squareIcon), A.leading (A.affix "$"), A.trailing spinning]
      cfg = adornedInput 260 (mconcat sides <> A.trailing (A.view (label "ok")))
      ui = column $ do
        (field, _) <- withValue value (textInputConfigured' cfg)
        (,) field <$> buttonConfigured' defaultButtonConfig {bcAdornments = A.leading spinning} "Saving"
  (field, saving) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  [iconR, dollarR, spinR, okR] <- adornmentRects ctx field
  [savingSpin] <- adornmentRects ctx saving
  [clip] <- pure (spanClips value spans)
  [okFg] <- pure [fg | (_, "ok", fg, _, _) <- spans]
  Just labelR <- pure (spanRectOf "Saving" spans)
  assert failed (all (covers (respRect field)) [iconR, dollarR, spinR, okR])
  assert failed (rightOf iconR < rectX dollarR && rightOf spinR < rectX okR)
  assert failed (rightOf dollarR <= rectX clip && rightOf clip <= rectX spinR)
  assertEq failed (14, 14) (rectW spinR, rectH spinR)
  assertEq failed (lerpColor (styleFg (themeInput theme)) (styleBg (themeInput theme)) 0.45) okFg
  assert failed (covers (respRect saving) savingSpin && rightOf savingSpin + 4 <= rectX labelR)
  (_, saving') <- runClick ctx inp0 ui (spanCenter savingSpin)
  assert failed (respClicked saving')

-- | A control in a field takes the pointer (a disabled one leaves it to the field) and
-- a press on it blurs another field; in the focused field Show clicks, the field
-- keeping focus and caret, unclicked and unhovered.
runAdornedFieldControlTest :: Context -> IORef Int -> IO ()
runAdornedFieldControlTest ctx failed = do
  showClicks <- newIORef (0 :: Int)
  let showButton = whenM (buttonWith tight "Show") (liftIO (modifyIORef' showClicks (+ 1)))
      offButton = A.leading (A.control (disabledWhen True (void (buttonWith tight "Off"))))
      cfg = (adornedInput 260 (offButton <> A.trailing (A.control showButton))) {ticPassword = True}
      ui = column ((,) <$> withValue "ab" (textInputConfigured' cfg) <*> withValue "bb" textInput')
  runSim ctx failed (withInputOff 400 200) ui $ do
    ((field, _), (other, _)) <- result
    showR <- rectOf "Show"
    _ <- click (On other) >> click "Show"
    checkEq (WidgetId 0) =<< focusId
    -- Focus the field with the caret at the end of its text.
    _ <- click (At (V2 (rectX showR - 20) (v2Y (spanCenter showR))))
    checkEq (respId field) =<< focusId
    ((hovered, _), _) <- moveTo "Show" >> moveTo "Show" >> moveTo "Show"
    check (not (respHovered hovered))
    checkEq UiCursorPointer =<< pointerCursor
    ((clicked, _), _) <- click "Show"
    check (not (respClicked clicked))
    checkEq 2 =<< liftIO (readIORef showClicks)
    checkEq (respId field) =<< focusId
    checkEq "abc" . snd . fst =<< typeText "c"
    _ <- moveTo "Off" >> moveTo "Off"
    checkEq (respId field) =<< liftIO (readIORef (ctxHotId ctx))
    checkEq UiCursorText =<< pointerCursor

-- | Display content neither enters tab order nor requests the input method, even focused.
runAdornmentKeyboardInertTest :: Context -> IORef Int -> IO ()
runAdornmentKeyboardInertTest ctx failed = do
  clicks <- newIORef (0 :: Int)
  childId <- newIORef (WidgetId 0)
  let inp = withInputOff 600 300
      decorative = do
        whenM (button "Display only") (liftIO (modifyIORef' clicks (+ 1)))
        (field, _) <- withValue "Decoration" textInput'
        liftIO (writeIORef childId (respId field))
      ui = column $ do
        outer <- buttonContent' decorative
        adorned <- buttonConfigured' defaultButtonConfig {bcAdornments = A.trailing (A.view decorative)} "Adorned"
        after <- button' "After"
        pure [outer, adorned, after]
  resps <- warmup2 ctx inp ui
  assertEq failed (map respId resps) =<< getFocusables ctx
  forM_ (resps ++ take 1 resps) $ \expected -> do
    _ <- runFrame ctx (tabInp inp) ui
    assertEq failed (respId expected) =<< getFocusId ctx
    void (runFrame ctx (keyInp KeyEnter inp) ui)
  assertEq failed 0 =<< readIORef clicks
  writeIORef (ctxFocusId ctx) =<< readIORef childId
  _ <- runFrame ctx inp ui
  assertEq failed Nothing =<< textInputArea ctx

-- | Text is clipped to its room: a value short of its affix (a longer one later
-- narrows it) or a search field's clear button, an affix at its button's edge.
runAdornmentClipTest :: Context -> IORef Int -> IO ()
runAdornmentClipTest ctx failed = do
  let inp0 = withInputOff 400 200
      value = T.replicate 60 "9"
      clipped c ui txt = do
        r <- warmup2 c inp0 (column ui)
        spans <- collectTextSpans c
        [clip] <- pure (spanClips txt spans)
        pure (r, clip, spans)
      withUnit unit = do
        let field = textInputConfigured' (adornedInput 200 (A.trailing (A.affix unit))) (constVar value)
        (_, clip, spans) <- clipped ctx field value
        Just unitR <- pure (spanRectOf unit spans)
        assert failed (rightOf clip <= rectX unitR)
        pure (rightOf clip, rectX unitR)
  (shortClip, kg) <- withUnit "kg"
  (longClip, kilograms) <- withUnit "kilograms"
  assert failed (kilograms < kg && longClip < shortClip)
  let search = defaultSearchInputConfig {sicLayout = fixedW 200 (sicLayout defaultSearchInputConfig)}
  searching <- newContext
  (field, clip, _) <- clipped searching (fst <$> withValue value (searchInputConfigured' search)) value
  assert failed (rightOf clip + 15 <= rightOf (respRect field))
  weighing <- newContext
  let weigh =
        defaultButtonConfig {bcLayout = fixedW 60 defaultLayout, bcAdornments = A.trailing (A.affix "kilograms")}

  (weighed, kgClip, _) <- clipped weighing (buttonConfigured' weigh "Weigh") "kilograms"
  assert failed (covers (respRect weighed) kgClip)
