module Cases.Adornment (tests) where

import Spec
import NanoUI.Adornment qualified as A
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (flowChildrenInOrder, getNodeRect, lookupNodeByWidgetId)

tests :: [Spec]
tests =
  [ spec "adorned-field" runAdornedFieldTest
  , spec "adorned-field-affix-change" runAdornedFieldAffixChangeTest
  , spec "adorned-button" runAdornedButtonTest
  , spec "adornment-ids" runAdornmentIdsTest
  , spec "text-input-click-start" runTextInputClickStartTest
  , spec "adorned-views" runAdornedViewsTest
  , spec "adorned-field-control" runAdornedFieldControlTest
  , spec "adorned-button-control" runAdornedButtonControlTest
  , spec "button-content" runButtonContentTest
  , spec "search-span-clip" runSearchSpanClipTest
  , spec "adornment-clip" runAdornmentClipTest
  ]

squareIcon :: IO Svg
squareIcon = either fail pure (parseSvg "<svg viewBox='0 0 24 24'><rect x='2' y='2' width='20' height='20'/></svg>")

-- | The rects of a widget's adornments, leading then trailing, each side in
-- the order it was given. Each side is a row inside the widget.
adornmentRects :: Context -> Response -> IO [Rect]
adornmentRects ctx resp = do
  let na = ctxNodeArena ctx
      rowsOf = flowChildrenInOrder na
  lookupNodeByWidgetId na (respId resp) >>= \case
    Nothing -> pure []
    Just idx -> rowsOf idx >>= fmap concat . mapM (rowsOf >=> mapM (getNodeRect na))

rectRight :: Rect -> Float
rectRight (Rect x _ w _) = x + w

-- | The clip rects of the spans whose text is @txt@.
spanClips :: T.Text -> [(Rect, T.Text, a, b, Rect)] -> [Rect]
spanClips txt spans = [clip | (_, t, _, _, clip) <- spans, t == txt]

-- | A field lays out a leading icon and a trailing affix inside its box,
-- keeps its text between them, and a press on the icon edits the field.
runAdornedFieldTest :: Context -> IORef Int -> IO ()
runAdornedFieldTest ctx failed = do
  doc <- squareIcon
  let
    inp0 = withInput 400 200
    cfg =
      defaultTextInputConfig
        { ticAdornments = A.leading (A.iconSized 16 doc) <> A.trailing (A.affix "kg")
        , ticLayout = fixedW 240 (ticLayout defaultTextInputConfig)
        }
    ui = column (textInputConfigured' cfg "12")
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  [iconR@(Rect _ _ iw ih), unitR] <- adornmentRects ctx resp
  [(valueR, valueClip)] <- pure [(r, clip) | (r, "12", _, _, clip) <- spans]
  Just unitSpan <- pure (spanRectOf "kg" spans)
  let field = respRect resp
  assertEq failed (16, 16) (iw, ih)
  assert failed (covers field iconR && covers field unitR)
  assert failed (abs (v2Y (spanCenter iconR) - v2Y (spanCenter field)) <= 1)
  -- The text starts past the icon and its clip stops short of the affix.
  assert failed (rectX valueR >= rectRight iconR + 1)
  assert failed (rectRight valueClip <= rectX unitR)
  assert failed (rectX unitSpan >= rectRight valueR && rectRight unitSpan <= rectRight field)
  -- A press on the icon focuses the field with the caret at the start.
  _ <- runClick ctx inp0 ui (spanCenter iconR)
  assertEq failed "312" . snd =<< evalUi ctx (inp0 {inputChars = "3"}) ui

-- | A longer affix on a later frame moves the affix and narrows the text's
-- room. The value overflows the field, so its span's clip is the whole room.
runAdornedFieldAffixChangeTest :: Context -> IORef Int -> IO ()
runAdornedFieldAffixChangeTest ctx failed = do
  let
    inp0 = withInput 400 200
    value = T.replicate 60 "9"
    ui unit =
      column $
        textInputConfigured'
          defaultTextInputConfig {ticAdornments = A.trailing (A.affix unit), ticLayout = fixedW 200 (ticLayout defaultTextInputConfig)}
          value
    layoutWith unit = do
      _ <- warmup2 ctx inp0 (ui unit)
      spans <- collectTextSpans ctx
      [clip] <- pure (spanClips value spans)
      Just unitR <- pure (spanRectOf unit spans)
      assert failed (rectRight clip <= rectX unitR)
      pure (rectRight clip, rectX unitR)
  (shortClip, kg) <- layoutWith "kg"
  (longClip, kilograms) <- layoutWith "kilograms"
  assert failed (kilograms < kg && longClip < shortClip)

-- | A button centres its icon and label together, in the label's colour,
-- repainted with its hover; an icon alone makes a square button; a trailing
-- icon follows the label; and a press on the icon clicks the button.
runAdornedButtonTest :: Context -> IORef Int -> IO ()
runAdornedButtonTest ctx failed = do
  doc <- squareIcon
  theme <- getTheme ctx
  let
    inp0 = withInputOff 400 300
    ui = column $ do
      -- Side by side, so both heights snap from the same y.
      (a, d) <- row ((,) <$> iconButton' doc "Save" <*> button' "Save")
      b <- buttonConfigured' defaultButtonConfig {bcAdornments = A.trailing (A.iconSized 16 doc)} "Next"
      c <- iconButton' doc ""
      pure (a, b, c, d)
    iconTinted iconR draw = any (\(r, col) -> col == styleFg (themeButton theme) && covers (rectInflate 1 iconR) r) <$> drawQuads draw
  ((a, b, c, d), draw) <- warmupDraw ctx inp0 ui
  spans <- collectTextSpans ctx
  [iconA] <- adornmentRects ctx a
  [iconB] <- adornmentRects ctx b
  [iconC] <- adornmentRects ctx c
  let labelIn resp lbl = [r | (r, txt, _, _, _) <- spans, txt == lbl, covers (respRect resp) r]
      ra@(Rect ax _ aw ah) = respRect a
      Rect _ _ cw ch = respRect c
      Rect _ _ dw dh = respRect d
  [labelA] <- pure (labelIn a "Save")
  [labelB] <- pure (labelIn b "Next")
  assert failed (covers ra iconA && rectX labelA >= rectRight iconA + 4)
  -- The icon and label are centred as one group.
  assert failed (abs ((rectX iconA - ax) - (ax + aw - rectRight labelA)) <= 2)
  assert failed (aw > dw)
  assertEq failed dh ah
  assert failed (rectRight labelB + 4 <= rectX iconB && rectRight iconB <= rectRight (respRect b))
  assert failed (abs (cw - ch) <= 1 && covers (respRect c) iconC)
  iconTinted iconA draw >>= assert failed
  (_, _, hoverDraw, _) <- runFrame ctx (inp0 {inputMousePos = centerOf a}) ui
  iconTinted iconA hoverDraw >>= assert failed
  (a', _, _, _) <- runClick ctx inp0 ui (spanCenter iconA)
  assert failed (respClicked a')

-- | Adornments take no ids from a widget's siblings: the widget after an
-- adorned button or field has the id it has after a plain one.
runAdornmentIdsTest :: Context -> IORef Int -> IO ()
runAdornmentIdsTest ctx failed = do
  doc <- squareIcon
  let
    inp0 = withInputOff 400 300
    ui adorned = column $ do
      _ <- if adorned then iconButton' doc "A" else button' "A"
      _ <-
        textInputConfigured'
          defaultTextInputConfig {ticAdornments = if adorned then A.leading (A.icon doc) <> A.trailing (A.affix "kg") else mempty}
          ""
      currentId
  plain <- warmup2 ctx inp0 (ui False)
  assertEq failed plain =<< warmup2 ctx inp0 (ui True)

-- | The first press on a field, left of its text, puts the caret at the
-- start: the cursor it writes is a change from the end, where a field that
-- never stored one reads it.
runTextInputClickStartTest :: Context -> IORef Int -> IO ()
runTextInputClickStartTest ctx failed = do
  let
    inp0 = withInput 400 200
    ui = column (textInput' "12")
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect x y _ h = respRect resp
  _ <- runClick ctx inp0 ui (V2 (x + 2) (y + h / 2))
  assertEq failed "312" . snd =<< evalUi ctx (inp0 {inputChars = "3"}) ui

-- | Any view adorns either side: a spinner and a label after a field's value,
-- an icon and an affix before it, in order and clear of the text; a view's
-- label takes the adornment colour; and a button keeps its label beside a
-- spinner, a press on which is the button's.
runAdornedViewsTest :: Context -> IORef Int -> IO ()
runAdornedViewsTest ctx failed = do
  doc <- squareIcon
  theme <- getTheme ctx
  let
    inp0 = withInput 400 200
    value = T.replicate 60 "9"
    cfg =
      defaultTextInputConfig
        { ticAdornments =
            A.leading (A.iconSized 16 doc) <> A.leading (A.affix "$")
              <> A.trailing (A.view (void (spinnerWith' id 14))) <> A.trailing (A.view (label "ok"))
        , ticLayout = fixedW 260 (ticLayout defaultTextInputConfig)
        }
    ui = column $ do
      (field, _) <- textInputConfigured' cfg value
      saving <- buttonConfigured' defaultButtonConfig {bcAdornments = A.leading (A.view (void (spinnerWith' id 14)))} "Saving"
      pure (field, saving)
  (field, saving) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  [iconR, dollarR, spinR, okR] <- adornmentRects ctx field
  [savingSpin] <- adornmentRects ctx saving
  [clip] <- pure (spanClips value spans)
  [okFg] <- pure [fg | (_, "ok", fg, _, _) <- spans]
  Just labelR <- pure (spanRectOf "Saving" spans)
  assert failed (all (covers (respRect field)) [iconR, dollarR, spinR, okR])
  assert failed (rectRight iconR < rectX dollarR && rectRight spinR < rectX okR)
  assert failed (rectRight dollarR <= rectX clip && rectRight clip <= rectX spinR)
  assertEq failed (14, 14) (rectW spinR, rectH spinR)
  assertEq failed (lerpColor (styleFg (themeInput theme)) (styleBg (themeInput theme)) 0.45) okFg
  assert failed (covers (respRect saving) savingSpin && rectRight savingSpin + 4 <= rectX labelR)
  (_, saving') <- runClick ctx inp0 ui (spanCenter savingSpin)
  assert failed (respClicked saving')

-- | A control in a field takes the pointer, and a disabled one leaves it to
-- the field. A press on a control in a field without focus blurs the field
-- that has it, as any button does. In the focused field, the Show button
-- clicks and takes the pointer cursor while the field keeps its focus and
-- caret, reports no click and is not hovered.
runAdornedFieldControlTest :: Context -> IORef Int -> IO ()
runAdornedFieldControlTest ctx failed = do
  showClicks <- newIORef (0 :: Int)
  let
    inp0 = withInputOff 400 200
    over r = inp0 {inputMousePos = spanCenter r}
    cfg =
      defaultTextInputConfig
        { ticPassword = True
        , ticAdornments =
            A.leading (A.control (disabledWhen True (void (buttonWith tight "Off"))))
              <> A.trailing (A.control (whenM (buttonWith tight "Show") (uiIO (modifyIORef' showClicks (+ 1)))))
        , ticLayout = fixedW 260 (ticLayout defaultTextInputConfig)
        }
    ui = column ((,) <$> textInputConfigured' cfg "ab" <*> textInput' "bb")
  ((field, _), (other, _)) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  Just showR <- pure (spanRectOf "Show" spans)
  Just offR <- pure (spanRectOf "Off" spans)
  _ <- runClick ctx inp0 ui (centerOf other)
  _ <- runClick ctx inp0 ui (spanCenter showR)
  readIORef (ctxFocusId ctx) >>= assertEq failed (WidgetId 0)
  -- Focus the field with the caret at the end of its text.
  _ <- runClick ctx inp0 ui (V2 (rectX showR - 20) (v2Y (spanCenter showR)))
  readIORef (ctxFocusId ctx) >>= assertEq failed (respId field)
  replicateM_ 2 (runFrame ctx (over showR) ui)
  (((hovered, _), _), _, _, _) <- runFrame ctx (over showR) ui
  assert failed (not (respHovered hovered))
  uiCursorKind ctx (over showR) >>= assertEq failed UiCursorPointer
  ((clicked, _), _) <- runClick ctx inp0 ui (spanCenter showR)
  assert failed (not (respClicked clicked))
  readIORef showClicks >>= assertEq failed 2
  readIORef (ctxFocusId ctx) >>= assertEq failed (respId field)
  (((_, typed), _), _, _, _) <- runFrame ctx ((over showR) {inputChars = "c"}) ui
  assertEq failed "abc" typed
  replicateM_ 2 (runFrame ctx (over offR) ui)
  readIORef (ctxHotId ctx) >>= assertEq failed (respId field)
  uiCursorKind ctx (over offR) >>= assertEq failed UiCursorText

-- | A control among a button's adornments clicks alone, also when its press
-- and release arrive in one frame with no hover before them, and the
-- button's label still clicks the button.
runAdornedButtonControlTest :: Context -> IORef Int -> IO ()
runAdornedButtonControlTest ctx failed = do
  removes <- newIORef (0 :: Int)
  let
    inp0 = withInputOff 400 200
    remove = A.trailing (A.control (whenM (buttonWith tight "x") (uiIO (modifyIORef' removes (+ 1)))))
    ui = column (buttonConfigured' defaultButtonConfig {bcAdornments = remove} "Chip")
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  Just xR <- pure (spanRectOf "x" spans)
  Just chipR <- pure (spanRectOf "Chip" spans)
  (tapped, _, _, _) <- runFrame ctx (inp0 {inputMousePos = spanCenter xR, inputButtonsPressed = buttonsFromList [MouseLeft], inputButtonsReleased = buttonsFromList [MouseLeft]}) ui
  onControl <- runClick ctx inp0 ui (spanCenter xR)
  assert failed (not (respClicked tapped || respClicked onControl))
  readIORef removes >>= assertEq failed 2
  onLabel <- runClick ctx inp0 ui (spanCenter chipR)
  assert failed (respClicked onLabel)
  readIORef removes >>= assertEq failed 2

-- | A content button lays its view out as a button lays out its label: with
-- a label as its content it is the size of 'button' with that label. A press
-- on its content clicks it, and a button inside its content is for display
-- and does not click.
runButtonContentTest :: Context -> IORef Int -> IO ()
runButtonContentTest ctx failed = do
  inner <- newIORef False
  let
    inp0 = withInputOff 400 200
    ui = column $ do
      pair <- row ((,) <$> buttonContent' (label "Save") <*> button' "Save")
      nested <- buttonContent' (whenM (button "x") (uiIO (writeIORef inner True)))
      pure (pair, nested)
  ((content, plain), nested) <- warmup2 ctx inp0 ui
  assertEq failed (rectW (respRect plain), rectH (respRect plain)) (rectW (respRect content), rectH (respRect content))
  spans <- collectTextSpans ctx
  [labelR] <- pure [r | (r, "Save", _, _, _) <- spans, covers (respRect content) r]
  ((onLabel, _), _) <- runClick ctx inp0 ui (spanCenter labelR)
  (_, onNested) <- runClick ctx inp0 ui (centerOf nested)
  assert failed (respClicked onLabel && respClicked onNested)
  readIORef inner >>= assert failed . not

-- | A search field's text spans clip short of its clear button, as paint
-- clips the text.
runSearchSpanClipTest :: Context -> IORef Int -> IO ()
runSearchSpanClipTest ctx failed = do
  let
    value = T.replicate 60 "9"
    ui = column (searchInputConfigured' defaultSearchInputConfig {sicLayout = fixedW 200 (sicLayout defaultSearchInputConfig)} value)
  (field, _) <- warmup2 ctx (withInputOff 400 200) ui
  [clip] <- spanClips value <$> collectTextSpans ctx
  assert failed (rectRight clip + 15 <= rectRight (respRect field))

-- | An adornment wider than its widget is cut off at the widget's edge.
runAdornmentClipTest :: Context -> IORef Int -> IO ()
runAdornmentClipTest ctx failed = do
  let ui = column (buttonConfigured' defaultButtonConfig {bcLayout = fixedW 60 defaultLayout, bcAdornments = A.trailing (A.affix "kilograms")} "Weigh")
  weigh <- warmup2 ctx (withInputOff 400 200) ui
  [clip] <- spanClips "kilograms" <$> collectTextSpans ctx
  assert failed (covers (respRect weigh) clip)
