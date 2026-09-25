-- | The pointer belongs to whatever is on top. Every kind of overlay is opened
-- over every kind of pointer-driven widget, the pointer clicks, drags, wheels
-- and right-clicks on the overlay, and the widget underneath must not notice.
module Cases.PointerOwnership (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import NanoUI.Internal.Context (Context (ctxActiveId), InteractionState (..), getsInteraction, intKey, textInputMenuWidget)
import NanoUI.Internal.Store (Slot (..), WidgetStore (..), isSelectOpen, slotKey)
import NanoUI.Internal.Widgets.TextArea (buffer, loadTextAreaState, selectionAnchor)
import NanoUI.Widgets.TextBuffer (getCursor)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))

tests :: [Spec]
tests =
  [ pixelSpec "pointer-ownership" runPointerOwnershipTest
  , spec "pointer-routing-lint" runPointerRoutingLintTest
  , pixelSpec "pointer-capture" runPointerCaptureTest
  ]

-- | Something drawn over the page. Its source is declared above the widget
-- under test; @ovOpen@ lists the frames that open it, given the source's
-- response and the point that has to end up covered.
data Overlay = Overlay
  { ovName :: String
  , ovSource :: NanoUI Response
  , ovOpen :: Response -> V2 -> Input -> [Input]
  , ovRect :: Context -> Input -> Response -> IO (Maybe Rect)
  }

-- | A widget that reacts to the pointer. @vcHot@ is a point where a press,
-- drag or wheel changes it, and @vcProbe@ reads state its value leaves out.
data Victim = Victim
  { vcName :: String
  , vcUi :: NanoUI (Response, String)
  , vcHot :: Context -> Response -> IO V2
  , vcProbe :: Context -> Response -> IO String
  }

data Gesture = ClickDrag | Wheel | RightClick
  deriving (Eq, Show)

win :: Input
win = withInput 900 700

-- | Overlays the frame draws itself (the panel rect rides on their spans).
spanPanelRect :: Context -> Input -> Response -> IO (Maybe Rect)
spanPanelRect ctx inp _ = do
  spans <- collectOverlayTextSpans ctx inp
  pure (listToMaybe [r | (_, _, _, _, r) <- spans])

-- | Floating panels, which are layout nodes.
nodePanelRect :: Context -> Input -> Response -> IO (Maybe Rect)
nodePanelRect ctx _ resp = getPrevRect ctx (respId resp)

-- | A click on the source, then a frame with the pointer still there.
clickSource :: Response -> V2 -> Input -> [Input]
clickSource src _ inp =
  let pos = spanCenter (respRect src)
      (press, release) = clickPair inp pos
   in [press, release, inp {inputMousePos = pos}]

alreadyOpen :: Response -> V2 -> Input -> [Input]
alreadyOpen _ _ _ = []

numbered :: T.Text -> [Int] -> [T.Text]
numbered what = map (\i -> what <> " " <> T.pack (show i))

panelBody :: NanoUI ()
panelBody = columnWith (fixedWH 420 420) (mapM_ label (numbered "overlay line" [1 .. 6]))

overlays :: [Overlay]
overlays =
  [ Overlay
      { ovName = "text-menu"
      , ovSource = fst <$> textInput' "source text"
      , -- A right click low in the field and left of the hot point, so the
        -- menu opens down and to the right, over it.
        ovOpen = \src (V2 hx _) inp ->
          let Rect _ sy _ sh = respRect src
              (press, release) = rightClickPair inp (V2 (hx - 30) (sy + sh - 3))
           in [press, release, inp {inputMousePos = inputMousePos press}]
      , ovRect = spanPanelRect
      }
  , Overlay
      { ovName = "select-dropdown"
      , ovSource = fst <$> selectWith' fillW (numbered "option" [1 .. 12]) 0
      , ovOpen = clickSource
      , ovRect = spanPanelRect
      }
  , Overlay
      { ovName = "combo-dropdown"
      , ovSource = fst <$> comboBox' "pick" (numbered "choice" [1 .. 12]) ""
      , ovOpen = clickSource
      , ovRect = spanPanelRect
      }
  , Overlay
      { ovName = "popup"
      , ovSource = do
          anchor <- button' "popup source"
          let cfg = defaultPopupConfig (AnchorRect (respRect anchor))
          fst <$> popup True cfg {cfgPlacement = PlacementBelow, cfgDismissable = False} panelBody
      , ovOpen = alreadyOpen
      , ovRect = nodePanelRect
      }
  , Overlay
      { ovName = "window"
      , ovSource = fst <$> window True "Overlay window" panelBody
      , ovOpen = alreadyOpen
      , ovRect = nodePanelRect
      }
  , Overlay
      { ovName = "modal"
      , ovSource = fst <$> modal True "Overlay modal" panelBody
      , ovOpen = alreadyOpen
      , ovRect = nodePanelRect
      }
  ]

-- | A hot point placed within the widget's rect.
hotAt :: (Rect -> V2) -> Context -> Response -> IO V2
hotAt place _ = pure . place . respRect

noProbe :: Context -> Response -> IO String
noProbe _ _ = pure ""

shown :: Show a => NanoUI (Response, a) -> NanoUI (Response, String)
shown = fmap (fmap show)

victims :: IO [Victim]
victims = do
  -- The tab bar does not keep its own state, so the test holds it.
  tabRef <- newIORef (0 :: Int)
  let tabUi = shown . held tabRef $ \cur ->
        (\r -> (toResponse r, tabActive r))
          <$> tabBar' cur [tab i (T.pack ("tab " <> show i)) () | i <- [0 .. 3 :: Int]]
  pure
    [ Victim "button" ((,"") <$> button' "victim button") (hotAt spanCenter) noProbe
    , Victim "checkbox" (shown (checkbox' "victim checkbox" False)) (hotAt spanCenter) noProbe
    , -- The last option.
      Victim
        "radio"
        (shown (radio' ["one", "two", "three"] 0))
        (hotAt (\(Rect x y w h) -> V2 (x + w / 2) (y + h - 6)))
        noProbe
    , Victim
        "slider"
        (shown (slider' 0 100 50))
        (hotAt (\(Rect x y w h) -> V2 (x + w * 0.8) (y + h / 2)))
        noProbe
    , Victim "knob" (shown (knobWith' id 36 0 100 50)) (hotAt spanCenter) noProbe
    , Victim
        "color-picker"
        (shown (colorPicker' (colorRGBA 200 40 40 255)))
        (hotAt (spanCenter . colorPickerSvSquare))
        noProbe
    , -- The up arrow.
      Victim
        "numeric-stepper"
        (shown (numericInput' 5))
        (hotAt (\(Rect x y w h) -> V2 (x + w - 8) (y + h / 4)))
        noProbe
    , Victim "text-input" (shown (textInput' "victim text")) (hotAt spanCenter) caretProbe
    , Victim
        "text-area"
        (shown (textAreaWith' (fixedH 120) (T.unlines (replicate 30 "victim area line"))))
        (hotAt spanCenter)
        areaProbe
    , Victim "select" (shown (selectWith' fillW ["a", "b", "c"] 0)) (hotAt spanCenter) selectProbe
    , -- The second tab.
      Victim "tab-bar" tabUi (hotAt (\(Rect x y _ h) -> V2 (x + 90) (y + h / 2))) noProbe
    , Victim
        "scroll-area"
        ( do
            (wid, ()) <- scrollArea (fillW . fixedH 140) (mapM_ label (numbered "scroll line" [1 .. 40]))
            ctx <- askContext
            rect <- uiIO (getPrevRect ctx wid)
            pure (mempty {rawRespId = wid, rawRespRect = fromMaybe (Rect 0 0 0 0) rect}, "")
        )
        -- On the scrollbar lane, where a press drags the thumb.
        (hotAt (\(Rect x y w h) -> V2 (x + w - 5) (y + h * 0.7)))
        (\ctx r -> show <$> getScrollOffset ctx (respId r))
    , Victim
        "table-columns"
        ( do
            (sort, _) <- useTableSort (SortCol 0 SortAsc)
            let rows = [(name, "value") | name <- numbered "row" [1 .. 4]]
            (\r -> (toResponse r, show (tableSort r, tableColOrder r)))
              <$> tableWith (fixedH 120) "victim table" (headed "Name" fst <> headed "Value" snd) rows sort
        )
        -- On the boundary between the two headers, where a press resizes.
        ( \ctx r -> do
            spans <- collectTextSpans ctx
            let valueX = listToMaybe [x | (Rect x _ _ _, txt, _, _, _) <- spans, "Value" `T.isPrefixOf` txt]
            pure (maybe (V2 0 0) (\x -> V2 (x - 9) (rectY (respRect r) + 12)) valueX)
        )
        noProbe
    ]
  where
    caretProbe ctx r = do
      st <- getStore ctx
      let slot s = IM.lookup (slotKey s (intKey (respId r))) (storeInt st)
      pure (show (slot SlotCursor, slot SlotAnchor))
    areaProbe ctx r = do
      st <- getStore ctx
      let s = loadTextAreaState st (intKey (respId r))
      off <- getScrollOffset ctx (respId r)
      pure (show (getCursor (buffer s), selectionAnchor s, off))
    selectProbe ctx r = (\st -> show (isSelectOpen st (intKey (respId r)))) <$> getStore ctx

-- | The source on top, then the widget under test pushed right and down by
-- spacers so its hot point lands under the overlay.
page :: Maybe Overlay -> Victim -> V2 -> NanoUI (Response, (Response, String))
page mOv vc (V2 dx dy) =
  columnWith (fixedW 860) $ do
    src <- maybe (label' "no overlay") ovSource mOv
    spacer (Fixed 0) (Fixed dy)
    victim <- row $ do
      spacer (Fixed dx) (Fixed 0)
      columnWith (fixedW 320) (vcUi vc)
    pure (src, victim)

-- | A fresh context with the page warmed up and the overlay opened: the
-- widget's hot point (@offset@ into its rect) and where the overlay landed.
open :: Maybe Overlay -> Victim -> V2 -> V2 -> IO (Context, V2, Maybe Rect)
open mOv vc (V2 offX offY) shift = do
  ctx <- newPixelContext
  let frame inp = (\(a, _, _, _) -> a) <$> runFrame ctx inp (page mOv vc shift)
  _ <- frame win
  (src, (resp, _)) <- frame win
  let hot = V2 (rectX (respRect resp) + offX) (rectY (respRect resp) + offY)
  forM_ (maybe [] (\ov -> ovOpen ov src hot win) mOv) frame
  (src', _) <- frame win {inputMousePos = hot}
  mRect <- case mOv of
    Just ov -> ovRect ov ctx win {inputMousePos = hot} src'
    Nothing -> pure (Just (Rect 0 0 900 700))
  pure (ctx, hot, mRect)

runPointerOwnershipTest :: Context -> IORef Int -> IO ()
runPointerOwnershipTest _ failed = do
  vcs <- victims
  let report name msgs = forM_ msgs $ \msg -> do
        putStrLn ("  " <> name <> ": " <> msg)
        modifyIORef' failed (+ 1)
  forM_ vcs $ \vc -> do
    -- The hot point is found with nothing on top (an overlay hides the spans
    -- some widgets are located by) and carried as an offset into the widget.
    offset <- do
      ctx <- newPixelContext
      _ <- runFrame ctx win (page Nothing vc (V2 0 0))
      ((_, (resp, _)), _, _, _) <- runFrame ctx win (page Nothing vc (V2 0 0))
      V2 hx hy <- vcHot vc ctx resp
      pure (V2 (hx - rectX (respRect resp)) (hy - rectY (respRect resp)))
    -- The control: with no overlay the widget does notice, so staying quiet
    -- under an overlay is the overlay's doing.
    noticed <- scenario Nothing vc offset (V2 0 0) ClickDrag
    when (null noticed) $
      report (vcName vc) ["ignores a click and drag on its hot point, so it tests nothing"]
    forM_ overlays $ \ov -> do
      -- Where the overlay lands does not depend on the widget under it, so one
      -- unshifted run says how far to push the hot point to get it covered.
      (_, V2 hx hy, mRect) <- open (Just ov) vc offset (V2 0 0)
      let shift = case mRect of
            Just (Rect ox oy _ _) -> V2 (max 0 (ox + 40 - hx)) (max 0 (oy + 60 - hy))
            Nothing -> V2 0 0
      forM_ [ClickDrag, Wheel, RightClick] $ \gesture ->
        report (ovName ov <> " over " <> vcName vc <> " (" <> show gesture <> ")")
          =<< scenario (Just ov) vc offset shift gesture

-- | Everything the widget noticed while the gesture ran on the overlay, or on
-- the widget itself when there is none.
scenario :: Maybe Overlay -> Victim -> V2 -> V2 -> Gesture -> IO [String]
scenario mOv vc offset shift gesture = do
  (ctx, hot, mRect) <- open mOv vc offset shift
  case mRect of
    Just r | rectContains r hot -> do
      let frame inp = (\((_, v), _, _, _) -> v) <$> runFrame ctx inp (page mOv vc shift)
          at p = win {inputMousePos = p}
      (resp0, value0) <- frame (at hot)
      probe0 <- vcProbe vc ctx resp0
      focus0 <- getFocusId ctx
      let victimId = respId resp0
          V2 hx hy = hot
          dragged = V2 (hx + 28) (hy - 18)
          -- Each frame, and whether the response's flags count on it: once
          -- the gesture is over, hovering the uncovered widget is fair.
          steps = case gesture of
            ClickDrag ->
              [ (True, pressAt win hot)
              , (True, holdAt win (V2 (hx + 14) (hy - 9)))
              , (True, holdAt win dragged)
              , (True, releaseAt (holdAt win dragged))
              , (False, at dragged)
              ]
            Wheel -> [(True, (at hot) {inputScroll = V2 0 s}) | s <- [3, -3, 3]]
            RightClick ->
              let (press, release) = rightClickPair win hot
               in [(True, press), (True, release), (False, at hot)]
      fmap concat . forM (zip [1 :: Int ..] steps) $ \(i, (flags, inp)) -> do
        (resp, value) <- frame inp
        probe <- vcProbe vc ctx resp
        focus <- getFocusId ctx
        active <- readIORef (ctxActiveId ctx)
        menu <- getsInteraction ctx isTextInputMenu
        let state =
              [ (value /= value0, "value " <> value0 <> " -> " <> value)
              , (probe /= probe0, "state " <> probe0 <> " -> " <> probe)
              , (focus == victimId && focus0 /= victimId, "took focus")
              , (active == victimId, "became the active widget")
              , (fmap textInputMenuWidget menu == Just victimId, "opened its context menu")
              ]
            response =
              [ (flags && flag resp, name)
              | (flag, name) <-
                  [ (respHovered, "hovered")
                  , (respPressed, "pressed")
                  , (respClicked, "clicked")
                  , (respChanged, "changed")
                  , (respRightPressed, "right-pressed")
                  , (respRightClicked, "right-clicked")
                  ]
              ]
        pure ["frame " <> show i <> ": " <> what | (True, what) <- state <> response]
    Just r -> pure ["scenario does not overlap: hot " <> show hot <> " overlay " <> show r]
    Nothing -> pure ["overlay did not open"]

-- | The frame's unrouted input is for the code that does the routing and for
-- what watches the whole window. A widget reading its presses from it would
-- react through whatever is drawn on top, the bug the rest of this module
-- looks for, so every module that touches it is listed here. The scan starts
-- at the imports, so a module that only re-exports it passes.
runPointerRoutingLintTest :: Context -> IORef Int -> IO ()
runPointerRoutingLintTest _ failed = do
  roots <- filterM doesDirectoryExist ["lib", "packages/nano-ui/lib"]
  case roots of
    [] -> complain "library sources not found from the working directory"
    root : _ -> do
      files <- haskellFiles root
      when (length files < 50) $ complain ("only " <> show (length files) <> " sources under " <> root)
      forM_ files $ \file -> do
        src <- snd . T.breakOn "\nimport " <$> TIO.readFile file
        let rel = map (\c -> if c == '\\' then '/' else c) (makeRelative root file)
        when ("askFrameInput" `T.isInfixOf` src && rel `notElem` allowed) $
          complain (rel <> " reads the unrouted input (askFrameInput); widgets use askInput")
  where
    allowed =
      [ "NanoUI/Internal/Monad.hs" -- defines it
      , "NanoUI/Internal/Widgets/Node.hs" -- routes a floating panel's body and a dropdown's owner
      , "NanoUI/Internal/Widgets/Behavior.hs" -- useDismissable: a press anywhere else dismisses
      ]
    complain msg = putStrLn ("  " <> msg) >> modifyIORef' failed (+ 1)
    haskellFiles dir = do
      entries <- map (dir </>) <$> listDirectory dir
      dirs <- filterM doesDirectoryExist entries
      (filter ((== ".hs") . takeExtension) entries <>) . concat <$> mapM haskellFiles dirs

-- | A held button stays with the layer it went down in: a drag that started
-- on the page keeps going over a window without waking the window's widgets,
-- and one that started in a window does nothing to the page it crosses.
runPointerCaptureTest :: Context -> IORef Int -> IO ()
runPointerCaptureTest ctx failed = do
  let ui = do
        s <- columnWith (fixedW 320) (slider' 0 100 50)
        (_, b) <- window True "Capture window" (columnWith (fixedWH 300 200) (button' "inside"))
        pure (s, b)
      frame inp = (\(a, _, _, _) -> a) <$> runFrame ctx inp ui
      at p = win {inputMousePos = p}
      hold = holdAt win
  _ <- frame win
  ((slider0, _), mInside) <- frame win
  assertJust failed mInside $ \inside0 -> do
    let Rect sx sy sw sh = respRect slider0
        onTrack = V2 (sx + sw * 0.25) (sy + sh / 2)
        onButton = spanCenter (respRect inside0)
        Rect bx by _ bh = respRect inside0
        onWindowBody = V2 (bx + 4) (by + bh + 40)
    -- The scenario needs the window clear of the slider.
    assert failed (v2X onButton > sx + sw)
    -- Page to window: the slider follows the pointer, the button stays cold.
    _ <- frame (pressAt win onTrack)
    ((_, dragged), overWindow) <- frame (hold onButton)
    assertEq failed dragged 100
    assert failed (maybe False (not . respHovered) overWindow)
    _ <- frame (releaseAt (hold onButton))
    ((_, rested), free) <- frame (at onButton)
    assert failed (maybe False respHovered free)
    -- Window to page: the slider neither moves nor lights up.
    _ <- frame (pressAt win onWindowBody)
    ((overPage, crossed), _) <- frame (hold onTrack)
    assertEq failed crossed rested
    assert failed (not (respHovered overPage))
    _ <- frame (releaseAt (hold onTrack))
    ((freed, _), _) <- frame (at onTrack)
    assert failed (respHovered freed)
    -- A press is routed afresh even with the other button still down: with
    -- the right button held on the page, a left click in the window lands.
    let rightHeld = win {inputMouseRightDown = True}
        chord = pressAt rightHeld onButton
    _ <- frame (fst (rightClickPair win onTrack))
    _ <- frame chord
    (_, chorded) <- frame (releaseAt chord)
    assert failed (maybe False respClicked chorded)
