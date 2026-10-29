{-# OPTIONS_GHC -O0 #-}
-- | The pointer belongs to whatever is on top. Every kind of overlay is opened
-- over every kind of pointer-driven widget, the pointer clicks, drags, wheels
-- and right-clicks on the overlay, and the widget underneath must not notice.
module Cases.PointerOwnership (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as TIO
import NanoUI.Internal.Context
  ( Context (ctxActiveId)
  , InteractionState (..)
  , getsInteraction
  , intKey
  , textInputMenuWidget
  )
import NanoUI.Internal.Store (Slot (..), WidgetStore (..), isSelectOpen, slotKey)
import NanoUI.Internal.Widgets.TextArea (loadTextAreaState, tasBuffer, tasSelectionAnchor)
import NanoUI.Testing.Sim
import NanoUI.Widgets.TextBuffer (getCursor)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))

tests :: [Spec]
tests =
  [ pixelSpec "pointer-ownership" runPointerOwnershipTest
  , spec "pointer-routing-lint" runPointerRoutingLintTest
  , pixelSimSpec "pointer-capture" 900 700 captureUi pointerCapture
  , simSpec "color-picker-bar-slack" 600 500 slackUi colorPickerBarSlack
  ]

red :: Color
red = colorRGBA 200 40 40 255

slackUi :: NanoUI (Response, Color)
slackUi = columnWith tight $ do
  _ <- buttonWith' (pinAt 500 400 . fixedWH 30 30) "x"
  columnWith (pointer PointerBlock . tight) (withValue red colorPicker')

-- | A colour picker's hue bar takes presses a little beside itself, even in a
-- view with a pinned node (where presses check what covers their widget).
colorPickerBarSlack :: Sim (Response, Color) ()
colorPickerBarSlack = do
  -- The field, then the hue bar; down its middle stretch, off hue 0 at either end.
  _ : Rect hx hy _ hh : _ <- map snd <$> onContext (nodesOfType NodeColorPicker)
  let p = At (V2 (hx - 1) (hy + hh * 0.3))
  (_, c) <- moveTo p >> press p >> moveTo (At (V2 (hx - 1) (hy + hh * 0.5)))
  check (c /= red)

-- | The page's result: the overlay's source, and the widget under test.
type Page = (Response, (Response, String))

-- | Something drawn over the page. Its source is declared above the widget
-- under test; @ovOpen@ opens it, given the source's response and the point
-- that has to end up covered, and @ovRect@ finds where it landed.
data Overlay = Overlay
  { ovName :: String
  , ovSource :: NanoUI Response
  , ovOpen :: Response -> V2 -> Sim Page ()
  , ovRect :: Response -> Sim Page (Maybe Rect)
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

numbered :: T.Text -> [Int] -> [T.Text]
numbered what = map (\i -> what <> " " <> T.pack (show i))

overlays :: [Overlay]
overlays =
  [ -- A right click low in the field and left of the hot point, so the menu
    -- opens down and to the right, over it.
    Overlay "text-menu" (fst <$> withValue "source text" textInput') openMenu spanPanelRect
  , Overlay "select-dropdown" (fst <$> withValue 0 (selectWith' fillW options)) clickSource spanPanelRect
  , Overlay "combo-dropdown" (fst <$> withValue "" (comboBox' "pick" options)) clickSource spanPanelRect
  , Overlay "popup" popupSource alreadyOpen nodePanelRect
  , Overlay "window" (fst <$> window' (constVar True) "Overlay window" panelBody) alreadyOpen nodePanelRect
  , Overlay "modal" (fst <$> modal' (constVar True) "Overlay modal" panelBody) alreadyOpen nodePanelRect
  ]
  where
    options = numbered "option" [1 .. 12]
    openMenu src (V2 hx _) =
      let Rect _ sy _ sh = respRect src in void (rightClick (At (V2 (hx - 30) (sy + sh - 3))))
    clickSource src _ = void (click (On src))
    alreadyOpen _ _ = pure ()
    popupSource = do
      anchor <- button' "popup source"
      let cfg = defaultPopupConfig (AnchorRect (respRect anchor))
      fst <$> popup' (constVar True) cfg {cfgPlacement = PlacementBelow, cfgDismissable = False} panelBody
    panelBody = columnWith (fixedWH 420 420) (mapM_ label (numbered "overlay line" [1 .. 6]))
    -- Overlays the frame draws itself: the panel rect rides on their spans.
    spanPanelRect _ = do
      inp <- getInput
      listToMaybe . map (\(_, _, _, _, r) -> r) <$> onContext (`collectOverlayTextSpans` inp)
    -- Floating panels, which are layout nodes.
    nodePanelRect src = onContext (`getPrevRect` respId src)

victims :: IO [Victim]
victims = do
  -- The tab bar does not keep its own state, so the test holds it.
  tabRef <- newVar (0 :: Int)
  let tabBarUi v = toResponse <$> tabBar' v [tab i (T.pack ("tab " <> show i)) () | i <- [0 .. 3 :: Int]]
      plain name ui place = Victim name ui (hotAt place) (\_ _ -> pure "")
      -- The point @fx@ across and @fy@ down the rect, moved by @dx@ and @dy@.
      pt fx dx fy dy (Rect x y w h) = V2 (x + w * fx + dx) (y + h * fy + dy)
      -- Where a press changes each control, and what else of its state to watch.
      control (name, ui) = case name of
        -- The last option.
        "radio" -> plain name ui (pt 0.5 0 1 (-6))
        "slider" -> plain name ui (pt 0.8 0 0.5 0)
        "color picker" -> plain name ui (spanCenter . colorPickerSvSquare)
        -- The up arrow.
        "numeric input" -> plain name ui (pt 1 (-8) 0.25 0)
        "text input" -> (plain name ui spanCenter) {vcProbe = caretProbe}
        "text area" -> (plain name ui spanCenter) {vcProbe = areaProbe}
        "select" -> (plain name ui spanCenter) {vcProbe = selectProbe}
        _ -> plain name ui spanCenter
  pure $
    map control everyControl
      ++ [ -- The second tab.
           plain "tab-bar" (fmap show <$> held tabRef tabBarUi) (pt 0 90 0.5 0)
         , -- On the scrollbar lane, where a press drags the thumb.
           (plain "scroll-area" (fmap show <$> scroller) (pt 1 (-5) 0.7 0)) {vcProbe = scrollProbe}
         , -- On the boundary between the two headers, where a press resizes.
           (plain "table-columns" (fmap show <$> tableUi) spanCenter) {vcHot = headerBoundary}
         ]
  where
    hotAt place _ = pure . place . respRect
    scroller = do
      (wid, ()) <- scrollArea (fillW . fixedH 140) (mapM_ label (numbered "scroll line" [1 .. 40]))
      ctx <- askContext
      rect <- liftIO (getPrevRect ctx wid)
      pure (mempty {rawRespId = wid, rawRespRect = fromMaybe (Rect 0 0 0 0) rect}, ())
    tableUi = do
      sortVar <- useVar (SortCol 0 SortAsc)
      sort <- readVar sortVar
      let rows = [(name, "value") | name <- numbered "row" [1 .. 4]]
          cols = headed "Name" fst <> headed "Value" snd
      r <- tableWith' (fixedH 120) "victim table" cols rows (constVar sort)
      pure (toResponse r, (tableSort r, tableColOrder r))
    headerBoundary ctx r = do
      spans' <- collectTextSpans ctx
      let valueX = listToMaybe [x | (Rect x _ _ _, txt, _, _, _) <- spans', "Value" `T.isPrefixOf` txt]
      pure (maybe (V2 0 0) (\x -> V2 (x - 9) (rectY (respRect r) + 12)) valueX)
    caretProbe ctx r = do
      st <- getStore ctx
      let slot s = IM.lookup (slotKey s (intKey (respId r))) (storeInt st)
      pure (show (slot SlotCursor, slot SlotAnchor))
    areaProbe ctx r = do
      st <- getStore ctx
      let s = loadTextAreaState st (intKey (respId r))
      off <- getScrollOffset ctx (respId r)
      pure (show (getCursor (tasBuffer s), tasSelectionAnchor s, off))
    scrollProbe ctx r = show <$> getScrollOffset ctx (respId r)
    selectProbe ctx r = (\st -> show (isSelectOpen st (intKey (respId r)))) <$> getStore ctx

-- | The source on top, then the widget under test pushed right and down by
-- spacers so its hot point lands under the overlay.
page :: Maybe Overlay -> Victim -> V2 -> NanoUI Page
page mOv vc (V2 dx dy) =
  columnWith (fixedW 860) $ do
    src <- maybe (label' "no overlay") ovSource mOv
    spacer (Fixed 0) (Fixed dy)
    victim <- row (spacer (Fixed dx) (Fixed 0) >> columnWith (fixedW 320) (vcUi vc))
    pure (src, victim)

-- | Run @script@ on the page with the overlay open, given the widget's hot
-- point (@offset@ into its rect) and where the overlay landed.
withPage :: IORef Int -> Maybe Overlay -> Victim -> V2 -> V2 -> (V2 -> Maybe Rect -> Sim Page r) -> IO r
withPage failed mOv vc (V2 offX offY) shift script = do
  ctx <- newPixelContext
  runSim ctx failed (withInput 900 700) (page mOv vc shift) $ do
    (src, (resp, _)) <- result
    let hot = V2 (rectX (respRect resp) + offX) (rectY (respRect resp) + offY)
    forM_ mOv (\ov -> ovOpen ov src hot)
    (src', _) <- moveTo (At hot)
    script hot =<< maybe (pure (Just (Rect 0 0 900 700))) (`ovRect` src') mOv

runPointerOwnershipTest :: Context -> IORef Int -> IO ()
runPointerOwnershipTest _ failed = do
  vcs <- victims
  let report name msgs = forM_ msgs $ \msg -> putStrLn ("  " <> name <> ": " <> msg) >> bump failed
  forM_ vcs $ \vc -> do
    -- Found with nothing on top, which could hide the spans some widgets need.
    offset <- withPage failed Nothing vc (V2 0 0) (V2 0 0) $ \_ _ -> do
      (_, (resp, _)) <- result
      V2 hx hy <- onContext (\ctx -> vcHot vc ctx resp)
      pure (V2 (hx - rectX (respRect resp)) (hy - rectY (respRect resp)))
    -- The control: with no overlay the widget does notice.
    noticed <- scenario failed Nothing vc offset (V2 0 0) ClickDrag
    when (null noticed) $
      report (vcName vc) ["ignores a click and drag on its hot point, so it tests nothing"]
    forM_ overlays $ \ov -> do
      -- An unshifted run says how far to push the hot point under the overlay.
      shift <- withPage failed (Just ov) vc offset (V2 0 0) $ \(V2 hx hy) mRect ->
        pure (maybe (V2 0 0) (\(Rect ox oy _ _) -> V2 (max 0 (ox + 40 - hx)) (max 0 (oy + 60 - hy))) mRect)
      forM_ [ClickDrag, Wheel, RightClick] $ \gesture ->
        report (ovName ov <> " over " <> vcName vc <> " (" <> show gesture <> ")")
          =<< scenario failed (Just ov) vc offset shift gesture

-- | Everything the widget noticed while the gesture ran on the overlay, or on
-- the widget itself when there is none.
scenario :: IORef Int -> Maybe Overlay -> Victim -> V2 -> V2 -> Gesture -> IO [String]
scenario failed mOv vc offset shift gesture = withPage failed mOv vc offset shift $ \hot@(V2 hx hy) -> \case
  Nothing -> pure ["overlay did not open"]
  Just r | not (rectContains r hot) -> pure ["hot " <> show hot <> " is not under the overlay at " <> show r]
  Just _ -> do
    (_, (resp0, value0)) <- frame
    probe0 <- onContext (\ctx -> vcProbe vc ctx resp0)
    focus0 <- focusedId
    let victimId = respId resp0
        -- Each step, and whether its flags count (hover once the gesture ends is fair).
        steps = case gesture of
          ClickDrag ->
            [ (True, press (At hot))
            , (True, moveTo (At (V2 (hx + 14) (hy - 9))))
            , (True, moveTo (At (V2 (hx + 28) (hy - 18))))
            , (True, release)
            , (False, frame)
            ]
          Wheel -> [(True, frameWith (\i -> i {inputScroll = V2 0 s})) | s <- [3, -3, 3]]
          RightClick ->
            [(True, pressButton MouseRight (At hot)), (True, releaseButton MouseRight), (False, frame)]
    fmap concat . forM (zip [1 :: Int ..] steps) $ \(i, (flags, step)) -> do
      (_, (resp, value)) <- step
      probe <- onContext (\ctx -> vcProbe vc ctx resp)
      focus <- focusedId
      active <- onContext (readIORef . ctxActiveId)
      menu <- onContext (`getsInteraction` isTextInputMenu)
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

-- | The frame's unrouted input is for routing code and what watches the whole
-- window; a widget reading presses from it reacts through whatever is on top.
-- The scan starts at the imports, so a re-export passes.
runPointerRoutingLintTest :: Context -> IORef Int -> IO ()
runPointerRoutingLintTest _ failed = do
  root : _ <- filterM doesDirectoryExist ["lib", "packages/nano-ui/lib"]
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
      , "NanoUI/Internal/Widgets/Popup.hs" -- tooltips: a press anywhere shuts one; one over its target stays
      ]
    complain msg = putStrLn ("  " <> msg) >> bump failed
    haskellFiles dir = do
      entries <- map (dir </>) <$> listDirectory dir
      dirs <- filterM doesDirectoryExist entries
      (filter ((== ".hs") . takeExtension) entries <>) . concat <$> mapM haskellFiles dirs

captureUi :: NanoUI ((Response, Float), Maybe Response)
captureUi = do
  s <- columnWith (fixedW 320) (withValue 50 (slider' 0 100))
  (_, b) <- window' (constVar True) "Capture window" (columnWith (fixedWH 300 200) (button' "inside"))
  pure (s, b)

-- | A held button stays with the layer it went down in, page or window, and
-- wakes nothing in the other; a press is routed afresh even with another
-- button still down.
pointerCapture :: Sim ((Response, Float), Maybe Response) ()
pointerCapture = do
  ((slider0, _), Just inside0) <- result
  let Rect sx sy sw sh = respRect slider0
      Rect bx by _ bh = respRect inside0
      onTrack = At (V2 (sx + sw * 0.25) (sy + sh / 2))
      onButton = On inside0
  -- The scenario needs the window clear of the slider.
  check (v2X (centerOf inside0) > sx + sw)
  ((_, dragged), overWindow) <- press onTrack >> moveTo onButton
  checkEq 100 dragged
  check (maybe False (not . respHovered) overWindow)
  ((_, rested), free) <- release >> moveTo onButton
  check (maybe False respHovered free)
  ((overPage, crossed), _) <- press (At (V2 (bx + 4) (by + bh + 40))) >> moveTo onTrack
  checkEq rested crossed
  check (not (respHovered overPage))
  ((freed, _), _) <- release >> frame
  check (respHovered freed)
  -- With the right button held on the page, a left click in the window lands.
  (_, chorded) <- pressButton MouseRight onTrack >> click onButton
  check (maybe False respClicked chorded)
