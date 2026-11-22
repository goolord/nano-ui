{-# OPTIONS_GHC -O0 #-}
-- | Every widget and container the "NanoUI" facade exports that no other
-- test calls, run once in a view: it settles without an exception and lays
-- out a node of its own, or, for a tooltip, hovering its target asks for
-- the wake that opens it.
--
-- The list was the facade's lowercase exports that no test under
-- @packages/*/test@ names. The rest of that list are not widgets: layout
-- and style modifiers ('alignStart', the border, font and padding
-- setters), colours and themes, input predicates ('inputMouseDown',
-- 'keyActivated'), queries ('uiTime', 'uiFontSize'), scroll commands
-- ('scrollToUi'), 'loadSvg' (a file read) and the clipboard calls, which
-- need a backend's clipboard.
module Cases.ApiSmoke (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (arenaCount)
import NanoUI.Testing.Sim qualified as Sim

tests :: [Spec]
tests =
  [spec "api-smoke" runApiSmokeTest]

data Fruit = Apple | Pear | Plum
  deriving (Eq, Show, Bounded, Enum)

-- | Each widget, named, and whether it is a tooltip on the button "target".
widgets :: IO [(String, Bool, NanoUI ())]
widgets = do
  fruit <- newVar Pear
  flag <- newVar False
  sortCol <- newVar (SortCol 0 SortAsc)
  doc <- newVar (textDocument "one\ntwo")
  icon <- either fail pure (parseSvg "<svg viewBox='0 0 24 24'><rect x='2' y='2' width='20' height='20'/></svg>")
  let show' = T.pack . show
      target = button' "target"
      cols = headed "Name" fst <> headed "Value" snd
  pure
    [ ("boundedSelect", False, boundedSelect show' fruit)
    , ("boundedSelect'", False, void (boundedSelect' show' fruit))
    , ("boundedTabBar", False, boundedTabBar show' fruit)
    , ("boundedTabBarConfigured", False, boundedTabBarConfigured defaultTabsConfig show' fruit)
    , ("boundedTabs", False, boundedTabs show' fruit (label . show'))
    , ("boundedTabsConfigured", False, boundedTabsConfigured defaultTabsConfig show' fruit (label . show'))
    , ("buttonContentWith", False, void (buttonContentWith id (label "content")))
    , ("buttonContentWith'", False, void (buttonContentWith' id (label "content")))
    , ("callout", False, callout (colorRGBA 200 40 40 255) (label "note"))
    , ("calloutWith", False, calloutWith (colorRGBA 200 40 40 255) fillW (label "note"))
    , ("captionButton", False, void (captionButton defaultCaptionConfig GlyphClose))
    , ("captionButtonsConfigured", False, void (captionButtonsConfigured defaultCaptionConfig False))
    , ("checkboxWith'", False, void (checkboxWith' fillW "check" flag))
    , ("drawingCached", False, void (drawingCached 40 20 12 0 id (pure (fixedWH 40 20 defaultLayout)) (\r -> pure (FillRect r (colorRGBA 0 0 255 255)))))
    , ("dropZone", False, void (dropZone (fixedWH 80 40) (label "drop")))
    , ("enumRadio", False, enumRadio fruit)
    , ("enumRadio'", False, void (enumRadio' fruit))
    , ("enumSelect", False, enumSelect fruit)
    , ("enumSelect'", False, void (enumSelect' fruit))
    , ("enumTabBar", False, enumTabBar fruit)
    , ("enumTabs", False, enumTabs fruit (label . show'))
    , ("hstack", False, hstack [label "a", label "b"])
    , ("vstack", False, vstack [label "a", label "b"])
    , ("kvBlock", False, kvBlock [("key", "value"), ("longer key", "v")])
    , ("menuButtonWith", False, void (menuButtonWith id "File" False))
    , ("menuButtonWith'", False, void (menuButtonWith' id "File" True))
    , ("rowWith'", False, void (rowWith' fillW (label "in a row")))
    , ("scroll2D", False, scroll2D (label "scrolled"))
    , ("selectableText'", False, void (selectableText' "selectable"))
    , ("selectableTextWith'", False, void (selectableTextWith' fillW "selectable"))
    , ("svgIconConfigured'", False, void (svgIconConfigured' defaultImageConfig {icLayout = fixedWH 24 24} icon))
    , ("svgIconWith'", False, void (svgIconWith' (fixedWH 24 24) icon))
    , ("table'", False, void (table' "t" cols [("a" :: T.Text, "1" :: T.Text), ("b", "2")] sortCol))
    , ("textAreaDocumentWith", False, textAreaDocumentWith (fixedWH 200 80) doc)
    , ("textAreaDocumentWith'", False, void (textAreaDocumentWith' (fixedWH 200 80) doc))
    , ("toggleButtonWith", False, toggleButtonWith id Accent "toggle" flag)
    , ("toggleButtonWith'", False, void (toggleButtonWith' id Success "toggle" flag))
    , ("tooltipAt", True, target >>= \t -> tooltipAt PlacementBelow t "tip")
    , ("tooltipWidget", True, target >>= \t -> void (tooltipWidget t (label "tip")))
    , ("windowFrame", False, windowFrame (defaultWindowFrame (colorRGBA 90 90 90 255)) (label "framed"))
    ]

-- | Each widget alone in a column lays out more nodes than the empty
-- column does. A tooltip opens after a delay on the monotonic clock, so for
-- the tooltips the check is that hovering the target asks for a timed wake
-- to open it.
runApiSmokeTest :: Context -> IORef Int -> IO ()
runApiSmokeTest _ failed = do
  ws <- widgets
  bare <- newContext
  Sim.runSim bare failed (withInput 400 300) (column (pure ())) (pure ())
  empty <- arenaCount (ctxNodeArena bare)
  forM_ ws $ \(name, hover, widget) -> do
    ctx <- newContext
    Sim.runSim ctx failed (withInput 400 300) (column widget) $
      if hover
        then do
          _ <- Sim.moveTo "target"
          Sim.checkEq (name, True) . (name,) . (> 0) =<< Sim.wakeTime
        else do
          nodes <- Sim.onContext (arenaCount . ctxNodeArena)
          Sim.checkEq (name, True) (name, nodes > empty)
