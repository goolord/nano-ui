module Main (main) where

import Cases.Runner qualified
import Cases.SIMD qualified
import Cases.NoThunks qualified
import Cases.State qualified
import Cases qualified
import Cases.Window qualified
import Cases.Cache qualified
import Cases.Grid qualified
import Cases.Caption qualified
import Cases.ViewApi qualified
import Cases.HostDraw qualified
import Cases.RichText qualified
import Cases.Shaping qualified
import Cases.Svg qualified
import Cases.Adornment qualified
import Cases.Atlas qualified
import Cases.Damage qualified
import Cases.Tabs qualified
import Cases.TextInput qualified
import Cases.Animation qualified
import Cases.Keyboard qualified
import Cases.Styling qualified
import Cases.Select qualified
import Cases.Combo qualified
import Cases.Demo qualified
import Cases.NumericInput qualified
import Cases.Scroll qualified
import Cases.Table qualified
import Cases.Modal qualified
import Cases.ContextMenu qualified
import Cases.PointerRelease qualified
import Cases.PointerOwnership qualified
import Cases.Tooltip qualified
import Cases.CustomWidget qualified
import Cases.WidgetIdIndex qualified
import Cases.Paths qualified
import Cases.LayoutFlow qualified
import Cases.Images qualified
import Cases.TooltipDelay qualified
import Cases.Visibility qualified
import Cases.PointerExtra qualified
import NanoUI.Testing.Runner (runTests)

main :: IO ()
main =
  runTests . concat $
    [ Cases.Runner.tests
    , Cases.SIMD.tests
    , Cases.NoThunks.tests
    , Cases.State.tests
    , Cases.tests
    , Cases.Window.tests
    , Cases.Cache.tests
    , Cases.Grid.tests
    , Cases.Caption.tests
    , Cases.ViewApi.tests
    , Cases.HostDraw.tests
    , Cases.RichText.tests
    , Cases.Shaping.tests
    , Cases.Svg.tests
    , Cases.Adornment.tests
    , Cases.Atlas.tests
    , Cases.Damage.tests
    , Cases.Tabs.tests
    , Cases.TextInput.tests
    , Cases.Animation.tests
    , Cases.Keyboard.tests
    , Cases.Styling.tests
    , Cases.Select.tests
    , Cases.Combo.tests
    , Cases.Demo.tests
    , Cases.NumericInput.tests
    , Cases.Scroll.tests
    , Cases.Table.tests
    , Cases.Modal.tests
    , Cases.ContextMenu.tests
    , Cases.PointerRelease.tests
    , Cases.PointerOwnership.tests
    , Cases.Tooltip.tests
    , Cases.CustomWidget.tests
    , Cases.WidgetIdIndex.tests
    , Cases.Paths.tests
    , Cases.LayoutFlow.tests
    , Cases.Images.tests
    , Cases.TooltipDelay.tests
    , Cases.Visibility.tests
    , Cases.PointerExtra.tests
    ]
