{-# OPTIONS_GHC -O0 #-}
module Main (main) where

import BackendApi (backendApiCheck)
import Cases.Adornment qualified
import Cases.Animation qualified
import Cases.ApiSmoke qualified
import Cases.Atlas qualified
import Cases.Cache qualified
import Cases.Combo qualified
import Cases.ContextMenu qualified
import Cases.Cursors qualified
import Cases.CustomWidget qualified
import Cases.Damage qualified
import Cases.DamageProps qualified
import Cases.Demo qualified
import Cases.Explain qualified
import Cases.Focus qualified
import Cases.Grid qualified
import Cases.HostDraw qualified
import Cases.Images qualified
import Cases.Ime qualified
import Cases.Keyboard qualified
import Cases.Labels qualified
import Cases.LayoutFlow qualified
import Cases.LayoutProps qualified
import Cases.LayoutReuse qualified
import Cases.Modal qualified
import Cases.NativeWindow qualified
import Cases.NoThunks qualified
import Cases.NumericInput qualified
import Cases.PaneGrid qualified
import Cases.Passes qualified
import Cases.Paths qualified
import Cases.PointerExtra qualified
import Cases.PointerOwnership qualified
import Cases.PointerRelease qualified
import Cases.Reorder qualified
import Cases.RichText qualified
import Cases.Runner qualified
import Cases.SIMD qualified
import Cases.Scroll qualified
import Cases.Select qualified
import Cases.Shaping qualified
import Cases.Shortcut qualified
import Cases.SimFuzz qualified
import Cases.State qualified
import Cases.Styling qualified
import Cases.Svg qualified
import Cases.Table qualified
import Cases.Tabs qualified
import Cases.Tasks qualified
import Cases.TextInput qualified
import Cases.Theming qualified
import Cases.Tooltip qualified
import Cases.TooltipDelay qualified
import Cases.ViewApi qualified
import Cases.Visibility qualified
import Cases.WidgetIdIndex qualified
import Cases.Window qualified
import NanoUI.Testing.Runner (runTests)

main :: IO ()
main = do
  backendApiCheck
  runTests . concat $
    [ Cases.Adornment.tests
    , Cases.Animation.tests
    , Cases.ApiSmoke.tests
    , Cases.Atlas.tests
    , Cases.Cache.tests
    , Cases.Combo.tests
    , Cases.ContextMenu.tests
    , Cases.Cursors.tests
    , Cases.CustomWidget.tests
    , Cases.Damage.tests
    , Cases.DamageProps.tests
    , Cases.Demo.tests
    , Cases.Explain.tests
    , Cases.Focus.tests
    , Cases.Grid.tests
    , Cases.HostDraw.tests
    , Cases.Images.tests
    , Cases.Ime.tests
    , Cases.Keyboard.tests
    , Cases.Labels.tests
    , Cases.LayoutFlow.tests
    , Cases.LayoutProps.tests
    , Cases.LayoutReuse.tests
    , Cases.Modal.tests
    , Cases.NativeWindow.tests
    , Cases.NoThunks.tests
    , Cases.NumericInput.tests
    , Cases.PaneGrid.tests
    , Cases.Passes.tests
    , Cases.Paths.tests
    , Cases.PointerExtra.tests
    , Cases.PointerOwnership.tests
    , Cases.PointerRelease.tests
    , Cases.Reorder.tests
    , Cases.RichText.tests
    , Cases.Runner.tests
    , Cases.SIMD.tests
    , Cases.Scroll.tests
    , Cases.Select.tests
    , Cases.Shaping.tests
    , Cases.Shortcut.tests
    , Cases.SimFuzz.tests
    , Cases.State.tests
    , Cases.Styling.tests
    , Cases.Svg.tests
    , Cases.Table.tests
    , Cases.Tabs.tests
    , Cases.Tasks.tests
    , Cases.TextInput.tests
    , Cases.Theming.tests
    , Cases.Tooltip.tests
    , Cases.TooltipDelay.tests
    , Cases.ViewApi.tests
    , Cases.Visibility.tests
    , Cases.WidgetIdIndex.tests
    , Cases.Window.tests
    ]
