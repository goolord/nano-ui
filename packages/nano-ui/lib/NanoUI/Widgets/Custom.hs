-- | Custom widgets and the reference widgets built on them.
--
-- 'customWidget' takes a 'CustomWidgetSpec': a layout, optional measurement,
-- drawing that sees hover and press state, an optional content key, a cursor,
-- and damage slop.
-- 'canvas' is the short form for drawing into a laid-out rectangle with
-- 'CanvasM'. 'useDrag2D' and 'useWheelDelta' are gesture hooks for your own
-- controls; 'knob' and 'toggleSwitch' show how they fit together.
module NanoUI.Widgets.Custom
  ( -- * Custom widgets
    CustomWidgetSpec (..)
  , defaultCustomWidgetSpec
  , customWidget
  , customWidgetWithId
  , contentKey
  , contentKeyOf
  , KeyPart
  , keyPart
  , CustomDrawContext (..)
  , CustomMeasureFn
  , CustomDrawBuild
    -- * Canvas
  , CanvasM
  , runCanvas
  , canvas
  , drawRect
  , drawRoundedRect
  , drawCircle
  , drawStroke
  , drawStrokeRoundedRect
  , drawStrokeCircle
  , drawStrokeAA
  , drawQuadGradient
  , drawLinearGradientH
  , drawLinearGradientV
  , drawImage
  , drawImageUV
  , drawText
    -- * Gestures
  , useDrag2D
  , Drag2D (..)
  , useWheelDelta
    -- * Reference widgets
  , knob
  , knob'
  , knobWith
  , knobWith'
  , toggleSwitch
  , toggleSwitch'
  , toggleSwitchWith
  , toggleSwitchWith'
  , circularProgress
  , circularProgress'
  , circularProgressWith
  , circularProgressWith'
  , spinner
  , spinner'
  , spinnerWith
  , spinnerWith'
  , progressBar
  , progressBar'
  , progressBarWith
  , progressBarWith'
  , sparkline
  , sparkline'
  , sparklineWith
  , sparklineWith'
  ) where

import NanoUI.Internal.Widgets.Custom
