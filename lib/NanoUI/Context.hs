{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , Animation (..)
  , WidgetStore (..)
  , newContext
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , wrapMeasureCache
  , clearMeasureCache
  , withExternalText
  , withTheme
  , withIcons
  , withHostProfile
  , enableMeasureCache
  , markDirty
  , clearDirty
  , isDirty
  , setWakeLoop
  , takeDamage
  , getHotId
  , getFocusId
  , anyAnimating
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , setAnimationValue
  , tickAnimations
  , easeSameSpec
  , getPrevRect
  , getPrevRectByKey
  , setPrevRect
  , getStore
  , setStore
  , intKey
  , pushMessage
  , drainMessages
  , decodeMessages
  , reduceMessages
  , reduceUpdates
  , registerFocusable
  , getFocusables
  , isDisabled
  , setDisabled
  , getScrollOffset
  , setScrollOffset
  , getAnimationValue
  , applyEase
  , animInProgress
  , approxEq
  , Ease (..)
  , SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  , clearTooltips
  , pushTooltip
  , readTooltips
  , PendingTooltip (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , withClipboard
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , beginModal
  , endModal
  , registerImage
  , registerImages
  , lookupImageUv
  , atlasSnapshot
  , atlasTextureId
  , setHost
  , askHostIO
  ) where

import NanoUI.Messages (FrameMsg (..), decodeMessages, reduceMessages, reduceUpdates)
import NanoUI.Animation
  ( Animation (..)
  , Ease (..)
  , animInProgress
  , applyEase
  , approxEq
  , easeSameSpec
  )
import NanoUI.Store (WidgetStore (..))
import NanoUI.Spring
  ( SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  )
import NanoUI.Context.Internal
  ( Context (..)
  , PendingTooltip (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , intKey
  , markDirty
  )
import NanoUI.Context.New (newContext)
import NanoUI.Context.PrevRects
  ( getPrevRect
  , getPrevRectByKey
  , setPrevRect
  )
import NanoUI.Context.Store
  ( getScrollOffset
  , getStore
  , isDisabled
  , setDisabled
  , setScrollOffset
  , setStore
  )
import NanoUI.Context.Config
  ( clearMeasureCache
  , enableMeasureCache
  , withExternalText
  , withFontMetrics
  , withHostProfile
  , withIcons
  , withMeasureText
  , withMonoFontMetrics
  , withTheme
  , wrapMeasureCache
  )
import NanoUI.Context.Dirty
  ( clearDirty
  , isDirty
  , setWakeLoop
  , takeDamage
  )
import NanoUI.Context.Tooltip
  ( clearTooltips
  , pushTooltip
  , readTooltips
  )
import NanoUI.Context.Atlas
  ( atlasSnapshot
  , atlasTextureId
  , lookupImageUv
  , registerImage
  , registerImages
  )
import NanoUI.Context.Animation
  ( anyAnimating
  , getAnimationValue
  , setAnimationValue
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , tickAnimations
  )
import NanoUI.Context.Modal
  ( beginModal
  , endModal
  , markEscapeConsumed
  , modalActive
  , overlayConsumesQuit
  , pointerBlockedByModal
  , textInputEditActive
  )
import NanoUI.Context.Host (askHostIO, setHost)
import NanoUI.Context.Focus (getFocusId, getHotId)
import NanoUI.Context.Messages (drainMessages, pushMessage)
import NanoUI.Context.Focusables (getFocusables, registerFocusable)
import NanoUI.Context.Clipboard (withClipboard)
