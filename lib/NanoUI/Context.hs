{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , Animation (..)
  , WidgetStore (..)
  , bumpMirror
  , slotKey
  , slotDisabled
  , slotCursor
  , slotAnchor
  , slotDrag
  , slotDragW
  , slotWinSize
  , boolInt
  , intBool
  , pairList
  , listPair
  , anySelectOpen
  , isSelectOpen
  , setSelectOpen
  , closeSelects
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
  , getFocusablesPrim
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
  , pointerBlockedByOverlay
  , seedFloatingPanel
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
import NanoUI.Store
  ( WidgetStore (..)
  , anySelectOpen
  , boolInt
  , bumpMirror
  , closeSelects
  , intBool
  , isSelectOpen
  , listPair
  , pairList
  , setSelectOpen
  , slotAnchor
  , slotCursor
  , slotDisabled
  , slotDrag
  , slotDragW
  , slotKey
  , slotWinSize
  )
import NanoUI.Spring
  ( SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  )
import NanoUI.Context.Internal (Context (..), intKey, markDirty)
import NanoUI.Context.Types
  ( PendingTooltip (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
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
  , withClipboard
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
  , pointerBlockedByOverlay
  , seedFloatingPanel
  , textInputEditActive
  )
import NanoUI.Context.Host (askHostIO, setHost)
import NanoUI.Context.Focus (getFocusId, getFocusables, getFocusablesPrim, getHotId, registerFocusable)
import NanoUI.Context.Messages (drainMessages, pushMessage)
