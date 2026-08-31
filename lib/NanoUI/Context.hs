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

import NanoUI.Animation
  ( Animation (..)
  , Ease (..)
  , animInProgress
  , applyEase
  , approxEq
  , easeSameSpec
  )
import NanoUI.Context.Internal
  ( Context (..)
  , PendingTooltip (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , anyAnimating
  , askHostIO
  , atlasSnapshot
  , atlasTextureId
  , beginModal
  , clearDirty
  , clearMeasureCache
  , clearTooltips
  , drainMessages
  , enableMeasureCache
  , endModal
  , getAnimationValue
  , getFocusId
  , getFocusables
  , getFocusablesPrim
  , getHotId
  , getPrevRect
  , getPrevRectByKey
  , getScrollOffset
  , getStore
  , intKey
  , isDisabled
  , isDirty
  , lookupImageUv
  , markDirty
  , markEscapeConsumed
  , modalActive
  , newContext
  , overlayConsumesQuit
  , pointerBlockedByModal
  , pointerBlockedByOverlay
  , pushMessage
  , pushTooltip
  , readTooltips
  , registerFocusable
  , registerImage
  , registerImages
  , seedFloatingPanel
  , setAnimationValue
  , setDisabled
  , setHost
  , setPrevRect
  , setScrollOffset
  , setStore
  , setWakeLoop
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , takeDamage
  , textInputEditActive
  , tickAnimations
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
import NanoUI.Messages (FrameMsg (..), decodeMessages, reduceMessages, reduceUpdates)
import NanoUI.Spring
  ( SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
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
