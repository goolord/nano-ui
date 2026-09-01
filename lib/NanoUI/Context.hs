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
  , DamageRequest (..)
  , requestDamage
  , damageWidget
  , damageKey
  , damageRect
  , damagePeers
  , damageFull
  , clearDamageRequests
  , getDamageRequests
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
  , getPrevClipRect
  , getPrevClipRectByKey
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
  , getScrollOffset2D
  , getScrollConfig
  , getScrollContentExtent
  , setScrollOffset
  , setScrollOffset2D
  , setScrollConfig
  , setScrollContentExtent
  , linkScrollAxes
  , getAnimationValue
  , applyEase
  , animInProgress
  , approxEq
  , Ease (..)
  , SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  , registerPopupConfig
  , lookupPopupConfig
  , clearPopupConfigs
  , TextInputMenu (..)
  , TextInputDrag (..)
  , TextFieldClickCell (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , withClipboard
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , pointerBlockedByOverlay
  , menuPointerGestureActive
  , armMenuPointerCapture
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
  , TextInputDrag (..)
  , TextFieldClickCell (..)
  , TextInputMenu (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , anyAnimating
  , armMenuPointerCapture
  , askHostIO
  , atlasSnapshot
  , atlasTextureId
  , beginModal
  , clearDirty
  , clearDamageRequests
  , clearMeasureCache
  , clearPopupConfigs
  , DamageRequest (..)
  , damageFull
  , damageKey
  , damagePeers
  , damageRect
  , damageWidget
  , drainMessages
  , enableMeasureCache
  , endModal
  , getAnimationValue
  , getDamageRequests
  , getFocusId
  , getFocusables
  , getFocusablesPrim
  , getHotId
  , getPrevRect
  , getPrevRectByKey
  , getPrevClipRect
  , getPrevClipRectByKey
  , getScrollOffset
  , getScrollOffset2D
  , getScrollConfig
  , getScrollContentExtent
  , getStore
  , intKey
  , isDisabled
  , isDirty
  , lookupImageUv
  , lookupPopupConfig
  , markDirty
  , markEscapeConsumed
  , menuPointerGestureActive
  , modalActive
  , newContext
  , overlayConsumesQuit
  , pointerBlockedByModal
  , pointerBlockedByOverlay
  , pushMessage
  , registerFocusable
  , registerImage
  , registerImages
  , registerPopupConfig
  , requestDamage
  , seedFloatingPanel
  , setAnimationValue
  , setDisabled
  , setHost
  , setPrevRect
  , setScrollOffset
  , setScrollOffset2D
  , setScrollConfig
  , setScrollContentExtent
  , linkScrollAxes
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
