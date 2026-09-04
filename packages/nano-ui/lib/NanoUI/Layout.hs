{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Layout
  ( -- * Sizing & Alignments
    Sizing (..)
  , Direction (..)
  , AlignX (..)
  , AlignY (..)
  , Padding (..)
  , Layout (..)
  , defaultLayout
  , LayoutModifier
  , askDefaultLayout
  , withDefaultLayout
  , withLayout
    -- * Layout Modifiers
  , padAll
  , padXY
  , gap
  , fillW
  , fillH
  , grow
  , minW
  , fixedW
  , fixedH
  , fixedWH
  , alignMid
  , alignEnd
  , alignStart
  , alignCenter
  , alignTop
  , alignBottom
  , tight
  , percent
  , gridMinColW
  , fixedAspectW
  , fixedAspectH
  , FontVariant (..)
  , fontRegular
  , fontHeading
  , fontMuted
  , fontMono
    -- * Containers
  , row
  , row_
  , rowWith
  , row'
  , rowResponse
  , column
  , column_
  , columnWith
  , column'
  , columnResponse
  , gridAutoFit
  , gridAutoFit_
  , gridAutoFitWith
  , gridAutoFit'
  , gridAutoFitResponse
  , responsive
  , responsiveRowCol
  , windowAspect
  , panel
  , panel_
  , panelWith
  , panel'
  , panelResponse
  , scroll
  , scroll_
  , scrollWith
  , scroll'
  , scrollArea
  , scrollArea2D
  , scrollConfigured
  , scroll2D
  , scroll2D_
  , scroll2DWith
  , scroll2D'
    -- * Spacers & Separators
  , separator
  , sep
  , spacer
  , flex
    -- * Presets
  , center
  , flexRow
  , flexCol
  , hGroup
  , vGroup
  ) where

import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , FontVariant (..)
  , Layout (..)
  , LayoutModifier
  , Padding (..)
  , Sizing (..)
  , alignBottom
  , alignCenter
  , alignEnd
  , alignMid
  , alignStart
  , alignTop
  , defaultLayout
  , fillH
  , fillW
  , fixedAspectH
  , fixedAspectW
  , fixedH
  , fixedW
  , fixedWH
  , fontHeading
  , fontMono
  , fontMuted
  , fontRegular
  , gap
  , gridMinColW
  , grow
  , minW
  , padAll
  , padXY
  , percent
  , tight
  )
import NanoUI.Monad (askDefaultLayout, withDefaultLayout, withLayout)
import NanoUI.Widgets.Layout
  ( center
  , column
  , column'
  , columnResponse
  , columnWith
  , column_
  , flex
  , flexCol
  , flexRow
  , gridAutoFit
  , gridAutoFit'
  , gridAutoFitResponse
  , gridAutoFitWith
  , gridAutoFit_
  , hGroup
  , panel
  , panel'
  , panelResponse
  , panelWith
  , panel_
  , responsive
  , responsiveRowCol
  , row
  , row'
  , rowResponse
  , rowWith
  , row_
  , scroll
  , scroll'
  , scroll_
  , scrollWith
  , scroll2D
  , scroll2D'
  , scroll2D_
  , scroll2DWith
  , scrollArea
  , scrollArea2D
  , scrollConfigured
  , sep
  , separator
  , spacer
  , vGroup
  , windowAspect
  )
