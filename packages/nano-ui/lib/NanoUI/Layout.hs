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
  , wrap
  , tight
  , percent
  , aspect
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
  , panel
  , panel_
  , panelWith
  , panel'
  , panelResponse
  , scroll
  , scroll_
  , scrollWith
  , scrollArea
  , scrollArea2D
  , scrollConfigured
  , scroll2D
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
  , aspect
  , defaultLayout
  , fillH
  , fillW
  , fixedH
  , fixedW
  , fixedWH
  , fontHeading
  , fontMono
  , fontMuted
  , fontRegular
  , gap
  , grow
  , minW
  , padAll
  , padXY
  , percent
  , tight
  , wrap
  )
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
  , hGroup
  , panel
  , panel'
  , panelResponse
  , panelWith
  , panel_
  , row
  , row'
  , rowResponse
  , rowWith
  , row_
  , scroll
  , scroll2D
  , scrollArea
  , scrollArea2D
  , scrollConfigured
  , scrollWith
  , scroll_
  , sep
  , separator
  , spacer
  , vGroup
  )
