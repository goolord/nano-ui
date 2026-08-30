{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Layout
  ( panel
  , row
  , column
  , label
  , labelEx
  , separator
  , spacer
  , scroll
  , scrollArea
  , flex
  , sep
  )
where

import Control.Monad (void)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..))
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeType (..)
  , addNodeFromLayout
  , getDirection
  , setWidgetId
  )
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiFinally, uiIO)
import NanoUI.Style
  ( Direction (..)
  , Layout (..)
  , Sizing (..)
  , defaultLayout
  )
import NanoUI.Widgets.Node
  ( Response
  , addSizingLeafNode
  , addWidget
  , container
  , parentIdx
  )

{-# INLINE panel #-}
panel :: Ui :> es => Layout -> Eff es a -> Eff es a
panel = container NodePanel

{-# INLINE row #-}
row :: Ui :> es => Layout -> Eff es a -> Eff es a
row layout child = container NodeContainer (layout {layoutDirection = Row}) child

{-# INLINE column #-}
column :: Ui :> es => Layout -> Eff es a -> Eff es a
column layout child = container NodeContainer (layout {layoutDirection = Column}) child

{-# INLINE label #-}
label :: Ui :> es => Text -> Eff es Response
label = labelEx defaultLayout

{-# INLINE labelEx #-}
labelEx :: Ui :> es => Layout -> Text -> Eff es Response
labelEx layout txt = do
  wid <- nextId
  addWidget wid NodeText txt 0 layout

{-# INLINE sep #-}
sep :: Ui :> es => Eff es ()
sep = void separator

{-# INLINE flex #-}
flex :: Ui :> es => Eff es ()
flex = void (spacer (Grow 1) Fit)

{-# INLINE separator #-}
separator :: Ui :> es => Eff es Response
separator = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let
      parent = parentIdx stack
    parentDir <-
      if parent < 0
        then pure DirColumn
        else getDirection (ctxNodeArena ctx) parent
    let
      (dir, wSiz, hSiz) =
        case parentDir of
          DirColumn -> (Column, Grow 1, Fixed 1)
          DirRow -> (Row, Fixed 1, Grow 1)
    addSizingLeafNode ctx inp wid NodeSeparator dir wSiz hSiz

{-# INLINE spacer #-}
spacer :: Ui :> es => Sizing -> Sizing -> Eff es Response
spacer w h = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ addSizingLeafNode ctx inp wid NodeSpacer Row w h

{-# INLINE scroll #-}
scroll :: Ui :> es => Layout -> Eff es a -> Eff es a
scroll layout child = do
  (_, r) <- scrollArea layout child
  pure r

{-# INLINE scrollArea #-}
scrollArea :: Ui :> es => Layout -> Eff es a -> Eff es (WidgetId, a)
scrollArea layout child = do
  ctx <- askContext
  wid <- nextId
  stack <- uiIO $ do
    stack0 <- readIORef (ctxContainerStack ctx)
    let
      parent = parentIdx stack0
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeScrollContainer parent layout
    setWidgetId (ctxNodeArena ctx) idx wid
    writeIORef (ctxContainerStack ctx) (idx : stack0)
    pure stack0
  childR <- uiFinally child (writeIORef (ctxContainerStack ctx) stack)
  pure (wid, childR)
