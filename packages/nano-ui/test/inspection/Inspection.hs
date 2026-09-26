{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time optimization invariants for nano-ui's hot-path coding
-- patterns, checked by the inspection-testing plugin after the optimizer
-- runs. The plugin can only inspect bindings defined in this module, so
-- probes exercise the actual inline SIMD writers. If GHC stops optimizing
-- them, the build fails.
module Main
  ( main
  , solidQuadProbe
  , gradientQuadProbe
  , cornerQuadProbe
  , storeWriteProbe
  , storeWriteByHand
  , storeReadProbe
  , storeReadByHand
  , channelProbe
  , channelByHand
  , commandReadProbe
  , commandWriteProbe
  , canvasProbe
  , canvasByHand
  )
where

import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Functor.Identity (Identity (..))
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Word (Word32, Word8)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.Exts (RealWorld)
import Foreign.Ptr (Ptr)

import Test.Inspection

import NanoUI.Internal.SIMD qualified as SIMD
import NanoUI.Internal.Store
import NanoUI
  ( Animatable (..)
  , Color
  , CustomDrawContext
  , Rect (..)
  , V2 (..)
  , drawCircle
  , drawRect
  , runCanvasFor
  )
import NanoUI.Testing (DrawCmd (..), DrawOp (..), Layer (..))

main :: IO ()
main = putStrLn "inspection invariants hold"

-- Inspect actual library calls, not copies of the quad writers. Dynamic
-- offsets, coordinates, colors and indices prevent a constant-only probe from
-- hiding boxing or a failure to inline the shared vertex writer.
solidQuadProbe :: Ptr Word8 -> Ptr Word8 -> Int -> Float -> Word32 -> IO ()
solidQuadProbe vp ip offset x base =
  SIMD.pokeQuadSIMD vp offset ip offset x x x x 0 0 1 1 x x x 1 base

gradientQuadProbe :: Ptr Word8 -> Ptr Word8 -> Int -> Float -> Word32 -> IO ()
gradientQuadProbe vp ip offset x base =
  SIMD.pokeQuadGradientSIMD vp offset ip offset x x x x 0 0
    (x, 0, 0, 1) (0, x, 0, 1) (0, 0, x, 1) (x, x, x, 1) base

inspect $ 'solidQuadProbe `doesNotUse` 'SIMD.pokeQuadSIMD
inspect $ 'solidQuadProbe `doesNotUse` 'SIMD.pokeQuadCornersSIMD
inspect $ 'solidQuadProbe `doesNotUse` 'SIMD.pokeVertexSIMD
inspect $ hasNoTypeClasses 'solidQuadProbe
inspect $ 'solidQuadProbe `hasNoType` ''[]
-- NoAllocation cannot be used here: on GHC 9.14 unboxed-tuple construction
-- appears as a datacon application, which the plugin (conservatively) counts
-- as allocation. Guard the pattern with type-level checks instead: no boxed
-- tuples/pairs may appear in the inlined poke body.
inspect $ 'solidQuadProbe `hasNoType` ''(,)
inspect $ 'gradientQuadProbe `doesNotUse` 'SIMD.pokeQuadGradientSIMD
inspect $ 'gradientQuadProbe `doesNotUse` 'SIMD.pokeVertexSIMD
inspect $ hasNoTypeClasses 'gradientQuadProbe
inspect $ 'gradientQuadProbe `hasNoType` ''(,,,)

-- The turned image quad's writer, with a corner of its own at each vertex.
cornerQuadProbe :: Ptr Word8 -> Ptr Word8 -> Int -> Float -> Word32 -> IO ()
cornerQuadProbe vp ip offset x base =
  SIMD.pokeQuadCornersSIMD vp offset ip offset x 0 x x 0 x 0 0 0 0 1 1 x x x 1 base

inspect $ 'cornerQuadProbe `doesNotUse` 'SIMD.pokeQuadCornersSIMD
inspect $ 'cornerQuadProbe `doesNotUse` 'SIMD.pokeVertexSIMD
inspect $ hasNoTypeClasses 'cornerQuadProbe
inspect $ 'cornerQuadProbe `hasNoType` ''(,)

-- The store's slot functions take the map they work on as a 'Field'. They
-- must compile to the record code they stand for, with no 'Field' left, so
-- that a composition of writes still builds the store once.
storeWriteProbe :: Int -> Int -> Text -> WidgetStore -> WidgetStore
storeWriteProbe k cursor txt =
  insertSlot fieldText k txt
    . insertSlot fieldInt (slotKey SlotCursor k) cursor
    . deleteSlot fieldInt (slotKey SlotAnchor k)

storeWriteByHand :: Int -> Int -> Text -> WidgetStore -> WidgetStore
storeWriteByHand k cursor txt st0 =
  let st1 = st0 {storeInt = IM.delete (slotKey SlotAnchor k) (storeInt st0)}
      st2 = st1 {storeInt = IM.insert (slotKey SlotCursor k) cursor (storeInt st1)}
   in st2 {storeText = IM.insert k txt (storeText st2)}

storeReadProbe :: Int -> WidgetStore -> (Int, Bool, Maybe Float)
storeReadProbe k st =
  (findSlot fieldInt 7 (slotKey SlotCursor k) st, flagSlot k st, lookupSlot fieldFloat k st)

storeReadByHand :: Int -> WidgetStore -> (Int, Bool, Maybe Float)
storeReadByHand k st =
  ( IM.findWithDefault 7 (slotKey SlotCursor k) (storeInt st)
  , IM.findWithDefault 0 k (storeInt st) /= 0
  , IM.lookup k (storeFloat st)
  )

inspect $ 'storeWriteProbe === 'storeWriteByHand
inspect $ 'storeReadProbe === 'storeReadByHand
inspect $ 'storeWriteProbe `hasNoType` ''Field
inspect $ 'storeReadProbe `hasNoType` ''Field

-- Direct channel traversal must specialize to scalar arithmetic, including
-- when the channel values and the transform are supplied at runtime.
channelProbe :: Float -> V2 -> V2
channelProbe delta = runIdentity . traverseChannels (\_ x -> Identity (x + delta))

channelByHand :: Float -> V2 -> V2
channelByHand delta (V2 x y) = V2 (x + delta) (y + delta)

inspect $ 'channelProbe === 'channelByHand
inspect $ hasNoTypeClasses 'channelProbe
inspect $ 'channelProbe `hasNoType` ''[]

commandReadProbe :: U.Vector DrawCmd -> Int -> Float
commandReadProbe cmds i =
  let
    cmd = U.unsafeIndex cmds i
   in
    cmdClipX cmd + cmdClipY cmd + cmdClipW cmd + cmdClipH cmd

commandWriteProbe ::
  UM.MVector RealWorld DrawCmd -> Int -> Float -> Word32 -> IO ()
commandWriteProbe cmds i x count = UM.unsafeWrite cmds i (DrawCmd x x x x i count count LayerContent)

inspect $ hasNoTypeClasses 'commandReadProbe
inspect $ hasNoTypeClasses 'commandWriteProbe
inspect $ 'commandReadProbe `doesNotUse` 'U.fromURepr
inspect $ 'commandWriteProbe `doesNotUse` 'U.toURepr

-- A block that draws no curve and asks for no draw context builds neither
-- the curve tolerance nor the context.
canvasProbe :: CustomDrawContext -> Float -> Color -> SmallArray DrawOp
canvasProbe cdc x color = runCanvasFor cdc $ do
  drawRect (Rect x 2 3 4) color
  drawCircle (V2 5 x) 6 color

canvasByHand :: CustomDrawContext -> Float -> Color -> SmallArray DrawOp
canvasByHand _ !x color = smallArrayFromList [FillRect (Rect x 2 3 4) color, FillCircle 5 x 6 color]

inspect $ 'canvasProbe === 'canvasByHand
