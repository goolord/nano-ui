module NanoUI.Sdl.Render
  ( ClipState (..)
  , renderDrawData
  , renderDrawDataPass
  , setLogicalClipRect
  , setLogicalClipKey
  , clearLogicalClipRect
  , logicalClipKey
  , toClipKey
  , clipPixelRect
  , snapDamage
  ) where

import NanoUI.Sdl.Batch (RenderBatch, batchDrawRange, batchFillSolid, flushRenderBatch)
import NanoUI.Sdl.Image (ImageAtlas, lookupAtlasTex)

import Control.Monad (void, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..), poke)
import GHC.IO (unsafePerformIO)
import NanoUI (Color (..), Rect (..), rectIntersect)
import NanoUI.Testing
  ( Damage (..)
  , DrawCmd (..)
  , DrawData (..)
  , Layer (..)
  , damageIsEmpty
  , glyphAtlasTextureId
  )
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Render
  ( renderClearSafe
  , setRenderClipRectSafe
  , setRenderDrawColorSafe
  )

nullClip :: PtrConst.PtrConst SDL_Rect
nullClip = PtrConst.unsafeFromPtr (nullPtr :: Ptr SDL_Rect)

data ClipState
  = ClipNone
  | ClipKey {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq)

{-# NOINLINE clipRectScratch #-}
clipRectScratch :: ForeignPtr SDL_Rect
clipRectScratch = unsafePerformIO (mallocForeignPtrBytes (sizeOf (undefined :: SDL_Rect)))

{-# INLINE snapDamage #-}
snapDamage :: Float -> Damage -> Damage
snapDamage _ DamageFull = DamageFull
snapDamage scale (DamageClip (Rect x y w h)) =
  let px = fromIntegral (floor (x * scale) :: Int) / scale
      py = fromIntegral (floor (y * scale) :: Int) / scale
      pw = fromIntegral (ceiling ((x + w) * scale) :: Int) / scale - px
      ph = fromIntegral (ceiling ((y + h) * scale) :: Int) / scale - py
   in DamageClip (Rect px py pw ph)

{-# INLINE clipPixelRect #-}
clipPixelRect :: Float -> Rect -> (Float, Float, Float, Float)
clipPixelRect uiScale (Rect x y w h) =
  let s = if uiScale > 0 then uiScale else 1
      x0 = fromIntegral (floor (x * s) :: Int) :: Float
      y0 = fromIntegral (floor (y * s) :: Int) :: Float
      x1 = fromIntegral (ceiling ((x + w) * s) :: Int) :: Float
      y1 = fromIntegral (ceiling ((y + h) * s) :: Int) :: Float
   in (x0, y0, x1 - x0, y1 - y0)

{-# INLINE logicalClipKey #-}
logicalClipKey :: Rect -> (Int, Int, Int, Int)
logicalClipKey (Rect x y w h) =
  let px = floor x :: Int
      py = floor y :: Int
      x1 = ceiling (x + w) :: Int
      y1 = ceiling (y + h) :: Int
      pw = max 1 (x1 - px)
      ph = max 1 (y1 - py)
   in (px, py, pw, ph)

{-# INLINE toClipKey #-}
toClipKey :: Rect -> ClipState
toClipKey r =
  let (px, py, pw, ph) = logicalClipKey r
   in ClipKey px py pw ph

setLogicalClipKey :: Ptr SDL_Renderer -> (Int, Int, Int, Int) -> IO ()
setLogicalClipKey ren (px, py, pw, ph) =
  withForeignPtr clipRectScratch $ \(sr :: Ptr SDL_Rect) -> do
    poke sr (SDL_Rect (fromIntegral px) (fromIntegral py) (fromIntegral pw) (fromIntegral ph))
    void $ setRenderClipRectSafe ren (PtrConst.unsafeFromPtr sr)

setLogicalClipRect :: Ptr SDL_Renderer -> Rect -> IO ()
setLogicalClipRect ren r =
  setLogicalClipKey ren (logicalClipKey r)

clearLogicalClipRect :: Ptr SDL_Renderer -> IO ()
clearLogicalClipRect ren = void $ setRenderClipRectSafe ren nullClip

applyClipState :: RenderBatch -> IORef ClipState -> Ptr SDL_Renderer -> ClipState -> IO ()
applyClipState batch ref ren next = do
  prev <- readIORef ref
  when (prev /= next) $ do
    flushRenderBatch batch
    writeIORef ref next
    case next of
      ClipNone -> clearLogicalClipRect ren
      ClipKey px py pw ph -> setLogicalClipKey ren (px, py, pw, ph)

renderDrawData :: Ptr SDL_Renderer -> Float -> Color -> DrawData -> ImageAtlas -> IO ()
renderDrawData _ _ _ _ _ =
  error "renderDrawData requires an active RenderBatch; use renderDrawDataPass"

renderDrawDataPass :: RenderBatch -> Ptr SDL_Renderer -> Float -> Maybe Color -> DrawData -> SmallArray Layer -> ImageAtlas -> Ptr () -> Damage -> IO ()
renderDrawDataPass batch ren uiScale mClear drawData layers images glyphTex damage = do
  when (not (damageIsEmpty damage) && sizeofSmallArray layers /= 0) $ do
    clipRef <- newIORef ClipNone
    clearLogicalClipRect ren
    case (mClear, damage) of
      (Just clearColor, DamageFull) -> do
        let (cr, cg, cb, ca) = unpackColor clearColor
        void $ setRenderDrawColorSafe ren cr cg cb ca
        void $ renderClearSafe ren
      (Just clearColor, DamageClip r) -> do
        let (cr, cg, cb, ca) = unpackColor clearColor
        batchFillSolid batch cr cg cb ca (rectX r) (rectY r) (rectW r) (rectH r)
        applyClipState batch clipRef ren (toClipKey r)
      (Nothing, DamageClip r) -> applyClipState batch clipRef ren (toClipKey r)
      (Nothing, DamageFull) -> pure ()
    let clip = case damage of
          DamageFull -> Nothing
          DamageClip r -> Just r
        vc = drawVertexCount drawData
        ic = drawIndexCount drawData
        cmds = drawCommands drawData
        !n = V.length cmds
        !layerMask = computeLayerMask layers
    withForeignPtr (drawVertices drawData) $ \vp ->
      withForeignPtr (drawIndices drawData) $ \ip ->
        let go !i
              | i >= n = pure ()
              | otherwise = do
                  let !cmd = V.unsafeIndex cmds i
                  when (testLayerMask layerMask (cmdLayer cmd)) $
                    drawCmd batch ren uiScale vp vc ip ic images glyphTex clip clipRef cmd
                  go (i + 1)
         in go 0
    applyClipState batch clipRef ren ClipNone

{-# INLINE layerOrder #-}
layerOrder :: Layer -> Int
layerOrder ly =
  case ly of
    LayerBackground -> 0
    LayerContent -> 1
    LayerOverlay -> 2
    LayerChrome -> 3

{-# INLINE computeLayerMask #-}
computeLayerMask :: SmallArray Layer -> Int
computeLayerMask arr = go 0 0
  where
    !len = sizeofSmallArray arr
    go !acc !i
      | i >= len = acc
      | otherwise =
          let !l = indexSmallArray arr i
              !bit = 1 `shiftL` layerOrder l
           in go (acc .|. bit) (i + 1)

{-# INLINE testLayerMask #-}
testLayerMask :: Int -> Layer -> Bool
testLayerMask !mask !l =
  (mask .&. (1 `shiftL` layerOrder l)) /= 0

{-# INLINE drawCmd #-}
drawCmd ::
  RenderBatch ->
  Ptr SDL_Renderer ->
  Float ->
  Ptr Word8 ->
  Int ->
  Ptr Word8 ->
  Int ->
  ImageAtlas ->
  Ptr () ->
  Maybe Rect ->
  IORef ClipState ->
  DrawCmd ->
  IO ()
drawCmd batch ren uiScale vp vc ip ic images glyphTex mDamage clipRef cmd = do
  let !count = fromIntegral (cmdIndexCount cmd)
      !cmdRect = Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)
      !cmdOpen = cmdClipW cmd >= 1e8 || cmdClipH cmd >= 1e8
      live = case (mDamage, cmdOpen) of
        (Nothing, _) -> Just cmdRect
        (Just dmg, True) -> Just dmg
        (Just dmg, False) -> rectIntersect dmg cmdRect
  when (count >= 3) $
    case live of
      Nothing -> pure ()
      Just clip -> do
        if cmdOpen && mDamage == Nothing
          then applyClipState batch clipRef ren ClipNone
          else applyClipState batch clipRef ren (toClipKey clip)
        let !start = fromIntegral (cmdIndexOffset cmd)
            !texId = cmdTextureId cmd
        (tex, tw, th) <-
          if texId == glyphAtlasTextureId
            then pure (glyphTex, 0, 0)
            else if texId > 0
              then
                lookupAtlasTex images texId >>= \case
                  Just hit -> pure hit
                  Nothing  -> pure (nullPtr, 0, 0)
              else pure (nullPtr, 0, 0)
        batchDrawRange batch vp vc ip ic start count texId tex tw th uiScale mDamage

{-# INLINE unpackColor #-}
unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral ((w `shiftR` 24) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral (w .&. 0xFF)
  )
