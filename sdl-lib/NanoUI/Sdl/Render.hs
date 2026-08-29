module NanoUI.Sdl.Render
  ( renderDrawData
  , renderDrawDataPass
  , setLogicalClipRect
  , clearLogicalClipRect
  , logicalClipKey
  , clipPixelRect
  , snapDamage
  ) where

import NanoUI.Sdl.Batch (RenderBatch, batchDrawRange, batchFillSolid, flushRenderBatch)
import NanoUI.Sdl.Image (ImageAtlas, lookupAtlasTex)

import Control.Monad (void, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..), poke)
import NanoUI (Color (..), Rect (..), rectIntersect)
import NanoUI.Testing
  ( Damage (..)
  , DrawCmd (..)
  , DrawData (..)
  , Layer (..)
  , damageIsEmpty
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
  | ClipRect !Rect
  deriving (Eq)

{-# INLINE snapDamage #-}
snapDamage :: Float -> Damage -> Damage
snapDamage _ DamageFull = DamageFull
snapDamage uiScale (DamageClip r) = DamageClip (snapClipRect uiScale r)

snapClipRect :: Float -> Rect -> Rect
snapClipRect uiScale (Rect x y w h) =
  let s = if uiScale > 0 then uiScale else 1
      x0 = fromIntegral (floor (x * s) :: Int)
      y0 = fromIntegral (floor (y * s) :: Int)
      x1 = fromIntegral (ceiling ((x + w) * s) :: Int)
      y1 = fromIntegral (ceiling ((y + h) * s) :: Int)
   in Rect (x0 / s) (y0 / s) ((x1 - x0) / s) ((y1 - y0) / s)

setLogicalClipRect :: Ptr SDL_Renderer -> Float -> Rect -> IO ()
setLogicalClipRect ren uiScale (Rect x y w h) =
  alloca $ \(r :: Ptr SDL_Rect) -> do
    let lx = x * uiScale
        ly = y * uiScale
        rx = (x + w) * uiScale
        by = (y + h) * uiScale
        x0 = floor lx
        y0 = floor ly
        x1 = ceiling rx
        y1 = ceiling by
    poke
      r
      SDL_Rect
        { x = ci x0
        , y = ci y0
        , w = ci (max (0 :: Int) (x1 - x0))
        , h = ci (max (0 :: Int) (y1 - y0))
        }
    void $ setRenderClipRectSafe ren (PtrConst.unsafeFromPtr r)

clearLogicalClipRect :: Ptr SDL_Renderer -> IO ()
clearLogicalClipRect ren = void $ setRenderClipRectSafe ren nullClip

applyClipState :: RenderBatch -> IORef ClipState -> Ptr SDL_Renderer -> Float -> ClipState -> IO ()
applyClipState batch ref ren uiScale next = do
  prev <- readIORef ref
  when (prev /= next) $ do
    flushRenderBatch batch
    writeIORef ref next
    case next of
      ClipNone -> clearLogicalClipRect ren
      ClipRect r -> setLogicalClipRect ren uiScale r

renderDrawData :: Ptr SDL_Renderer -> Float -> Color -> DrawData -> ImageAtlas -> IO ()
renderDrawData _ _ _ _ _ =
  error "renderDrawData requires an active RenderBatch; use renderDrawDataPass"

renderDrawDataPass :: RenderBatch -> Ptr SDL_Renderer -> Float -> Maybe Color -> DrawData -> [Layer] -> ImageAtlas -> Damage -> IO ()
renderDrawDataPass batch ren uiScale mClear drawData layers images damage = do
  when (not (damageIsEmpty damage) && not (null layers)) $ do
    clipRef <- newIORef ClipNone
    clearLogicalClipRect ren
    case (mClear, damage) of
      (Just clearColor, DamageFull) -> do
        let (cr, cg, cb, ca) = unpackColor clearColor
        void $ setRenderDrawColorSafe ren cr cg cb ca
        void $ renderClearSafe ren
      (Just clearColor, DamageClip r) -> do
        let (cr, cg, cb, ca) = unpackColor clearColor
            px = rectX r * uiScale
            py = rectY r * uiScale
            pw = rectW r * uiScale
            ph = rectH r * uiScale
        batchFillSolid batch cr cg cb ca px py pw ph
        applyClipState batch clipRef ren uiScale (ClipRect r)
      (Nothing, DamageClip r) -> applyClipState batch clipRef ren uiScale (ClipRect r)
      (Nothing, DamageFull) -> pure ()
    let selected = [c | c <- drawCommands drawData, cmdLayer c `elem` layers]
        cmds = sortBy (comparing layerOrder) selected
        clip = case damage of
          DamageFull -> Nothing
          DamageClip r -> Just r
        vc = drawVertexCount drawData
        ic = drawIndexCount drawData
    withForeignPtr (drawVertices drawData) $ \vp ->
      withForeignPtr (drawIndices drawData) $ \ip ->
        drawCmds batch ren uiScale vp vc ip ic images clip clipRef cmds
    applyClipState batch clipRef ren uiScale ClipNone

layerOrder :: DrawCmd -> Int
layerOrder cmd =
  case cmdLayer cmd of
    LayerBackground -> 0
    LayerContent -> 1
    LayerOverlay -> 2
    LayerChrome -> 3

drawCmds ::
  RenderBatch ->
  Ptr SDL_Renderer ->
  Float ->
  Ptr Word8 ->
  Int ->
  Ptr Word8 ->
  Int ->
  ImageAtlas ->
  Maybe Rect ->
  IORef ClipState ->
  [DrawCmd] ->
  IO ()
drawCmds batch ren uiScale vp vc ip ic images mDamage clipRef = go
  where
    go [] = pure ()
    go (cmd : rest) = do
      drawCmd batch ren uiScale vp vc ip ic images mDamage clipRef cmd
      go rest

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
  Maybe Rect ->
  IORef ClipState ->
  DrawCmd ->
  IO ()
drawCmd batch ren uiScale vp vc ip ic images mDamage clipRef cmd = do
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
          then applyClipState batch clipRef ren uiScale ClipNone
          else applyClipState batch clipRef ren uiScale (ClipRect clip)
        let !start = fromIntegral (cmdIndexOffset cmd)
            !texId = cmdTextureId cmd
        (tex, tw, th) <-
          if texId > 0
            then
              lookupAtlasTex images texId >>= \case
                Just hit -> pure hit
                Nothing -> pure (nullPtr, 0, 0)
            else pure (nullPtr, 0, 0)
        batchDrawRange batch vp vc ip ic start count texId tex tw th uiScale mDamage

ci :: Int -> CInt
ci = fromIntegral

unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral $ (w `shiftR` 24) .&. 0xFF
  , fromIntegral $ (w `shiftR` 16) .&. 0xFF
  , fromIntegral $ (w `shiftR` 8) .&. 0xFF
  , fromIntegral $ w .&. 0xFF
  )

{-# INLINE logicalClipKey #-}
logicalClipKey :: Float -> Rect -> (Int, Int, Int, Int)
logicalClipKey uiScale (Rect x y w h) =
  let s = if uiScale > 0 then uiScale else 1
      x0 = floor (x * s) :: Int
      y0 = floor (y * s) :: Int
      x1 = ceiling ((x + w) * s) :: Int
      y1 = ceiling ((y + h) * s) :: Int
   in (x0, y0, max 0 (x1 - x0), max 0 (y1 - y0))

{-# INLINE clipPixelRect #-}
clipPixelRect :: Float -> Rect -> (Float, Float, Float, Float)
clipPixelRect uiScale (Rect x y w h) =
  let s = if uiScale > 0 then uiScale else 1
      x0 = fromIntegral (floor (x * s) :: Int) :: Float
      y0 = fromIntegral (floor (y * s) :: Int) :: Float
      x1 = fromIntegral (ceiling ((x + w) * s) :: Int) :: Float
      y1 = fromIntegral (ceiling ((y + h) * s) :: Int) :: Float
   in (x0, y0, max 0 (x1 - x0), max 0 (y1 - y0))
