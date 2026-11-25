{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Core shader descriptors, capability flags, and packed per-frame call
-- payloads. Shaders are pure, immutable, content-hashed values: views never
-- hold a backend handle, and backends compile and cache GPU programs lazily
-- on first draw.
module NanoUI.Internal.Shader
  ( ShaderKind (..)
  , IsNodeEffect (..)
  , ShaderCapabilities (..)
  , noShaderCapabilities
  , ShaderProgram (..)
  , emptyShaderProgram
  , hashShaderProgram
  , CpuFragCtx (..)
  , CpuShaderEval
  , ShaderPass (..)
  , Shader (..)
  , contramapShader
  , fixUniform
  , rawShader
  , rawLayerShader
  , ShaderConfig (..)
  , defaultShaderConfig
  , PackedShaderPass (..)
  , packShaderPass
  , ShaderNodeEntry (..)
  , ShaderEffectEntry (..)
  ) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Dynamic (Dynamic)
import Data.Primitive.ByteArray
  ( ByteArray
  , MutableByteArray
  , fillByteArray
  , newByteArray
  , unsafeFreezeByteArray
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Style (LayoutModifier)
import NanoUI.Internal.Types (Color, V2 (..), colorTransparent, colorWhite)
import NanoUI.Internal.Uniforms
  ( UniformField (..)
  , Uniforms (..)
  , hashUniformBytes
  , uniformLayout
  , uniformSize
  )

-- | Role a fragment shader plays in the node tree.
data ShaderKind
  = -- | Leaf surface fill over a node's rectangle; requires no offscreen target.
    Surface
  | -- | Pure pixel-color transform over a subtree; composes and fuses into one pass.
    ColorEffect
  | -- | Coordinate warp over a subtree; fuses with other distortions and color effects.
    DistortionEffect
  | -- | General neighborhood-sampling post-process over an offscreen subtree target.
    LayerEffect
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Constraint selecting shader kinds valid for wrapping a node subtree
-- ('NanoUI.withShader').
class IsNodeEffect (k :: ShaderKind) where
  -- | Witness the effect kind at runtime.
  nodeEffectKind :: Proxy k -> ShaderKind

instance IsNodeEffect 'ColorEffect where
  nodeEffectKind _ = ColorEffect

instance IsNodeEffect 'DistortionEffect where
  nodeEffectKind _ = DistortionEffect

instance IsNodeEffect 'LayerEffect where
  nodeEffectKind _ = LayerEffect

-- | Which shader features the active backend supports.
data ShaderCapabilities = ShaderCapabilities
  { capSurfaceShaders :: !Bool
  -- ^ Backend can run Tier 1 leaf surface shaders.
  , capNodeEffects :: !Bool
  -- ^ Backend can run Tier 2 offscreen subtree effects.
  }
  deriving (Eq, Show)

-- | Capabilities for headless, SVG, or non-GPU backends.
noShaderCapabilities :: ShaderCapabilities
noShaderCapabilities = ShaderCapabilities False False

-- | Compiled backend shader payloads for one fragment pass. Fields are lazy
-- so each backend only evaluates the target format it needs.
data ShaderProgram = ShaderProgram
  { spGlsl150 :: Maybe Text
  -- ^ OpenGL 3.2 core fragment shader (@nano-ui-rgfw@).
  , spGlslEs300 :: Maybe Text
  -- ^ WebGL2 fragment shader (@nano-ui-sdl-web@).
  , spSpirv :: Maybe ByteString
  -- ^ SPIR-V 1.0 fragment binary (@nano-ui-sdl@ Vulkan via SDL 3.4 GPU).
  , spMsl :: Maybe Text
  -- ^ Metal Shading Language fragment source (@nano-ui-sdl@ Metal).
  , spNeedsTex :: !Bool
  -- ^ Whether the fragment shader samples the input texture (@uInput@).
  , spUniformLayout :: ![UniformField]
  -- ^ Flattened std140 fields of the user uniform block (after the 48-byte
  -- system header).
  , spUniformBytes :: !Int
  -- ^ Padded user uniform byte count (multiple of 16; 0 for @()@).
  }

-- | Program with no backend artifacts and no user uniforms.
emptyShaderProgram :: ShaderProgram
emptyShaderProgram =
  ShaderProgram
    { spGlsl150 = Nothing
    , spGlslEs300 = Nothing
    , spSpirv = Nothing
    , spMsl = Nothing
    , spNeedsTex = False
    , spUniformLayout = []
    , spUniformBytes = 0
    }

-- | Stable 64-bit FNV-1a content hash identifying a compiled 'ShaderProgram'
-- across frames for backend program caching and paint-hash tracking.
hashShaderProgram :: ShaderProgram -> Word64
hashShaderProgram sp =
  let h0 = 0xcbf29ce484222325 :: Word64
      h1 = mixByte h0 (if spNeedsTex sp then 1 else 0)
      h2 = mixInt h1 (spUniformBytes sp)
      h3 = maybe h2 (mixBytes h2 . TE.encodeUtf8) (spGlsl150 sp)
      h4 = maybe h3 (mixBytes h3 . TE.encodeUtf8) (spGlslEs300 sp)
      h5 = maybe h4 (mixBytes h4) (spSpirv sp)
      h6 = maybe h5 (mixBytes h5 . TE.encodeUtf8) (spMsl sp)
   in if h6 == 0 then 1 else h6
  where
    mixByte :: Word64 -> Word8 -> Word64
    mixByte !h !b = (h `xor` fromIntegral b) * 0x100000001b3
    mixInt :: Word64 -> Int -> Word64
    mixInt !h !n = (h `xor` fromIntegral n) * 0x100000001b3
    mixBytes !h0 bs = BS.foldl' mixByte (mixInt h0 (BS.length bs)) bs

-- | Fragment evaluation context passed to a CPU fallback evaluator.
data CpuFragCtx = CpuFragCtx
  { cfcUv :: !V2
  -- ^ Normalized coordinates in @[0, 1] x [0, 1]@.
  , cfcPos :: !V2
  -- ^ Local logical pixel coordinates.
  , cfcSize :: !V2
  -- ^ Node width and height in logical pixels.
  , cfcScale :: !Float
  -- ^ Physical pixels per logical layout unit.
  , cfcColor :: !Color
  -- ^ Vertex tint color.
  , cfcCornerRadius :: !Float
  -- ^ Logical corner radius.
  }
  deriving (Eq, Show)

-- | Pure CPU fragment evaluator: given fragment context and an input texture
-- sampler (in local logical pixel coordinates), returns the output color.
type CpuShaderEval = CpuFragCtx -> (V2 -> Color) -> Color

-- | One GPU fragment pass parameterized by uniform record @u@.
data ShaderPass u = ShaderPass
  { spHash :: {-# UNPACK #-} !Word64
  , spProgram :: !ShaderProgram
  , spUniformSize :: {-# UNPACK #-} !Int
  , spPack :: !(u -> MutableByteArray RealWorld -> Int -> IO ())
  }

-- | Pure, phantom-typed fragment shader descriptor.
--
-- @k@ fixes whether the shader paints a leaf 'Surface' or wraps a subtree
-- ('ColorEffect', 'DistortionEffect', 'LayerEffect'). @u@ is the Haskell
-- uniform record packed into std140 bytes each frame.
data Shader (k :: ShaderKind) u = Shader
  { shaderHash :: {-# UNPACK #-} !Word64
  , shaderPasses :: ![ShaderPass u]
  -- ^ GPU passes executed in order (one for most shaders; two or more for
  -- separable multi-pass filters such as Gaussian blur).
  , shaderMargin :: !(u -> V2)
  -- ^ Maximum sample offset in logical pixels in X and Y, used to size
  -- offscreen targets and expand partial-damage bounds.
  , shaderCpuEval :: !(Maybe (u -> CpuShaderEval))
  -- ^ Optional CPU evaluator for headless differential testing and SVG export.
  , shaderSvgOpacity :: !(Maybe (u -> Float))
  -- ^ Optional native opacity extractor so @nano-ui-svg@ can emit @<g opacity>@.
  , shaderFusion :: !(Maybe Dynamic)
  -- ^ Optional monomorphic EDSL fusion body attached by @nano-ui-shader@ so
  -- compatible effects fuse into a single GPU pass.
  }

-- | Adapt a shader to accept a larger or different uniform record @u2@.
contramapShader :: (u2 -> u1) -> Shader k u1 -> Shader k u2
contramapShader f s =
  s
    { shaderPasses = [p {spPack = \u -> spPack p (f u)} | p <- shaderPasses s]
    , shaderMargin = shaderMargin s . f
    , shaderCpuEval = fmap (. f) (shaderCpuEval s)
    , shaderSvgOpacity = fmap (. f) (shaderSvgOpacity s)
    , shaderFusion = Nothing
    }

-- | Specialize a shader with a constant uniform value.
fixUniform :: u -> Shader k u -> Shader k ()
fixUniform u = contramapShader (const u)

-- | Build a shader from hand-written backend sources while keeping typed
-- 'Uniforms' packing and automatic paint-hash damage tracking.
rawShader :: forall k u. Uniforms u => ShaderProgram -> Shader k u
rawShader prog0 =
  let !layout = uniformLayout (Proxy :: Proxy u)
      !sz = uniformSize (Proxy :: Proxy u)
      prog = prog0 {spUniformLayout = layout, spUniformBytes = sz}
      !h = hashShaderProgram prog
      pass =
        ShaderPass
          { spHash = h
          , spProgram = prog
          , spUniformSize = sz
          , spPack = \u mba base -> do
              _ <- pokeUniformAt u mba base 0
              pure ()
          }
   in Shader
        { shaderHash = h
        , shaderPasses = [pass]
        , shaderMargin = const (V2 0 0)
        , shaderCpuEval = Nothing
        , shaderSvgOpacity = Nothing
        , shaderFusion = Nothing
        }

-- | Build a 'LayerEffect' shader from hand-written backend sources with an
-- explicit maximum sample offset in logical pixels.
rawLayerShader :: forall u. Uniforms u => (u -> V2) -> ShaderProgram -> Shader 'LayerEffect u
rawLayerShader margin prog = (rawShader prog {spNeedsTex = True}) {shaderMargin = margin}

-- | Configuration for a leaf 'Surface' shader node ('NanoUI.shaderConfigured').
data ShaderConfig = ShaderConfig
  { scLayout :: !LayoutModifier
  -- ^ Layout modifier applied to 'NanoUI.defaultLayout'.
  , scCornerRadius :: !Float
  -- ^ Corner radius in logical pixels, forwarded in @uMeta.y@ and used for
  -- fallback rounded-rect rendering.
  , scFallback :: !Color
  -- ^ Solid fill drawn when the active backend does not support shaders (or
  -- during SVG export when CPU rasterization is off).
  , scTint :: !Color
  -- ^ Vertex tint color passed as @vColor@ (default opaque white).
  }

-- | Default leaf shader config: unconstrained layout, square corners,
-- transparent fallback, white tint.
defaultShaderConfig :: ShaderConfig
defaultShaderConfig =
  ShaderConfig
    { scLayout = id
    , scCornerRadius = 0
    , scFallback = colorTransparent
    , scTint = colorWhite
    }

-- | Existential-free packed pass payload captured during view construction.
data PackedShaderPass = PackedShaderPass
  { pspHash :: {-# UNPACK #-} !Word64
  , pspProgram :: !ShaderProgram
  , pspUniforms :: !ByteArray
  , pspUniformHash :: {-# UNPACK #-} !Word64
  }

-- | Pack a 'ShaderPass' with its frame uniform value @u@ into zero-padded
-- std140 bytes and compute its content hash.
packShaderPass :: ShaderPass u -> u -> IO PackedShaderPass
packShaderPass pass u = do
  let !sz = spUniformSize pass
  mba <- newByteArray sz
  whenPos sz $ do
    fillByteArray mba 0 sz 0
    spPack pass u mba 0
  ba <- unsafeFreezeByteArray mba
  let !uHash = hashUniformBytes ba
  pure
    PackedShaderPass
      { pspHash = spHash pass
      , pspProgram = spProgram pass
      , pspUniforms = ba
      , pspUniformHash = uHash
      }
  where
    whenPos n act = if n > 0 then act else pure ()

-- | Registration attached to a 'NodeShader' in 'NanoUI.Internal.Context.Types.NodeCustom'.
data ShaderNodeEntry = ShaderNodeEntry
  { snePass :: !PackedShaderPass
  , sneCornerRadius :: {-# UNPACK #-} !Float
  , sneFallback :: {-# UNPACK #-} !Color
  , sneTint :: {-# UNPACK #-} !Color
  }

-- | Registration attached to a shaded 'NodeContainer' in
-- 'NanoUI.Internal.Context.Types.NodeCustom'.
data ShaderEffectEntry = ShaderEffectEntry
  { seePasses :: ![PackedShaderPass]
  , seeMargin :: !V2
  , seeSvgOpacity :: !(Maybe Float)
  }
