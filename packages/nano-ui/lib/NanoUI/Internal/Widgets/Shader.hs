{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

-- | Leaf surface shader nodes ('shaderBox', 'shaderConfigured') and Tier 2
-- subtree effect wrappers ('withShader', 'withShaderWith').
module NanoUI.Internal.Widgets.Shader
  ( shaderBox
  , shaderBox'
  , shaderConfigured
  , shaderConfigured'
  , withShader
  , withShader'
  , withShaderWith
  , withShaderWith'
  , askShaderCapabilities
  ) where

import Control.Monad (void)
import Data.Bits (xor)
import Data.Primitive.ByteArray (emptyByteArray)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Context
  ( Context (..)
  , getShaderCapabilities
  , registerNodeShaderAt
  , registerShaderEffectAt
  )
import NanoUI.Internal.Id (mix64)
import NanoUI.Internal.Layout.Arena
  ( NodeType (NodeContainer, NodeShader)
  , addNodeFromLayout
  , mixNodePaintHash
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Internal.Monad (NanoUI, askContext, askDefaultLayout, askInput, liftIO, nextId, withContext)
import NanoUI.Internal.Shader
  ( IsNodeEffect (..)
  , PackedShaderPass (..)
  , Shader (..)
  , ShaderCapabilities (..)
  , ShaderConfig (..)
  , ShaderEffectEntry (..)
  , ShaderKind (Surface)
  , ShaderNodeEntry (..)
  , defaultShaderConfig
  , emptyShaderProgram
  , packShaderPass
  )
import NanoUI.Internal.Style (LayoutModifier, defaultLayout, tight)
import NanoUI.Internal.Types (V2 (..), colorToWord32)
import NanoUI.Internal.WidgetText (containerFlagShaderEffect)
import NanoUI.Internal.Widgets.Combinators (widgetBaseLayout)
import NanoUI.Internal.Widgets.Node (Response, addWidgetNode, currentParent, resolveInteraction, withContainerNode)

-- | Paint a leaf rectangle using a 'Surface' fragment shader and typed
-- uniform value @u@. Uniform changes invalidate the node automatically via
-- its paint hash; unchanged uniforms produce zero damage.
{-# INLINE shaderBox #-}
shaderBox :: LayoutModifier -> Shader 'Surface u -> u -> NanoUI ()
shaderBox f sh = void . shaderBox' f sh

-- | Like 'shaderBox', returning the node's 'Response'.
{-# INLINE shaderBox' #-}
shaderBox' :: LayoutModifier -> Shader 'Surface u -> u -> NanoUI Response
shaderBox' f = shaderConfigured' defaultShaderConfig {scLayout = f}

-- | Paint a leaf 'Surface' shader node with explicit corner radius, fallback
-- color (used on non-GPU backends and SVG export), and vertex tint.
{-# INLINE shaderConfigured #-}
shaderConfigured :: ShaderConfig -> Shader 'Surface u -> u -> NanoUI ()
shaderConfigured cfg sh = void . shaderConfigured' cfg sh

-- | Like 'shaderConfigured', returning the node's 'Response'.
shaderConfigured' :: ShaderConfig -> Shader 'Surface u -> u -> NanoUI Response
shaderConfigured' cfg sh u = do
  wid <- nextId
  ctx <- askContext
  !lay <- widgetBaseLayout defaultLayout (scLayout cfg)
  pass <- liftIO $ case shaderPasses sh of
    (p : _) -> packShaderPass p u
    [] -> pure (PackedShaderPass (shaderHash sh) emptyShaderProgram emptyByteArray 0)
  let !radius = max 0 (scCornerRadius cfg)
      !fallback = scFallback cfg
      !tint = scTint cfg
      !paintH =
        mix64
          (pspHash pass `xor` pspUniformHash pass)
          ( fromIntegral (castFloatToWord32 radius)
              `xor` mix64 (fromIntegral (colorToWord32 fallback)) (fromIntegral (colorToWord32 tint))
          )
      !entry =
        ShaderNodeEntry
          { snePass = pass
          , sneCornerRadius = radius
          , sneFallback = fallback
          , sneTint = tint
          }
  addWidgetNode wid NodeShader T.empty 0 lay $ \arena idx -> do
    mixNodePaintHash arena idx paintH
    registerNodeShaderAt ctx idx entry

-- | Wrap a child subtree in a Tier 2 fragment effect ('NanoUI.ColorEffect',
-- 'NanoUI.DistortionEffect', or 'NanoUI.LayerEffect'). On backends that do not support
-- offscreen node effects, children render straight through with no effect.
{-# INLINE withShader #-}
withShader :: IsNodeEffect k => Shader k u -> u -> NanoUI a -> NanoUI a
withShader = withShaderWith id

-- | Like 'withShader', returning the body's result and the effect container's
-- 'Response' (for hover, clicks, or size-reactive shader uniforms).
{-# INLINE withShader' #-}
withShader' :: IsNodeEffect k => Shader k u -> u -> NanoUI a -> NanoUI (a, Response)
withShader' = withShaderWith' id

-- | Like 'withShader', applying a 'LayoutModifier' to the effect container.
{-# INLINE withShaderWith #-}
withShaderWith :: forall k u a. IsNodeEffect k => LayoutModifier -> Shader k u -> u -> NanoUI a -> NanoUI a
withShaderWith f sh u child = fst <$> withShaderWith' f sh u child

-- | Like 'withShaderWith', returning the body's result and the effect
-- container's 'Response'.
withShaderWith' :: forall k u a. IsNodeEffect k => LayoutModifier -> Shader k u -> u -> NanoUI a -> NanoUI (a, Response)
withShaderWith' f sh u child = do
  let !_ = nodeEffectKind (Proxy @k)
  wid <- nextId
  inp <- askInput
  base <- askDefaultLayout
  let !lay = f (tight base)
  idx <- withContext $ \ctx -> do
    passes <- mapM (`packShaderPass` u) (shaderPasses sh)
    let V2 mx0 my0 = shaderMargin sh u
        !margin = V2 (max 0 mx0) (max 0 my0)
        !entry =
          ShaderEffectEntry
            { seePasses = passes
            , seeMargin = margin
            , seeSvgOpacity = fmap ($ u) (shaderSvgOpacity sh)
            }
        !uHash = foldl' (\acc p -> mix64 acc (pspHash p `xor` pspUniformHash p)) (shaderHash sh) passes
        !mHash = mix64 (fromIntegral (castFloatToWord32 (v2X margin))) (fromIntegral (castFloatToWord32 (v2Y margin)))
    parent <- currentParent ctx
    let arena = ctxNodeArena ctx
    i <- addNodeFromLayout arena NodeContainer parent lay
    setWidgetId arena i wid
    setStyleIdx arena i containerFlagShaderEffect
    mixNodePaintHash arena i (mix64 uHash mHash)
    registerShaderEffectAt ctx i entry
    pure i
  r <- withContainerNode True idx child
  resp <- withContext (\ctx -> resolveInteraction ctx inp (-1) wid)
  pure (r, resp)

-- | Query which shader tiers the active backend supports.
askShaderCapabilities :: NanoUI ShaderCapabilities
askShaderCapabilities = withContext getShaderCapabilities
