{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module NanoUI.Monad
  ( NanoUI
  , Ui
  , runNanoUI
  , runUi
  , uiIO
  , emit
  , withKey
  , keyed
  , keyedTag
  , scope
  , nextId
  , burstNextIds
  , currentId
  , askContext
  , askInput
  , askHost
  , damageWidgetNow
  , damageKeyNow
  , damageRectNow
  , damageGroupNow
  , damageFullNow
  )
where

import Control.Monad (forM_)
import Data.Hashable (Hashable, hash)
import Data.IORef (readIORef, writeIORef)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Effectful
  ( Dispatch (Static)
  , DispatchOf
  , Eff
  , Effect
  , IOE
  , runEff
  , type (:>)
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects)
  , StaticRep
  , evalStaticRep
  , getStaticRep
  , unsafeEff_
  )
import NanoUI.Context (Context (..), askHostIO, damageFull, damageKey, damagePeers, damageRect, damageWidget, pushMessage)
import NanoUI.Types (DamageBounds, Rect)
import NanoUI.Id
  ( IdContext (IdContext, siblingId)
  , WidgetId (..)
  , enterKeyed
  , enterScope
  , mix64
  , scopeTag
  )
import NanoUI.Input (Input)
import NanoUI.Messages (FrameMsg (..))

type NanoUI = Eff '[Ui, IOE]

data Ui :: Effect

type instance DispatchOf Ui = Static WithSideEffects

data instance StaticRep Ui = UiRep !Context !Input

{-# INLINE runUi #-}
runUi :: IOE :> es => Context -> Input -> Eff (Ui : es) a -> Eff es a
runUi ctx inp = evalStaticRep (UiRep ctx inp)

{-# INLINE runNanoUI #-}
runNanoUI :: Context -> Input -> NanoUI a -> IO a
runNanoUI ctx inp = runEff . runUi ctx inp

{-# INLINE uiIO #-}
uiIO :: Ui :> es => IO a -> Eff es a
uiIO m = do
  UiRep {} <- getStaticRep
  unsafeEff_ m

{-# INLINE emit #-}
emit :: (Typeable msg, Ui :> es) => msg -> Eff es ()
emit msg = do
  ctx <- askContext
  uiIO (pushMessage ctx (FrameMsg msg))

{-# INLINE peekId #-}
peekId :: Ui :> es => Eff es WidgetId
peekId = do
  ctx <- askContext
  ic <- uiIO (readIORef (ctxIdContext ctx))
  let
    IdContext cid sid = ic
    raw = mix64 cid sid
  pure (if raw == 0 then WidgetId 1 else WidgetId raw)

{-# INLINE nextId #-}
nextId :: Ui :> es => Eff es WidgetId
nextId = do
  ctx <- askContext
  uiIO $ do
    ic <- readIORef (ctxIdContext ctx)
    let
      IdContext cid sid = ic
      raw = mix64 cid sid
      wid = if raw == 0 then WidgetId 1 else WidgetId raw
    writeIORef (ctxIdContext ctx) (ic {siblingId = sid + 1})
    pure wid

-- | Issue many widget ids in one IO loop (avoids deep Eff bind chains).
{-# INLINE burstNextIds #-}
burstNextIds :: Ui :> es => Int -> Eff es ()
burstNextIds n
  | n <= 0 = pure ()
  | otherwise = do
      ctx <- askContext
      uiIO $ forM_ [1 .. n] $ \_ -> do
        ic <- readIORef (ctxIdContext ctx)
        let IdContext _ sid = ic
        writeIORef (ctxIdContext ctx) (ic {siblingId = sid + 1})

{-# INLINE currentId #-}
currentId :: Ui :> es => Eff es WidgetId
currentId = peekId

{-# INLINE scope #-}
scope :: Ui :> es => Eff es a -> Eff es a
scope m = do
  ctx <- askContext
  parent' <- uiIO $ do
    old <- readIORef (ctxIdContext ctx)
    let
      (p, c) = enterScope scopeTag old
    writeIORef (ctxIdContext ctx) c
    pure p
  r <- m
  uiIO (writeIORef (ctxIdContext ctx) parent')
  pure r

{-# INLINE keyed #-}

-- | Stable child path from @tag@. Keys must be unique among siblings in the same scope.
keyed :: (Hashable k, Ui :> es) => k -> Eff es a -> Eff es a
keyed k = keyedTag (fromIntegral (hash k))

{-# INLINE keyedTag #-}
keyedTag :: Ui :> es => Word64 -> Eff es a -> Eff es a
keyedTag tag m = do
  ctx <- askContext
  parent' <- uiIO $ do
    old <- readIORef (ctxIdContext ctx)
    let
      (p, c) = enterKeyed tag old
    writeIORef (ctxIdContext ctx) c
    pure p
  r <- m
  uiIO (writeIORef (ctxIdContext ctx) parent')
  pure r

{-# INLINE withKey #-}
withKey :: (Hashable k, Ui :> es) => k -> Eff es a -> Eff es a
withKey = keyed

{-# INLINE askContext #-}
askContext :: Ui :> es => Eff es Context
askContext = do
  UiRep ctx _ <- getStaticRep
  pure ctx

{-# INLINE askInput #-}
askInput :: Ui :> es => Eff es Input
askInput = do
  UiRep _ inp <- getStaticRep
  pure inp

{-# INLINE askHost #-}
askHost :: (Typeable a, Ui :> es) => Eff es (Maybe a)
askHost = do
  ctx <- askContext
  uiIO (askHostIO ctx)

{-# INLINE damageWidgetNow #-}
damageWidgetNow :: (Ui :> es) => WidgetId -> DamageBounds -> Eff es ()
damageWidgetNow wid bounds = do
  ctx <- askContext
  uiIO (damageWidget ctx wid bounds)

{-# INLINE damageKeyNow #-}
damageKeyNow :: (Ui :> es) => Int -> DamageBounds -> Eff es ()
damageKeyNow k bounds = do
  ctx <- askContext
  uiIO (damageKey ctx k bounds)

{-# INLINE damageRectNow #-}
damageRectNow :: (Ui :> es) => Rect -> Eff es ()
damageRectNow r = do
  ctx <- askContext
  uiIO (damageRect ctx r)

{-# INLINE damageGroupNow #-}
damageGroupNow :: (Ui :> es) => [WidgetId] -> DamageBounds -> Eff es ()
damageGroupNow wids bounds = do
  ctx <- askContext
  uiIO (damagePeers ctx wids bounds)

{-# INLINE damageFullNow #-}
damageFullNow :: (Ui :> es) => Eff es ()
damageFullNow = do
  ctx <- askContext
  uiIO (damageFull ctx)
