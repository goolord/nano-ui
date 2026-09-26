{-# LANGUAGE OverloadedRecordDot #-}

-- | The SDL backend's input-method path on a hidden window, from SDL's
-- native events to the drawn composition and the text input area SDL gets.
module ImeSelftest
  ( selftest
  ) where

import Control.Monad (unless, void)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (isJust, isNothing)
import Data.Text.Foreign qualified as TF
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes, maybePeek, with)
import Foreign.Storable (peek, poke, sizeOf)
import System.Environment (lookupEnv)
import NanoUI
import NanoUI.Backend (clearEphemeral)
import NanoUI.Backend.Sdl
import NanoUI.Sdl.Internal.Input (SdlEvent (..), applyEvent, newTextInputSync, pollEvents, syncTextInput)
import NanoUI.Testing (TextInputArea (..), collectTextSpans, textInputArea)
import NanoUI.Testing.Harness (hasText, held, tabInp)
import DemoApp (withHiddenWindow)
import SDL3.Sys.Bindgen.Events (SDL_Event, SDL_EventType (..), SDL_TextEditingEvent (..), SDL_TextInputEvent (..))
import SDL3.Sys.Bindgen.Events qualified as Events
import SDL3.Sys.Bindgen.Hints (sDL_HINT_IME_IMPLEMENTED_UI)
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Stdinc (Uint32 (..))
import SDL3.Sys.Events (pushEvent)
import SDL3.Sys.Hints (getHint)
import SDL3.Sys.Keyboard (getTextInputArea, textInputActive)
import Data.ByteString qualified as BS

selftest :: IO ()
selftest =
  withHiddenWindow 480 200 (V2 5 5) id $ \ctx env base -> do
    ref <- newIORef "ab"
    zoom <- windowZoom env
    let check ok msg = unless ok (fail ("ime: " <> msg))
        drawFrame inp = void (sdlDrawFrame ctx (void (column (held ref textInput'))) env inp False)
        -- The native events queued so far, folded into the next frame's input.
        step inp = (\evs -> (evs, foldl' applyEvent (clearEphemeral inp) evs)) <$> pollEvents
        sync s = syncTextInput s (sdlWindow env) zoom ctx
    hint <- BS.useAsCString sDL_HINT_IME_IMPLEMENTED_UI (getHint . PtrConst.unsafeFromPtr) >>= maybePeek peekCString . PtrConst.unsafeToPtr
    -- An environment variable of the hint's name wins over the session.
    overridden <- isJust <$> lookupEnv "SDL_IME_IMPLEMENTED_UI"
    check (hint == Just "composition" || overridden) ("the session did not ask SDL for the composition: " <> show hint)
    mapM_ drawFrame [base, base, tabInp base, base]
    (evs, composing) <- editing "かな" 2 0 (step base)
    check (EvEditing "かな" 2 0 `elem` evs && inputComposition composing == Just (Composition "かな" 2 0))
      ("composition not applied: " <> show evs)
    -- The frames drawn told SDL of the focused field: nothing restarts, and it shows.
    restartedInField <- sync (sdlTextInput env) composing
    drawFrame composing
    drawn <- hasText "abかな" <$> collectTextSpans ctx
    value <- readIORef ref
    check (not restartedInField && drawn && value == "ab") ("in the field: " <> show (restartedInField, drawn, value))
    -- A sync that heard of no focus restarts text input, which must be on again.
    restarted <- newTextInputSync >>= (`sync` composing)
    active <- textInputActive (sdlWindow env)
    check (restarted && active) "text input not restarted, or left off, when the focus moved while composing"
    -- The caret's area reaches SDL, in window coordinates.
    area <- textInputArea ctx
    native <- alloca $ \cursorP -> with (SDL_Rect 0 0 0 0) $ \rectP ->
      getTextInputArea (sdlWindow env) rectP cursorP >> (,) <$> peek rectP <*> peek cursorP
    let at v = round (v * zoom)
        want (TextInputArea (Rect x y w h) cursor) = (SDL_Rect (at x) (at y) (max 1 (at w)) (max 1 (at h)), at cursor)
    check (fmap want area == Just native) ("text input area " <> show native <> ", wanted " <> show (want <$> area))
    -- Committed text arrives as typing, and the composition ends.
    (_, committed) <- typed "仮名" (editing "" 0 0 (step composing))
    check (isNothing (inputComposition committed) && inputChars committed == "仮名") "commit not applied"
    mapM_ drawFrame [committed, clearEphemeral committed]
    readIORef ref >>= \v -> check (v == "ab仮名") ("the commit did not insert: " <> show v)
    -- Losing the keyboard focus ends a composition SDL may not report ending.
    (_, again) <- editing "か" 1 0 (step committed)
    (_, blurred) <- focusLost (step again)
    check (isJust (inputComposition again) && isNothing (inputComposition blurred)) "losing the window's focus did not end the composition"
  where
    editing txt start len = queue txt $ \p c ->
      poke p.edit (SDL_TextEditingEvent Events.SDL_EVENT_TEXT_EDITING 0 0 0 c (fromIntegral (start :: Int)) (fromIntegral (len :: Int)))
    typed txt = queue txt $ \p c -> poke p.text (SDL_TextInputEvent Events.SDL_EVENT_TEXT_INPUT 0 0 0 c)
    focusLost = queue "" $ \p _ ->
      let SDL_EventType ty = Events.SDL_EVENT_WINDOW_FOCUS_LOST in poke p.type' (Uint32 (fromIntegral ty))
    -- Queue an event around a native copy of @txt@, which lives while @k@ polls it.
    queue txt fill k = TF.withCString txt $ \c -> do
      alloca $ \p -> do
        fillBytes p 0 (sizeOf (undefined :: SDL_Event))
        fill p (PtrConst.unsafeFromPtr c) :: IO ()
        pushEvent p >>= (`unless` fail "ime: SDL_PushEvent failed")
      k
