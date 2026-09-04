{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Rgfw.Render
  ( renderArena
  , renderTextEditMenuOverlay
  ) where

import Control.Exception (finally)
import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import qualified Data.IntMap.Strict as IM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import qualified Data.Text as T
import GHC.Exts (RealWorld)
import GHC.IO (unsafePerformIO)
import NanoUI (Color (..), FontVariant (..), Padding (..), Rect (..), V2 (..), WidgetId (..), colorRGBA, rectContains)
import NanoUI.Context
  ( Context
  , TextInputMenu (..)
  , WidgetStore (..)
  , ctxFontMetrics
  , ctxHostProfile
  , ctxTextInputMenu
  , getStore
  , intKey
  )
import NanoUI.Frame.Hit (widgetOverlayAllowed)
import NanoUI.Frame.TextEdit
  ( TextEditMenuRow (..)
  , textFieldMenuActionEnabled
  , textEditMenuContentRect
  , textEditMenuLayout
  )
import NanoUI.Store
  ( slotAnchor
  , slotCursor
  , slotKey
  , slotTextAreaAnchorCol
  , slotTextAreaAnchorRow
  , slotTextAreaCol
  , slotTextAreaRow
  )
import NanoUI.Layout.Arena
  ( NodeArena
  , NodeType (..)
  , isFloatingNode
  , arenaCount
  , getClipRect
  , getNodeType
  , getNodeValue
  , getPadding
  , getParent
  , getRect
  , getStyleIdx
  , getText
  , getWidgetId
  , withArenaArraysSnap
  )
import NanoUI.Rgfw.Font.Cozette (CozetteFont)
import NanoUI.Rgfw.Surface
  ( RgfwSurface
  , drawRectOutline
  , drawTextScaled
  , fillRect
  , packColor
  , popClip
  , pushClip
  , toPhysRect
  )
import NanoUI.Rgfw.Theme (RgfwTheme (..))

textNodeFontVariant :: Int -> FontVariant
textNodeFontVariant si =
  let v = si .&. 0x0F
   in if v >= fromEnum (minBound :: FontVariant) && v <= fromEnum (maxBound :: FontVariant)
        then toEnum v
        else FontRegular

data RenderScratch = RenderScratch
  { rsCap     :: {-# UNPACK #-} !Int
  , rsOverlay :: !(MutablePrimArray RealWorld Int)
  , rsPopup   :: !(MutablePrimArray RealWorld Int)
  , rsWindows :: !(MutablePrimArray RealWorld Int)
  , rsPopups  :: !(MutablePrimArray RealWorld Int)
  }

{-# NOINLINE globalRenderScratchPool #-}
globalRenderScratchPool :: IORef (Maybe RenderScratch)
globalRenderScratchPool = unsafePerformIO (newIORef Nothing)

allocRenderScratch :: Int -> IO RenderScratch
allocRenderScratch !cap = do
  ov <- newPrimArray cap
  pop <- newPrimArray cap
  wins <- newPrimArray cap
  pops <- newPrimArray cap
  pure $ RenderScratch cap ov pop wins pops

withRenderScratch :: Int -> (RenderScratch -> IO a) -> IO a
withRenderScratch !reqN act = do
  mSc <- atomicModifyIORef' globalRenderScratchPool (\m -> (Nothing, m))
  sc <- case mSc of
    Just s | rsCap s >= reqN -> pure s
    _ -> do
      let !cap = max 256 (max reqN (maybe 0 ((* 2) . rsCap) mSc))
      allocRenderScratch cap
  act sc `finally` atomicModifyIORef' globalRenderScratchPool (\_ -> (Just sc, ()))

-- | Pure bounding-box painter.
-- Every widget is strictly drawn as its exact collision / bounding box.
-- Floating popups and context menus are drawn in an overlay pass on top of all widgets.
renderArena ::
  RgfwSurface ->
  CozetteFont ->
  Float -> -- Scale factor
  RgfwTheme ->
  Context ->
  NodeArena ->
  WidgetId -> -- Hot (hovered) widget ID
  WidgetId -> -- Active (pressed) widget ID
  WidgetId -> -- Focused widget ID
  IO ()
renderArena surf font !scale theme ctx na hotId activeId focusId = do
  store <- getStore ctx
  !n <- arenaCount na
  when (n > 0) $ withRenderScratch n $ \scratch -> withArenaArraysSnap na $ do
    let isOverlayArr = rsOverlay scratch
        isPopupArr   = rsPopup scratch
        winArr       = rsWindows scratch
        popArr       = rsPopups scratch

    let renderNode !i = do
          nt <- getNodeType na i
          (rx, ry, rw, rh) <- getRect na i
          let (!x, !y, !w, !h) = toPhysRect scale rx ry rw rh
          wid <- getWidgetId na i
          val <- getNodeValue na i
          txt <- getText na i
          mClip <- getClipRect na i

          let !isHot = wid /= WidgetId 0 && wid == hotId
              !isActive = wid /= WidgetId 0 && wid == activeId
              !isFocus = wid /= WidgetId 0 && wid == focusId

          case mClip of
            Just (Rect cx cy cw ch) ->
              let (!px, !py, !pw, !ph) = toPhysRect scale cx cy cw ch
               in pushClip surf px py pw ph
            Nothing -> pure ()

          case nt of
            NodePopup -> do
              -- Drop shadow + card background + border outline
              let !shadowOff = max 1 (round (2.0 * scale))
              fillRect surf (x + shadowOff) (y + shadowOff) w h (packColor (colorRGBA 16 16 16 255))
              fillRect surf x y w h (packColor (thPanelBg theme))
              drawRectOutline surf x y w h (packColor (thBorder theme))

            NodeWindow -> do
              -- Drop shadow + window panel + header
              let !shadowOff = max 1 (round (3.0 * scale))
                  !headerBottom = round ((ry + 24.0) * scale)
                  !headerH = max 1 (headerBottom - y)
              fillRect surf (x + shadowOff) (y + shadowOff) w h (packColor (colorRGBA 16 16 16 255))
              fillRect surf x (y + headerH) w (max 0 (h - headerH)) (packColor (thPanelBg theme))
              drawRectOutline surf x (y + headerH) w (max 0 (h - headerH)) (packColor (thBorder theme))
              fillRect surf x y w headerH (packColor (thWindowHeader theme))
              drawRectOutline surf x y w headerH (packColor (thBorder theme))
              when (not (T.null txt)) $
                drawTextScaled surf font scale (rx + 8.0) (ry + 5.5) txt (packColor (thText theme))

              -- Bottom-right resize grip (three diagonal hatch marks)
              let !gripColor = packColor (thBorder theme)
                  !gx = x + w - max 4 (round (4.0 * scale))
                  !gy = y + h - max 4 (round (4.0 * scale))
                  !ps = max 1 (round scale)
                  !d1 = max 2 (round (3.0 * scale))
                  !d2 = max 4 (round (6.0 * scale))
                  !d3 = max 6 (round (9.0 * scale))
              fillRect surf (gx - d1) gy ps ps gripColor
              fillRect surf gx (gy - d1) ps ps gripColor
              fillRect surf (gx - d2) gy ps ps gripColor
              fillRect surf (gx - d1) (gy - d1) ps ps gripColor
              fillRect surf gx (gy - d2) ps ps gripColor
              fillRect surf (gx - d3) gy ps ps gripColor
              fillRect surf (gx - d2) (gy - d1) ps ps gripColor
              fillRect surf (gx - d1) (gy - d2) ps ps gripColor
              fillRect surf gx (gy - d3) ps ps gripColor

            NodePanel -> do
              fillRect surf x y w h (packColor (thPanelBg theme))
              drawRectOutline surf x y w h (packColor (thBorder theme))

            NodeScrollContainer -> do
              isO <- readPrimArray isOverlayArr i
              when (isO == 0) $ do
                fillRect surf x y w h (packColor (thPanelBg theme))
                drawRectOutline surf x y w h (packColor (thBorder theme))

            NodeButton -> do
              isPop <- readPrimArray isPopupArr i
              let !isTab = T.isPrefixOf "tab:" txt || T.isPrefixOf "\x02" txt
                  !isClose = T.isPrefixOf "\x01" txt || txt == "[X]" || txt == "X" || txt == "\xd7" || txt == "\xf00d"
              if isClose
                then do
                  -- Window or Tab Close Button
                  let !bgColor =
                        if isActive
                          then colorRGBA 160 36 36 255
                          else if isHot
                            then colorRGBA 205 45 45 255
                            else colorRGBA 0 0 0 0
                      !iconColor =
                        if isHot || isActive
                          then packColor (colorRGBA 255 255 255 255)
                          else packColor (thText theme)
                      !borderColor =
                        if isHot || isActive
                          then colorRGBA 180 40 40 255
                          else if isFocus
                            then thBorderFocused theme
                            else colorRGBA 0 0 0 0
                  when (isHot || isActive) $ do
                    fillRect surf x y w h (packColor bgColor)
                    drawRectOutline surf x y w h (packColor borderColor)
                  when (isFocus && not (isHot || isActive)) $
                    drawRectOutline surf x y w h (packColor (thBorderFocused theme))

                  -- Draw centered crisp diagonal close cross
                  let !cx = rx + rw / 2.0
                      !cy = ry + rh / 2.0
                      !arm = max 2.5 (min 4.5 (min rw rh / 4.0))
                      drawCross !d
                        | d > arm = pure ()
                        | otherwise = do
                            let (!px1, !py1, !pw, !ph) = toPhysRect scale (cx + d - 0.5) (cy + d - 0.5) 1.0 1.0
                                (!px2, !py2, _, _)     = toPhysRect scale (cx - d - 0.5) (cy + d - 0.5) 1.0 1.0
                                (!px3, !py3, _, _)     = toPhysRect scale (cx + d - 0.5) (cy - d - 0.5) 1.0 1.0
                                (!px4, !py4, _, _)     = toPhysRect scale (cx - d - 0.5) (cy - d - 0.5) 1.0 1.0
                            fillRect surf px1 py1 pw ph iconColor
                            fillRect surf px2 py2 pw ph iconColor
                            fillRect surf px3 py3 pw ph iconColor
                            fillRect surf px4 py4 pw ph iconColor
                            drawCross (d + 1.0)
                  drawCross 0.0
                else if isPop == 1 && not isTab
                  then do
                    -- Menu item inside popup: full row hover highlight, left-aligned text
                    when isHot $
                      fillRect surf x y w h (packColor (thWidgetHover theme))
                    when isActive $
                      fillRect surf x y w h (packColor (thPrimaryActive theme))
                    when isFocus $
                      drawRectOutline surf x y w h (packColor (thBorderFocused theme))
                    let !ty = ry + max 0.0 ((rh - 13.0) / 2.0)
                    drawTextScaled surf font scale (rx + 4.0) ty txt (packColor (thText theme))
                else if isTab
                  then do
                    let (!tabTitle, !isActiveTab) =
                          if T.isPrefixOf "tab:active:" txt
                            then (T.drop 11 txt, True)
                            else if T.isPrefixOf "tab:" txt
                              then (T.drop 4 txt, False)
                              else if T.isPrefixOf "\x02" txt
                                then (T.drop 1 txt, val > 0.5)
                                else (txt, False)
                    if isActiveTab
                      then do
                        -- Active Tab: Same background as panel body, 2px top primary accent stripe, open bottom!
                        let !stripeH = max 1 (round (2.0 * scale))
                        fillRect surf x (y + stripeH) w (max 0 (h - stripeH)) (packColor (thPanelBg theme))
                        fillRect surf x y w stripeH (packColor (thPrimary theme))
                        fillRect surf x (y + stripeH) 1 (max 0 (h - stripeH)) (packColor (thBorder theme))
                        fillRect surf (x + w - 1) (y + stripeH) 1 (max 0 (h - stripeH)) (packColor (thBorder theme))
                        let !txtW = fromIntegral (T.length tabTitle * 6)
                            !tx = rx + max 0.0 ((rw - txtW) / 2.0)
                            !ty = ry + max 0.0 ((rh - 13.0) / 2.0)
                        drawTextScaled surf font scale tx ty tabTitle (packColor (thText theme))
                      else do
                        -- Inactive Tab: Recessed background, 4-sided border, muted text
                        let !bgColor = if isHot then thWidgetHover theme else thWidgetBg theme
                            !txtColor = if isHot then thText theme else thTextMuted theme
                            !borderColor = if isFocus then thBorderFocused theme else thBorder theme
                            !txtW = fromIntegral (T.length tabTitle * 6)
                            !tx = rx + max 0.0 ((rw - txtW) / 2.0)
                            !ty = ry + max 0.0 ((rh - 13.0) / 2.0)
                        fillRect surf x y w h (packColor bgColor)
                        drawRectOutline surf x y w h (packColor borderColor)
                        drawTextScaled surf font scale tx ty tabTitle (packColor txtColor)
                  else do
                    -- Standard Push Button
                    let !dispTxt =
                          if T.isPrefixOf "\x01" txt || T.isPrefixOf "\x02" txt || T.isPrefixOf "\x03" txt
                            then T.drop 1 txt
                            else txt
                        !bgColor =
                          if isActive
                            then thPrimaryActive theme
                            else if isHot
                              then thPrimaryHover theme
                              else thPrimary theme
                        !borderColor = if isFocus then thBorderFocused theme else thBorder theme
                        !txtColor = thText theme
                        !txtW = fromIntegral (T.length dispTxt * 6)
                        !tx = rx + max 0.0 ((rw - txtW) / 2.0)
                        !ty = ry + max 0.0 ((rh - 13.0) / 2.0)
                    fillRect surf x y w h (packColor bgColor)
                    drawRectOutline surf x y w h (packColor borderColor)
                    drawTextScaled surf font scale tx ty dispTxt (packColor txtColor)

            NodeCheckbox -> do
              let !boxLogX = rx + 2.0
                  !boxLogY = ry + max 0.0 ((rh - 14.0) / 2.0)
                  (!bx, !by, !bw, !bh) = toPhysRect scale boxLogX boxLogY 14.0 14.0
                  !borderColor = if isFocus || isHot then thBorderFocused theme else thBorder theme
              fillRect surf bx by bw bh (packColor (thWidgetBg theme))
              drawRectOutline surf bx by bw bh (packColor borderColor)
              when (val > 0.5) $ do
                let (!cx, !cy, !cw, !ch) = toPhysRect scale (boxLogX + 3.0) (boxLogY + 3.0) 8.0 8.0
                fillRect surf cx cy cw ch (packColor (thPrimary theme))
              when (not (T.null txt)) $ do
                drawTextScaled surf font scale (boxLogX + 14.0 + 6.0) (ry + max 0.0 ((rh - 13.0) / 2.0)) txt (packColor (thText theme))

            NodeRadio -> do
              let !boxLogX = rx + 2.0
                  !boxLogY = ry + max 0.0 ((rh - 14.0) / 2.0)
                  (!bx, !by, !bw, !bh) = toPhysRect scale boxLogX boxLogY 14.0 14.0
                  !borderColor = if isFocus || isHot then thBorderFocused theme else thBorder theme
              fillRect surf bx by bw bh (packColor (thWidgetBg theme))
              drawRectOutline surf bx by bw bh (packColor borderColor)
              when (val > 0.5) $ do
                let (!cx, !cy, !cw, !ch) = toPhysRect scale (boxLogX + 4.0) (boxLogY + 4.0) 6.0 6.0
                fillRect surf cx cy cw ch (packColor (thPrimary theme))
              when (not (T.null txt)) $ do
                drawTextScaled surf font scale (boxLogX + 14.0 + 6.0) (ry + max 0.0 ((rh - 13.0) / 2.0)) txt (packColor (thText theme))

            NodeSlider -> do
              let !trackLogY = ry + (rh - 6.0) / 2.0
                  (!tx, !ty, !tw, !th) = toPhysRect scale rx trackLogY rw 6.0
                  !clampedVal = max 0.0 (min 1.0 val)
                  !thumbW = 10.0 :: Float
                  !thumbH = 16.0 :: Float
                  !thumbLogX = rx + (rw - thumbW) * clampedVal
                  !thumbLogY = ry + (rh - thumbH) / 2.0
                  (!thx, !thy, !thw, !thh) = toPhysRect scale thumbLogX thumbLogY thumbW thumbH
                  !thumbColor = if isActive then thPrimaryActive theme else if isHot then thThumbHover theme else thThumb theme
              fillRect surf tx ty tw th (packColor (thScrollTrack theme))
              drawRectOutline surf tx ty tw th (packColor (thBorder theme))
              let !fillW = max 0 (thx - tx + thw `div` 2)
              fillRect surf tx ty fillW th (packColor (thPrimary theme))
              fillRect surf thx thy thw thh (packColor thumbColor)
              drawRectOutline surf thx thy thw thh (packColor (thBorder theme))

            NodeTextInput -> do
              let !key = intKey wid
                  !mTyped = IM.lookup key (storeText store)
                  (!displayTxt, !isPlaceholder) = case mTyped of
                    Just t  -> if T.null t then (txt, True) else (t, False)
                    Nothing -> (txt, True)
                  !textColor = if isPlaceholder then thTextMuted theme else thText theme
                  !borderColor = if isFocus then thBorderFocused theme else thBorder theme
                  !bgColor = if isHot then thWidgetHover theme else thWidgetBg theme
                  !textLen = T.length displayTxt
                  !curPos = max 0 (min textLen (IM.findWithDefault textLen (slotKey slotCursor key) (storeInt store)))
                  !anchorPos = max 0 (min textLen (IM.findWithDefault curPos (slotKey slotAnchor key) (storeInt store)))
                  !selLo = min anchorPos curPos
                  !selHi = max anchorPos curPos

              fillRect surf x y w h (packColor bgColor)
              drawRectOutline surf x y w h (packColor borderColor)

              -- Draw selection highlight
              when (isFocus && not isPlaceholder && selLo < selHi) $ do
                let !selLogX = rx + 6.0 + max 0.0 (min (rw - 10.0) (fromIntegral selLo * 6.0))
                    !selLogX2 = rx + 6.0 + max 0.0 (min (rw - 10.0) (fromIntegral selHi * 6.0))
                    (!sx, !sy, !sw, !sh) = toPhysRect scale selLogX (ry + 3.0) (max 1.0 (selLogX2 - selLogX)) (max 0.0 (rh - 6.0))
                fillRect surf sx sy sw sh (packColor (thSelection theme))

              when (not (T.null displayTxt)) $
                drawTextScaled surf font scale (rx + 6.0) (ry + max 0.0 ((rh - 13.0) / 2.0)) displayTxt (packColor textColor)

              when isFocus $ do
                let !cursorLogX = rx + 6.0 + max 0.0 (min (rw - 10.0) (fromIntegral curPos * 6.0))
                    !cx = round (cursorLogX * scale)
                    !cy = round ((ry + 3.0) * scale)
                    !ch = max 0 (round ((ry + rh - 3.0) * scale) - cy)
                    !cw = max 1 (round (2.0 * scale))
                fillRect surf cx cy cw ch (packColor (thText theme))

            NodeTextArea -> do
              let !key = intKey wid
                  !mTyped = IM.lookup key (storeText store)
                  !displayTxt = fromMaybe txt mTyped
                  !borderColor = if isFocus then thBorderFocused theme else thBorder theme
                  !curRow = IM.findWithDefault 0 (slotKey slotTextAreaRow key) (storeInt store)
                  !curCol = IM.findWithDefault 0 (slotKey slotTextAreaCol key) (storeInt store)
                  !anchorRow = IM.findWithDefault curRow (slotKey slotTextAreaAnchorRow key) (storeInt store)
                  !anchorCol = IM.findWithDefault curCol (slotKey slotTextAreaAnchorCol key) (storeInt store)
              fillRect surf x y w h (packColor (thWidgetBg theme))
              drawRectOutline surf x y w h (packColor borderColor)
              let linesOfText = T.lines displayTxt
                  safeLines = if null linesOfText then [""] else linesOfText
                  ((r0, c0), (r1, c1)) =
                    if (anchorRow, anchorCol) <= (curRow, curCol)
                      then ((anchorRow, anchorCol), (curRow, curCol))
                      else ((curRow, curCol), (anchorRow, anchorCol))

              -- Draw multi-line selection highlight
              when (isFocus && (anchorRow, anchorCol) /= (curRow, curCol)) $ do
                let drawSelRow !r
                      | r > r1 = pure ()
                      | otherwise = do
                          let lineStr = if r < length safeLines then safeLines !! r else ""
                              lineLen = T.length lineStr
                              (sc, ec) =
                                if r == r0 && r == r1
                                  then (c0, c1)
                                  else if r == r0
                                    then (c0, lineLen)
                                    else if r == r1
                                      then (0, c1)
                                      else (0, lineLen)
                          when (ec > sc) $ do
                            let !selLogX = rx + 6.0 + max 0.0 (min (rw - 10.0) (fromIntegral sc * 6.0))
                                !selLogX2 = rx + 6.0 + max 0.0 (min (rw - 10.0) (fromIntegral ec * 6.0))
                                !selLogY = ry + 6.0 + fromIntegral r * 14.0
                                (!sx, !sy, !sw, !sh) = toPhysRect scale selLogX selLogY (max 1.0 (selLogX2 - selLogX)) 13.0
                            when (selLogY + 13.0 <= ry + rh) $
                              fillRect surf sx sy sw sh (packColor (thSelection theme))
                          drawSelRow (r + 1)
                drawSelRow r0

              let renderLines !_ [] = pure ()
                  renderLines !ly (l : ls)
                    | ly + 13.0 > ry + rh = pure ()
                    | otherwise = do
                        drawTextScaled surf font scale (rx + 6.0) ly l (packColor (thText theme))
                        renderLines (ly + 14.0) ls
              renderLines (ry + 6.0) safeLines

              when isFocus $ do
                let !cursorLogX = rx + 6.0 + max 0.0 (min (rw - 10.0) (fromIntegral curCol * 6.0))
                    !cursorLogY = ry + 6.0 + fromIntegral curRow * 14.0
                    !cx = round (cursorLogX * scale)
                    !cy = round (cursorLogY * scale)
                    !ch = min (round (13.0 * scale)) (max 0 (y + h - cy))
                    !cw = max 1 (round (2.0 * scale))
                fillRect surf cx cy cw ch (packColor (thText theme))

            NodeSelect -> do
              let !borderColor = if isFocus || isHot then thBorderFocused theme else thBorder theme
              fillRect surf x y w h (packColor (thWidgetBg theme))
              drawRectOutline surf x y w h (packColor borderColor)
              drawTextScaled surf font scale (rx + 6.0) (ry + max 0.0 ((rh - 13.0) / 2.0)) txt (packColor (thText theme))
              let !arrowLogX = rx + rw - 16.0
              drawTextScaled surf font scale arrowLogX (ry + max 0.0 ((rh - 13.0) / 2.0)) "v" (packColor (thTextMuted theme))

            NodeBox -> do
              styleIdx <- getStyleIdx na i
              let !boxColor =
                    if styleIdx /= 0
                      then packColor (Color (fromIntegral styleIdx))
                      else packColor (thPrimary theme)
              fillRect surf x y w h boxColor
              drawRectOutline surf x y w h (packColor (thBorder theme))

            NodeText -> do
              isPop <- readPrimArray isPopupArr i
              pad <- getPadding na i
              styleIdx <- getStyleIdx na i
              let fvar = textNodeFontVariant styleIdx
                  !txtColor = if fvar == FontMuted then thTextMuted theme else thText theme
              when (not (T.null txt)) $ do
                let !isMultiline = T.elem '\n' txt
                    (!tx, !ty) =
                      if isPop == 1 && padL pad == 0
                        then
                          let !offX = 4.0
                              !offY = max 0.0 ((rh - 13.0) / 2.0)
                           in (rx + offX, ry + offY)
                        else if not isMultiline && padT pad == 0 && rh > 13.0
                          then (rx + padL pad, ry + max 0.0 ((rh - 13.0) / 2.0))
                          else (rx + padL pad, ry + padT pad)
                drawTextScaled surf font scale tx ty txt (packColor txtColor)

            NodeSeparator -> do
              isO <- readPrimArray isOverlayArr i
              if isO == 1
                then do
                  let !lineY = y + max 1 (h `div` 2)
                      !padX = max 1 (round (2.0 * scale))
                  fillRect surf (rowX' + padX) lineY (max 0 (w - padX * 2)) 1 (packColor (thBorder theme))
                else fillRect surf x y w h (packColor (thBorder theme))
              where
                !rowX' = x

            NodeSpacer ->
              pure ()

            _ ->
              pure ()

          when (isJust mClip) $
            popClip surf

    -- Main Pass: Classify overlay status and render in-flow nodes immediately.
    -- Deferred window and popup node indices are collected into scratch arrays.
    let loopMain !i !winCount !popCount
          | i >= n = pure (winCount, popCount)
          | otherwise = do
              nt <- getNodeType na i
              p <- getParent na i
              isPOverlay <- if p >= 0 && p < i then readPrimArray isOverlayArr p else pure (0 :: Int)
              let !isO = if isFloatingNode nt || isPOverlay == 1 then 1 else 0
              writePrimArray isOverlayArr i (isO :: Int)
              isPPopup <- if p >= 0 && p < i then readPrimArray isPopupArr p else pure (0 :: Int)
              let !isP = if nt == NodePopup || isPPopup == 1 then 1 else 0
              writePrimArray isPopupArr i (isP :: Int)
              if isP == 1
                then do
                  writePrimArray popArr popCount i
                  loopMain (i + 1) winCount (popCount + 1)
                else if isO == 1
                  then do
                    writePrimArray winArr winCount i
                    loopMain (i + 1) (winCount + 1) popCount
                  else do
                    renderNode i
                    loopMain (i + 1) winCount popCount

    (!winCount, !popCount) <- loopMain 0 0 0

    -- Overlay Pass 1: Render floating windows & modals and their contents over in-flow UI
    let drawWindows !k
          | k >= winCount = pure ()
          | otherwise = do
              idx <- readPrimArray winArr k
              renderNode idx
              drawWindows (k + 1)
    drawWindows 0

    -- Overlay Pass 2: Render floating popups and context menus on top of everything
    let drawPopups !k
          | k >= popCount = pure ()
          | otherwise = do
              idx <- readPrimArray popArr k
              renderNode idx
              drawPopups (k + 1)
    drawPopups 0

-- | Render the built-in text input / text area context menu overlay
renderTextEditMenuOverlay ::
  RgfwSurface ->
  CozetteFont ->
  Float -> -- Scale factor
  RgfwTheme ->
  Context ->
  V2 -> -- Mouse position for hover highlight
  IO ()
renderTextEditMenuOverlay surf font !scale theme ctx mousePos = do
  mMenu <- readIORef (ctxTextInputMenu ctx)
  case mMenu of
    Nothing -> pure ()
    Just menu -> do
      let wid = textInputMenuWidget menu
      allow <- widgetOverlayAllowed ctx wid
      when allow $ do
        let menuRect = textInputMenuRect menu
            (!mx, !my, !mw, !mh) = toPhysRect scale (rectX menuRect) (rectY menuRect) (rectW menuRect) (rectH menuRect)
            content = textEditMenuContentRect (ctxHostProfile ctx) menuRect (ctxFontMetrics ctx)
            !cx = rectX content
            !cy = rectY content
            !cw = rectW content
            !shadowOff = max 1 (round (2.0 * scale))
        -- Drop shadow
        fillRect surf (mx + shadowOff) (my + shadowOff) mw mh (packColor (colorRGBA 16 16 16 255))
        -- Background & border
        fillRect surf mx my mw mh (packColor (thPanelBg theme))
        drawRectOutline surf mx my mw mh (packColor (thBorder theme))
        -- Render menu rows
        forM_ (textEditMenuLayout (ctxHostProfile ctx)) $ \(entry, relY, h) -> do
          let (!rowX, !rowY, !rowW, !rowH) = toPhysRect scale cx (cy + relY) cw h
              rowRect = Rect cx (cy + relY) cw h
          case entry of
            TextEditMenuSep -> do
              let !lineY = rowY + max 1 (rowH `div` 2)
                  !padX = max 1 (round (2.0 * scale))
              fillRect surf (rowX + padX) lineY (max 0 (rowW - padX * 2)) 1 (packColor (thBorder theme))
            TextEditMenuItem action lbl -> do
              enabled <- textFieldMenuActionEnabled ctx wid action
              let !hovered = enabled && rectContains rowRect mousePos
              when hovered $ do
                fillRect surf rowX rowY rowW rowH (packColor (thWidgetHover theme))
                let !barW = max 1 (round (2.0 * scale))
                fillRect surf rowX (rowY + barW) barW (max 1 (rowH - barW * 2)) (packColor (thPrimary theme))
              let !textColor = if enabled then thText theme else thTextMuted theme
                  !textY = cy + relY + max 0.0 ((h - 13.0) / 2.0)
              drawTextScaled surf font scale (cx + 5.0) textY lbl (packColor textColor)

