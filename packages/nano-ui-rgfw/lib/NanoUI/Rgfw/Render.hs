{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Rgfw.Render
  ( renderArena
  ) where

import Control.Monad (when)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive.PrimArray
  ( newPrimArray
  , readPrimArray
  , writePrimArray
  )
import qualified Data.Text as T
import NanoUI (Color (..), Padding (..), Rect (..), WidgetId (..), colorRGBA)
import NanoUI.Context
  ( Context
  , WidgetStore (..)
  , getStore
  , intKey
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
  , drawText
  , fillRect
  , packColor
  , popClip
  , pushClip
  )
import NanoUI.Rgfw.Theme (RgfwTheme (..))

-- | Pure bounding-box painter.
-- Every widget is strictly drawn as its exact collision / bounding box.
-- Floating popups and context menus are drawn in an overlay pass on top of all widgets.
renderArena ::
  RgfwSurface ->
  CozetteFont ->
  RgfwTheme ->
  Context ->
  NodeArena ->
  WidgetId -> -- Hot (hovered) widget ID
  WidgetId -> -- Active (pressed) widget ID
  WidgetId -> -- Focused widget ID
  IO ()
renderArena surf font theme ctx na hotId activeId focusId = do
  store <- getStore ctx
  !n <- arenaCount na
  when (n > 0) $ withArenaArraysSnap na $ do
    -- Precompute overlay status for all nodes in O(N)
    isOverlayArr <- newPrimArray n
    let prepOverlay !i
          | i >= n = pure ()
          | otherwise = do
              nt <- getNodeType na i
              p <- getParent na i
              isPOverlay <- if p >= 0 && p < i then readPrimArray isOverlayArr p else pure (0 :: Int)
              let !isO = if nt == NodePopup || isPOverlay == 1 then 1 else 0
              writePrimArray isOverlayArr i (isO :: Int)
              prepOverlay (i + 1)
    prepOverlay 0

    let renderNode !i = do
          nt <- getNodeType na i
          (rx, ry, rw, rh) <- getRect na i
          let !x = round rx
              !y = round ry
              !w = round rw
              !h = round rh
          wid <- getWidgetId na i
          val <- getNodeValue na i
          txt <- getText na i
          mClip <- getClipRect na i

          let !isHot = wid /= WidgetId 0 && wid == hotId
              !isActive = wid /= WidgetId 0 && wid == activeId
              !isFocus = wid /= WidgetId 0 && wid == focusId

          case mClip of
            Just (Rect cx cy cw ch) ->
              pushClip surf (round cx) (round cy) (round cw) (round ch)
            Nothing -> pure ()

          case nt of
            NodePopup -> do
              -- Drop shadow + card background + border outline
              fillRect surf (x + 2) (y + 2) w h (packColor (colorRGBA 16 16 16 255))
              fillRect surf x y w h (packColor (thPanelBg theme))
              drawRectOutline surf x y w h (packColor (thBorder theme))

            NodeWindow -> do
              let !headerH = 22
              fillRect surf x (y + headerH) w (max 0 (h - headerH)) (packColor (thPanelBg theme))
              drawRectOutline surf x (y + headerH) w (max 0 (h - headerH)) (packColor (thBorder theme))
              fillRect surf x y w headerH (packColor (thWindowHeader theme))
              drawRectOutline surf x y w headerH (packColor (thBorder theme))
              drawText surf font (x + 8) (y + 4) txt (packColor (thText theme))

            NodePanel -> do
              fillRect surf x y w h (packColor (thPanelBg theme))
              drawRectOutline surf x y w h (packColor (thBorder theme))

            NodeScrollContainer -> do
              fillRect surf x y w h (packColor (thPanelBg theme))
              drawRectOutline surf x y w h (packColor (thBorder theme))

            NodeButton -> do
              isO <- readPrimArray isOverlayArr i
              let !isTab = T.isPrefixOf "tab:" txt || T.isPrefixOf "\x02" txt
              if isO == 1 && not isTab
                then do
                  -- Menu item inside popup: full row hover highlight, left-aligned text
                  when isHot $
                    fillRect surf x y w h (packColor (thWidgetHover theme))
                  when isActive $
                    fillRect surf x y w h (packColor (thPrimaryActive theme))
                  when isFocus $
                    drawRectOutline surf x y w h (packColor (thBorderFocused theme))
                  let !ty = y + max 0 ((h - 13) `div` 2)
                  drawText surf font (x + 8) ty txt (packColor (thText theme))
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
                        fillRect surf x (y + 2) w (max 0 (h - 2)) (packColor (thPanelBg theme))
                        fillRect surf x y w 2 (packColor (thPrimary theme))
                        fillRect surf x (y + 2) 1 (max 0 (h - 2)) (packColor (thBorder theme))
                        fillRect surf (x + w - 1) (y + 2) 1 (max 0 (h - 2)) (packColor (thBorder theme))
                        let !txtW = T.length tabTitle * 6
                            !tx = x + max 0 ((w - txtW) `div` 2)
                            !ty = y + max 0 ((h - 13) `div` 2)
                        drawText surf font tx ty tabTitle (packColor (thText theme))
                      else do
                        -- Inactive Tab: Recessed background, 4-sided border, muted text
                        let !bgColor = if isHot then thWidgetHover theme else thWidgetBg theme
                            !txtColor = if isHot then thText theme else thTextMuted theme
                            !borderColor = if isFocus then thBorderFocused theme else thBorder theme
                            !txtW = T.length tabTitle * 6
                            !tx = x + max 0 ((w - txtW) `div` 2)
                            !ty = y + max 0 ((h - 13) `div` 2)
                        fillRect surf x y w h (packColor bgColor)
                        drawRectOutline surf x y w h (packColor borderColor)
                        drawText surf font tx ty tabTitle (packColor txtColor)
                  else do
                    -- Standard Push Button
                    let !bgColor =
                          if isActive
                            then thPrimaryActive theme
                            else if isHot
                              then thPrimaryHover theme
                              else thPrimary theme
                        !borderColor = if isFocus then thBorderFocused theme else thBorder theme
                        !txtColor = thText theme
                        !txtW = T.length txt * 6
                        !tx = x + max 0 ((w - txtW) `div` 2)
                        !ty = y + max 0 ((h - 13) `div` 2)
                    fillRect surf x y w h (packColor bgColor)
                    drawRectOutline surf x y w h (packColor borderColor)
                    drawText surf font tx ty txt (packColor txtColor)

            NodeCheckbox -> do
              let !boxX = x + 2
                  !boxY = y + max 0 ((h - 14) `div` 2)
                  !boxS = 14
                  !borderColor = if isFocus || isHot then thBorderFocused theme else thBorder theme
              fillRect surf boxX boxY boxS boxS (packColor (thWidgetBg theme))
              drawRectOutline surf boxX boxY boxS boxS (packColor borderColor)
              when (val > 0.5) $ do
                fillRect surf (boxX + 3) (boxY + 3) (boxS - 6) (boxS - 6) (packColor (thPrimary theme))
              when (not (T.null txt)) $ do
                drawText surf font (boxX + boxS + 6) (y + max 0 ((h - 13) `div` 2)) txt (packColor (thText theme))

            NodeRadio -> do
              let !boxX = x + 2
                  !boxY = y + max 0 ((h - 14) `div` 2)
                  !boxS = 14
                  !borderColor = if isFocus || isHot then thBorderFocused theme else thBorder theme
              fillRect surf boxX boxY boxS boxS (packColor (thWidgetBg theme))
              drawRectOutline surf boxX boxY boxS boxS (packColor borderColor)
              when (val > 0.5) $ do
                fillRect surf (boxX + 4) (boxY + 4) (boxS - 8) (boxS - 8) (packColor (thPrimary theme))
              when (not (T.null txt)) $ do
                drawText surf font (boxX + boxS + 6) (y + max 0 ((h - 13) `div` 2)) txt (packColor (thText theme))

            NodeSlider -> do
              let !trackY = y + (h - 6) `div` 2
                  !trackH = 6
                  !clampedVal = max 0 (min 1 val)
                  !thumbW = 10
                  !thumbH = 16
                  !thumbX = x + round (fromIntegral (w - thumbW) * clampedVal)
                  !thumbY = y + (h - thumbH) `div` 2
                  !thumbColor = if isActive then thPrimaryActive theme else if isHot then thThumbHover theme else thThumb theme
              fillRect surf x trackY w trackH (packColor (thScrollTrack theme))
              drawRectOutline surf x trackY w trackH (packColor (thBorder theme))
              let !fillW = max 0 (thumbX - x + thumbW `div` 2)
              fillRect surf x trackY fillW trackH (packColor (thPrimary theme))
              fillRect surf thumbX thumbY thumbW thumbH (packColor thumbColor)
              drawRectOutline surf thumbX thumbY thumbW thumbH (packColor (thBorder theme))

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
                let !selX = x + 6 + max 0 (min (w - 10) (selLo * 6))
                    !selX2 = x + 6 + max 0 (min (w - 10) (selHi * 6))
                    !selW = max 1 (selX2 - selX)
                fillRect surf selX (y + 3) selW (max 0 (h - 6)) (packColor (thSelection theme))

              when (not (T.null displayTxt)) $
                drawText surf font (x + 6) (y + max 0 ((h - 13) `div` 2)) displayTxt (packColor textColor)

              when isFocus $ do
                let !cursorX = x + 6 + max 0 (min (w - 10) (curPos * 6))
                fillRect surf cursorX (y + 3) 2 (max 0 (h - 6)) (packColor (thText theme))

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
                            let !sx = x + 6 + max 0 (min (w - 10) (sc * 6))
                                !sx2 = x + 6 + max 0 (min (w - 10) (ec * 6))
                                !sw = max 1 (sx2 - sx)
                                !sy = y + 6 + r * 14
                            when (sy + 13 <= y + h) $
                              fillRect surf sx sy sw 13 (packColor (thSelection theme))
                          drawSelRow (r + 1)
                drawSelRow r0

              let renderLines !_ [] = pure ()
                  renderLines !ly (l : ls)
                    | ly + 13 > y + h = pure ()
                    | otherwise = do
                        drawText surf font (x + 6) ly l (packColor (thText theme))
                        renderLines (ly + 14) ls
              renderLines (y + 6) safeLines

              when isFocus $ do
                let !cursorX = x + 6 + max 0 (min (w - 10) (curCol * 6))
                    !cursorY = y + 6 + curRow * 14
                fillRect surf cursorX cursorY 2 (min 13 (max 0 (y + h - cursorY))) (packColor (thText theme))

            NodeSelect -> do
              let !borderColor = if isFocus || isHot then thBorderFocused theme else thBorder theme
              fillRect surf x y w h (packColor (thWidgetBg theme))
              drawRectOutline surf x y w h (packColor borderColor)
              drawText surf font (x + 6) (y + max 0 ((h - 13) `div` 2)) txt (packColor (thText theme))
              let !arrowX = x + w - 16
              drawText surf font arrowX (y + max 0 ((h - 13) `div` 2)) "v" (packColor (thTextMuted theme))

            NodeBox -> do
              styleIdx <- getStyleIdx na i
              let !boxColor =
                    if styleIdx /= 0
                      then packColor (Color (fromIntegral styleIdx))
                      else packColor (thPrimary theme)
              fillRect surf x y w h boxColor
              drawRectOutline surf x y w h (packColor (thBorder theme))

            NodeText -> do
              pad <- getPadding na i
              when (not (T.null txt)) $ do
                drawText surf font (x + round (padL pad)) (y + round (padT pad)) txt (packColor (thText theme))

            NodeSeparator -> do
              fillRect surf x y w h (packColor (thBorder theme))

            NodeSpacer ->
              pure ()

            _ ->
              pure ()

          when (isJust mClip) $
            popClip surf

    -- Pass 1: Render all in-flow / non-overlay nodes
    let loopNormal !i
          | i >= n = pure ()
          | otherwise = do
              isO <- readPrimArray isOverlayArr i
              when (isO == 0) $ renderNode i
              loopNormal (i + 1)
    loopNormal 0

    -- Pass 2: Render floating popups and their descendants
    let loopOverlay !i
          | i >= n = pure ()
          | otherwise = do
              isO <- readPrimArray isOverlayArr i
              when (isO == 1) $ renderNode i
              loopOverlay (i + 1)
    loopOverlay 0
