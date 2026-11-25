-- | Push buttons.
module NanoUI.Internal.Widgets.Button
  ( button
  , button'
  , buttonWith
  , buttonWith'
  , ButtonConfig (..)
  , defaultButtonConfig
  , buttonConfigured
  , buttonConfigured'
  , iconButton
  , iconButton'
  , buttonContent
  , buttonContent'
  , buttonContentWith
  , buttonContentWith'
  , buttonContentStyled
  , buttonGroup
  , buttonGroupWith
  )
where

import Control.Monad (when)
import Data.Bits ((.|.))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (NodeType (..), addNodeFromLayout, getFirstChild, getNextSibling, mixNodePaintHash, setStyleIdx)
import NanoUI.Internal.Monad (NanoUI, askDefaultLayout, uiTheme, withContext)
import NanoUI.Internal.Style (Direction (..), Layout (..), Style (..), Theme (..), defaultLayout, gap, tight)
import NanoUI.Internal.Types (Color)
import NanoUI.Internal.WidgetText (buttonFlagContent, containerFlagButtonGroup)
import NanoUI.Internal.Widgets.Adornment (Adornments, adornButton, adornmentRow, icon, leading)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx)
import NanoUI.Internal.Widgets.Node (Response, currentParent, respClicked, respId, withContainerNode, withWidgetChildren)
import NanoUI.Svg (Svg)

-- | Button with a text label. 'True' on the frame it is clicked, by pointer
-- or by Enter or Space while focused.
--
-- > whenM (button "Save") saveDocument
{-# INLINE button #-}
button :: Text -> NanoUI Bool
button txt = respClicked <$> button' txt

-- | 'button' returning its 'Response', for tooltips, anchored popups, or
-- hover state.
--
-- > help <- button' "Help"
-- > tooltip help "Open the manual"
-- > when (respClicked help) openManual
{-# INLINE button' #-}
button' :: Text -> NanoUI Response
button' = buttonWith' id

-- | 'button' with a layout modifier.
--
-- > whenM (buttonWith (fixedW 120) "Submit") submitForm
{-# INLINE buttonWith #-}
buttonWith :: (Layout -> Layout) -> Text -> NanoUI Bool
buttonWith f txt = respClicked <$> buttonWith' f txt

{-# INLINE buttonWith' #-}
-- | 'buttonWith' returning the full response; activation is in @respClicked@.
buttonWith' :: (Layout -> Layout) -> Text -> NanoUI Response
buttonWith' f txt = buttonStyledEx True txt 0 (f defaultLayout) 0

-- | A button's layout, and the adornments ("NanoUI.Adornment") it draws
-- beside its label. The label and its adornments sit together in the middle
-- of the button, the layout's gap apart.
data ButtonConfig = ButtonConfig
  { bcLayout :: !Layout
  , bcAdornments :: !Adornments
  }
  deriving (Show)

-- | The default layout and no adornments: a plain 'button'.
defaultButtonConfig :: ButtonConfig
defaultButtonConfig = ButtonConfig {bcLayout = defaultLayout, bcAdornments = mempty}

-- | 'button' with its own layout or adornments, drawn in the label's colour.
--
-- > whenM (buttonConfigured defaultButtonConfig {bcAdornments = A.trailing (A.icon chevron)} "Next") next
{-# INLINE buttonConfigured #-}
buttonConfigured :: ButtonConfig -> Text -> NanoUI Bool
buttonConfigured cfg txt = respClicked <$> buttonConfigured' cfg txt

-- | 'buttonConfigured' returning its 'Response'.
buttonConfigured' :: ButtonConfig -> Text -> NanoUI Response
buttonConfigured' (ButtonConfig lay adorns) txt = do
  resp <- buttonStyledEx True txt 0 lay 0
  adornButton resp lay (labelColor lay) adorns

labelColor :: Layout -> Theme -> Color
labelColor lay theme = fromMaybe (styleFg (themeButton theme)) (layoutFontColor lay)

-- | A button with an icon before its label, or with an empty label an icon
-- alone in a square button.
--
-- > whenM (iconButton saveIcon "Save") saveDocument
-- > whenM (iconButton gearIcon "") openSettings
{-# INLINE iconButton #-}
iconButton :: Svg -> Text -> NanoUI Bool
iconButton doc txt = respClicked <$> iconButton' doc txt

-- | 'iconButton' returning its 'Response', for a tooltip on an icon alone.
{-# INLINE iconButton' #-}
iconButton' :: Svg -> Text -> NanoUI Response
iconButton' doc = buttonConfigured' defaultButtonConfig {bcAdornments = leading (icon doc)}

-- | A button whose content is any view, as iced's buttons are: laid out in a
-- row, the layout's gap apart, and centred as a label is, with a label's
-- padding around it. Labels in it take the button's text colour. The content
-- is for display, so a press anywhere on the button is the button's.
--
-- > whenM (buttonContent (svgIcon 16 saveIcon >> label "Save")) saveDocument
{-# INLINE buttonContent #-}
buttonContent :: NanoUI () -> NanoUI Bool
buttonContent = buttonContentWith id

-- | 'buttonContent' returning its 'Response'.
{-# INLINE buttonContent' #-}
buttonContent' :: NanoUI () -> NanoUI Response
buttonContent' = buttonContentWith' id

-- | 'buttonContent' with a layout modifier.
--
-- > whenM (buttonContentWith (gap 4 . fixedW 160) (spinner >> label "Saving")) cancel
{-# INLINE buttonContentWith #-}
buttonContentWith :: (Layout -> Layout) -> NanoUI () -> NanoUI Bool
buttonContentWith f content = respClicked <$> buttonContentWith' f content

-- | 'buttonContentWith' returning its 'Response'.
buttonContentWith' :: (Layout -> Layout) -> NanoUI () -> NanoUI Response
buttonContentWith' f = buttonContentStyled 0 0 (f defaultLayout)

-- | A content button with style index @styleIx@ and the extra button
-- @flags@, as a table's header passes them.
buttonContentStyled :: Float -> Int -> Layout -> NanoUI () -> NanoUI Response
buttonContentStyled styleIx flags lay content = do
  resp <- buttonStyledEx True "" styleIx lay (flags .|. buttonFlagContent)
  theme <- uiTheme
  -- The content is the button's one child, which lays out and paints as an
  -- adornment beside an empty label does.
  _ <- withWidgetChildren (respId resp) (adornmentRow lay (labelColor lay theme) content)
  pure resp

-- | A row of adjacent buttons sharing internal 1-pixel borders with outer-only
-- corner rounding. Works with plain buttons, icon buttons, toggle buttons,
-- content buttons, and selects.
--
-- > buttonGroup $ do
-- >   whenM (button "Left") alignL
-- >   whenM (button "Center") alignC
-- >   whenM (button "Right") alignR
{-# INLINE buttonGroup #-}
buttonGroup :: NanoUI a -> NanoUI a
buttonGroup = buttonGroupWith id

-- | 'buttonGroup' with a layout modifier. Defaults to a tight row with zero
-- gap; override 'layoutDirection' to @Column@ for a vertical connected group.
buttonGroupWith :: (Layout -> Layout) -> NanoUI a -> NanoUI a
buttonGroupWith f child = do
  base <- askDefaultLayout
  let lay = f (tight . gap 0 $ base {layoutDirection = Row})
  idx <- withContext $ \ctx -> do
    parent <- currentParent ctx
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeContainer parent lay
    idx <$ setStyleIdx (ctxNodeArena ctx) idx containerFlagButtonGroup
  res <- withContainerNode True idx child
  -- Mix each direct child's 2-bit (isFirst, isLast) cap mask into its paint
  -- hash so adding or removing a sibling repaints the neighbour's corner
  -- rounding and internal seam on partial-damage frames.
  withContext $ \ctx -> do
    let na = ctxNodeArena ctx
    fc <- getFirstChild na idx
    let go ci = when (ci >= 0) $ do
          ns <- getNextSibling na ci
          let !mask = (if ns < 0 then 1 else 0) .|. (if ci == fc then 2 else 0 :: Word64)
          mixNodePaintHash na ci (0x424700 .|. mask)
          go ns
    go fc
  pure res
