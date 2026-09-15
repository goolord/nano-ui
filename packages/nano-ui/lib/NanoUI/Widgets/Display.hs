{-# LANGUAGE OverloadedStrings #-}

-- | Display helpers: styled labels, key/value rows, cards, toolbars, images
-- and colour boxes.
module NanoUI.Widgets.Display
  ( label_
  , heading
  , muted
  , mono
  , danger
  , bold
  , italic
  , underline
  , kv
  , kvMono
  , kvBlock
  , card
  , toolbar
  , image
  , image_
  , box
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, nextId)
import NanoUI.Style
  ( Layout (..)
  , alignEnd
  , alignMid
  , defaultLayout
  , fillW
  , fontBold
  , fontDanger
  , fontItalic
  , fontMedium
  , fontMono
  , fontMuted
  , fontUnderline
  , gap
  , minW
  , padXY
  , tight
  )
import NanoUI.Types (Color (..), ImageId (..), colorToWord32)
import NanoUI.WidgetText (intValueText)
import NanoUI.Widgets.Layout (label, labelEx, labelWith, panelWith, row', rowWith)
import NanoUI.Widgets.Node (Response, addWidget, addWidgetStyled)

{-# INLINE label_ #-}
label_ :: Ui :> es => Text -> Eff es ()
label_ txt = void (label txt)

heading :: Ui :> es => Text -> Eff es ()
heading txt = void (labelWith (tight . fontMedium) txt)

muted :: Ui :> es => Text -> Eff es ()
muted txt = void (labelWith (fillW . fontMuted) txt)

mono :: Ui :> es => Text -> Eff es ()
mono txt = void (labelWith fontMono txt)

danger :: Ui :> es => Text -> Eff es ()
danger txt = void (labelWith (fillW . fontDanger) txt)

bold :: Ui :> es => Text -> Eff es ()
bold txt = void (labelWith fontBold txt)

italic :: Ui :> es => Text -> Eff es ()
italic txt = void (labelWith fontItalic txt)

underline :: Ui :> es => Text -> Eff es ()
underline txt = void (labelWith fontUnderline txt)

-- | Key/value row: a muted key on the left, the value right-aligned. Trailing
-- whitespace in the value is dropped.
kv :: Ui :> es => Text -> Text -> Eff es ()
kv k v =
  void $
    row' (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
      void (labelEx (fontMuted . tight . minW 88 $ defaultLayout) k)
      void (labelEx (tight . fillW . alignEnd $ defaultLayout) (T.stripEnd v))

-- | Key/value row with a monospace value.
kvMono :: Ui :> es => Text -> Text -> Eff es ()
kvMono k v =
  void $
    row' (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
      void (labelEx (tight . minW 88 $ defaultLayout) k)
      void (labelEx (tight . fillW . alignEnd . fontMono $ defaultLayout) (T.stripEnd v))

kvBlock :: (Foldable f, Ui :> es) => f (Text, Text) -> Eff es ()
kvBlock rows =
  let maxK = foldl' (\acc (k, _) -> max acc (T.length k)) 0 rows
      padK k = T.justifyLeft maxK ' ' k
   in void $
        labelEx
          (tight . gap 0 . fontMono $ defaultLayout)
          (T.unlines (foldr (\(k, v) rest -> (padK k <> "  " <> v) : rest) [] rows))

card :: Ui :> es => Eff es a -> Eff es a
card = panelWith (minW 300 . padXY 12 10 . gap 8 . fillW)

toolbar :: Ui :> es => Eff es a -> Eff es a
toolbar = rowWith (tight . gap 8 . alignMid . fillW)

image :: Ui :> es => Layout -> ImageId -> Eff es Response
image layout (ImageId tid) = do
  wid <- nextId
  let
    stored = if tid <= 0 then T.empty else intValueText tid
  addWidget wid NodeImage stored 0 layout

{-# INLINE image_ #-}
image_ :: Ui :> es => Layout -> ImageId -> Eff es ()
image_ layout iid = void (image layout iid)

box :: Ui :> es => Layout -> Color -> Eff es ()
box layout col = do
  wid <- nextId
  void
    ( addWidgetStyled
        wid
        NodeBox
        T.empty
        0
        layout
        (fromIntegral (colorToWord32 col))
        Nothing
    )
