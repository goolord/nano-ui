{-# LANGUAGE OverloadedStrings #-}

-- | Display helpers: styled labels, key/value rows, cards, toolbars, images
-- and colour boxes.
module NanoUI.Widgets.Display
  ( heading
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
import NanoUI.Widgets.Layout (labelEx, labelWith, panelWith, row', rowWith)
import NanoUI.Widgets.Node (addWidget, addWidgetStyled)

heading :: Ui :> es => Text -> Eff es ()
heading = labelWith (tight . fontMedium)

muted :: Ui :> es => Text -> Eff es ()
muted = labelWith (fillW . fontMuted)

mono :: Ui :> es => Text -> Eff es ()
mono = labelWith fontMono

danger :: Ui :> es => Text -> Eff es ()
danger = labelWith (fillW . fontDanger)

bold :: Ui :> es => Text -> Eff es ()
bold = labelWith fontBold

italic :: Ui :> es => Text -> Eff es ()
italic = labelWith fontItalic

underline :: Ui :> es => Text -> Eff es ()
underline = labelWith fontUnderline

-- | Key/value row: a muted key on the left, the value right-aligned. Trailing
-- whitespace in the value is dropped.
kv :: Ui :> es => Text -> Text -> Eff es ()
kv k v =
  row' (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
    void (labelEx (fontMuted . tight . minW 88 $ defaultLayout) k)
    void (labelEx (tight . fillW . alignEnd $ defaultLayout) (T.stripEnd v))

-- | Key/value row with a monospace value.
kvMono :: Ui :> es => Text -> Text -> Eff es ()
kvMono k v =
  row' (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
    void (labelEx (tight . minW 88 $ defaultLayout) k)
    void (labelEx (tight . fillW . alignEnd . fontMono $ defaultLayout) (T.stripEnd v))

-- | Key/value pairs as one monospace block with the keys padded to a column.
kvBlock :: (Foldable f, Ui :> es) => f (Text, Text) -> Eff es ()
kvBlock rows =
  let maxK = foldl' (\acc (k, _) -> max acc (T.length k)) 0 rows
      padK k = T.justifyLeft maxK ' ' k
   in void $
        labelEx
          (tight . gap 0 . fontMono $ defaultLayout)
          (T.concat (foldr (\(k, v) rest -> padK k : "  " : v : "\n" : rest) [] rows))

card :: Ui :> es => Eff es a -> Eff es a
card = panelWith (minW 300 . padXY 12 10 . gap 8 . fillW)

toolbar :: Ui :> es => Eff es a -> Eff es a
toolbar = rowWith (tight . gap 8 . alignMid . fillW)

-- | An image registered with the host, sized by the layout modifier.
image :: Ui :> es => (Layout -> Layout) -> ImageId -> Eff es ()
image f (ImageId tid) = do
  wid <- nextId
  let
    stored = if tid <= 0 then T.empty else intValueText tid
  void (addWidget wid NodeImage stored 0 (f defaultLayout))

-- | A solid rectangle sized by the layout modifier.
box :: Ui :> es => (Layout -> Layout) -> Color -> Eff es ()
box f col = do
  wid <- nextId
  void
    ( addWidgetStyled
        wid
        NodeBox
        T.empty
        0
        (f defaultLayout)
        (fromIntegral (colorToWord32 col))
        Nothing
    )
