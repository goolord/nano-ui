{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import NanoUI
import qualified Data.ByteString as BS
import NanoUI.Backend.Sdl (runSdlAppWith)
import qualified Data.Text as T

-- Stay below panelPaintPad so the page column does not fill.
pagePad :: Padding
pagePad =
  let p = panelPaintPad - 2
   in Padding p p p p

cardPad :: Padding
cardPad = Padding 16 16 14 14

headerPad :: Padding
headerPad = Padding 16 16 12 12

pageGap :: Float
pageGap = 8

sectionGap :: Float
sectionGap = 8

pageLayout :: Layout
pageLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutHeight = Grow 1
    , layoutPadding = pagePad
    , layoutGap = pageGap
    }

headerCard :: Layout
headerCard =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutPadding = headerPad
    , layoutGap = 8
    }

cardLayout :: Layout
cardLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutPadding = cardPad
    , layoutGap = sectionGap
    , layoutMinW = 280
    }

headerRow :: Layout
headerRow =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutAlignY = AlignMiddle
    , layoutGap = 16
    }

scrollFill :: Layout
scrollFill =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutHeight = Grow 1
    , layoutPadding = Padding 0 0 0 0
    , layoutGap = 0
    }

bodyRow :: Layout
bodyRow =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutGap = pageGap
    , layoutWrap = True
    }

pageInner :: Layout
pageInner =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutPadding = Padding 0 0 0 0
    , layoutGap = pageGap
    }

listScroll :: Layout
listScroll =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutHeight = Fixed 136
    , layoutPadding = Padding 8 8 8 8
    }

stackCol :: Layout
stackCol =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutPadding = Padding 0 0 0 0
    , layoutGap = sectionGap
    }

thumbLayout :: Layout
thumbLayout =
  defaultLayout
    { layoutWidth = Fixed 80
    , layoutHeight = Fixed 80
    }

-- Grow so assigned width remasures and wraps inside the card.
wrapCopy :: Layout
wrapCopy =
  defaultLayout
    { layoutWidth = Grow 1
    }

main :: IO ()
main = do
  ctx <- newSdlContext
  lastClick <- newIORef ("" :: String)
  aboutOpen <- newIORef False
  ok1 <- registerImage ctx (ImageId 1) 32 32 swatchPixels
  ok2 <- registerImage ctx (ImageId 2) 32 32 checkerPixels
  ok3 <- registerImage ctx (ImageId 3) 32 32 stripePixels
  unless (ok1 && ok2 && ok3) $ fail "registerImage failed"
  runSdlAppWith
    ctx
    (\_ -> pure ())
    (\inp -> KeyEscape `elem` inputKeys inp)
    $
    column pageLayout $ do
      (_, _) <-
        scrollArea scrollFill $
          column pageInner $ do
            panel headerCard $
              row headerRow $ do
                _ <- label "nano-ui SDL3 demo"
                _ <- spacer (Grow 1) Fit
                row
                  defaultLayout
                    { layoutPadding = Padding 0 0 0 0
                    , layoutAlignY = AlignMiddle
                    , layoutGap = 8
                    }
                  $ do
                    ok <- button "OK"
                    when (respClicked ok) $
                      liftIO $ writeIORef lastClick "OK"
                    cancel <- button "Cancel"
                    when (respClicked cancel) $
                      liftIO $ writeIORef lastClick "Cancel"
                    about <- button "About"
                    when (respClicked about) $ liftIO $ writeIORef aboutOpen True
                    pure ()
            row bodyRow $ do
              panel cardLayout $
                column stackCol $ do
                    _ <- label "Controls"
                    (_, checked) <- checkbox "Feature" False
                    (_, vol) <- slider (defaultLayout {layoutWidth = Grow 1}) "Volume" 0 100 50
                    (_, quality) <- select "Quality" ["Low", "Medium", "High"] 1
                    (_, name) <- textInput "Name" ""
                    _ <- separator
                    _ <- label "List"
                    (_, _) <-
                      scrollArea listScroll $
                        column
                          ( defaultLayout
                              { layoutWidth = Grow 1
                              , layoutPadding = Padding 0 0 0 0
                              , layoutGap = 0
                              }
                          )
                          $ mapM_ (\i -> label (T.pack ("Item " <> show (i :: Int)))) [1 .. 12]
                    click <- liftIO $ readIORef lastClick
                    _ <- separator
                    _ <- label (T.pack ("Feature  " <> if checked then "on" else "off"))
                    _ <- label (T.pack ("Volume   " <> show (round vol :: Int)))
                    _ <- label (T.pack ("Quality  " <> show quality))
                    _ <- label (T.pack ("Name     " <> if null name then "-" else name))
                    _ <- label (T.pack ("Clicked  " <> if null click then "-" else click))
                    pure ()
              panel cardLayout $
                column stackCol $ do
                    _ <- label "Gallery"
                    row
                      defaultLayout
                        { layoutPadding = Padding 0 0 0 0
                        , layoutGap = 12
                        , layoutWrap = True
                        }
                      $ do
                        column (defaultLayout {layoutPadding = Padding 0 0 0 0, layoutGap = 6}) $ do
                          _ <- image thumbLayout (ImageId 1)
                          _ <- label "Swatch"
                          pure ()
                        column (defaultLayout {layoutPadding = Padding 0 0 0 0, layoutGap = 6}) $ do
                          _ <- image thumbLayout (ImageId 2)
                          _ <- label "Checker"
                          pure ()
                        column (defaultLayout {layoutPadding = Padding 0 0 0 0, layoutGap = 6}) $ do
                          _ <- image thumbLayout (ImageId 3)
                          _ <- label "Stripe"
                          pure ()
                    _ <- separator
                    _ <- labelEx wrapCopy "Click widgets or type in Name."
                    _ <- labelEx wrapCopy "Esc closes About, then quits."
                    pure ()
      showAbout <- liftIO (readIORef aboutOpen)
      (aboutResp, _) <-
        modal showAbout "About" $ do
          _ <- labelEx wrapCopy "Immediate-mode GUI for Haskell."
          _ <- labelEx wrapCopy "Esc closes this dialog, then the app."
          row
            defaultLayout
              { layoutWidth = Grow 1
              , layoutGap = 8
              }
            $ do
              _ <- spacer (Grow 1) Fit
              close <- button "Close"
              when (respClicked close) $ liftIO $ writeIORef aboutOpen False
              pure ()
      when (respClicked aboutResp) $ liftIO $ writeIORef aboutOpen False
      pure ()

swatchPixels :: BS.ByteString
swatchPixels =
  BS.pack
    [ chan
    | y <- [0 .. 31] :: [Int]
    , x <- [0 .. 31] :: [Int]
    , chan <-
        [ fromIntegral (x * 255 `div` 31)
        , fromIntegral (y * 255 `div` 31)
        , 180
        , 255
        ]
    ]

checkerPixels :: BS.ByteString
checkerPixels =
  BS.pack
    [ chan
    | y <- [0 .. 31] :: [Int]
    , x <- [0 .. 31] :: [Int]
    , let on = (x `div` 8 + y `div` 8) `mod` 2 == 0
    , chan <-
        if on
          then [240, 200, 80, 255]
          else [40, 50, 70, 255]
    ]

stripePixels :: BS.ByteString
stripePixels =
  BS.pack
    [ chan
    | y <- [0 .. 31] :: [Int]
    , x <- [0 .. 31] :: [Int]
    , let on = (x + y) `mod` 8 < 4
    , chan <-
        if on
          then [80, 200, 220, 255]
          else [20, 30, 90, 255]
    ]
