-- | A 'NanoUI' view is an @effectful@ computation in the row 'NanoUIEs'.
-- Nothing else in nano-ui needs this module; it is for applications that
-- already use @effectful@ and want their views to share effects with the
-- rest of the program.
--
-- A view written in a larger row declares widgets with 'embedNanoUI', and
-- hands a container widget a body that uses its other effects with
-- 'withRunInNanoUI':
--
-- > sidebar :: (Ui :> es, Reader Config :> es) => Eff es ()
-- > sidebar = withRunInNanoUI $ \run -> column $ do
-- >   title <- run (asks configTitle)
-- >   heading title
--
-- 'runFrameEff' runs a frame of such a view, given a runner for the effects
-- other than 'Ui'.
module NanoUI.Effectful
  ( -- * The view type
    NanoUI (..)
  , NanoUIEs
  , Ui

    -- * Views in other effect rows
  , embedNanoUI
  , withRunInNanoUI
  , runUi
  , runFrameEff
  )
where

import NanoUI.Internal.Frame (runFrameEff)
import NanoUI.Internal.Monad (NanoUI (..), NanoUIEs, Ui, embedNanoUI, runUi, withRunInNanoUI)
