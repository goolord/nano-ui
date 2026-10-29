-- | Snippets the tutorials show beside their example programs
-- (@book/src/tutorials/@).
module Book.Tutorials where

import Control.Exception (bracket)
import Data.Text (Text)

-- ANCHOR: bracket
job :: Text -> IO [Text]
job q = bracket (connect info) disconnect $ \conn -> runQuery conn q
-- ANCHOR_END: bracket

-- Stand-ins for application code the snippets refer to.

data Connection = Connection

info :: ()
info = ()

connect :: () -> IO Connection
connect _ = pure Connection

disconnect :: Connection -> IO ()
disconnect _ = pure ()

runQuery :: Connection -> Text -> IO [Text]
runQuery _ q = pure [q]
