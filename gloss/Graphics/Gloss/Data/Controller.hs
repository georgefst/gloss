
module Graphics.Gloss.Data.Controller
        ( Controller    (..))
where
import Graphics.Gloss.Data.ViewPort


-- | Functions to asynchronously control a `Gloss` display.
data Controller
        = Controller
        { -- | Indicate that we want the picture to be redrawn.
          controllerSetRedraw       :: IO ()

          -- | Modify the current viewport,
          --   also forcing it to refresh.
        , controllerModifyViewPort  :: (ViewPort -> IO ViewPort) -> IO () }
