
-- | Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.IO.Interact
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , interactIO
        , Controller    (..)
        , Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Event
import Graphics.Gloss.Internals.Interface.Interact
import Graphics.Gloss.Internals.Interface.Backend


-- | Open a new window and interact with an infrequently updated picture.
--
--   Similar to `displayIO`, except that you manage your own events.
--
interactIO
        :: MonadGloss m world e
        => Display                      -- ^ Display mode.
        -> Color                        -- ^ Background color.
        -> (world -> IO (Picture, String))        -- ^ A function to produce the current picture.
        -> (Event -> m ()) -- ^ A function to handle input events.
        -> (e -> IO ())
        -> (Controller -> IO ())        -- ^ Callback to take the display controller.
        -> m ()

interactIO
 =      interactWithBackend
                defaultBackendState
