{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Gloss.Internals.Interface.Interact
        (interactWithBackend, MonadGloss)
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Rendering
import Graphics.Gloss.Internals.Interface.Event
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.ViewState.Reshape
import qualified Graphics.Gloss.Internals.Interface.Callback as Callback
import Data.IORef
import System.Mem
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Bifunctor

-- type M r world err = StateT world (ReaderT r (ExceptT err IO))
-- example :: forall r world err.
--         GLUTState
--         -> Display
--         -> Color
--         -> (((((), r), world) -> IO Picture)
--                   -> (Event -> M r world err ())
--                   -> (Either err () -> IO ())
--                   -> (Controller -> IO ())
--                   -> M r world err ())
-- example = interactWithBackend @GLUTState @(M r world err)

-- state-y, error-y things
class MonadIO m => MonadGloss m world err | m -> err, m -> world where
        runUpdate :: (err -> IO a) -> m a -> world -> IO (world, a)
        initWorld :: m world
instance MonadGloss IO () () where
        runUpdate _h x () = ((),) <$> x
        initWorld = pure ()
instance (MonadGloss m world err0) => MonadGloss (ExceptT err m) world (Either err err0) where
        runUpdate :: (Either err err0 -> IO a) -> ExceptT err m a -> (world -> IO (world, a))
        runUpdate h x s = runUpdate (h . Right) (runExceptT x >>= either (liftIO . h . Left) pure) s
        initWorld = lift initWorld
instance (MonadGloss m world err) => MonadGloss (ReaderT r m) (world, r) err where
        runUpdate :: (err -> IO a) -> ReaderT r m a -> (world, r) -> IO ((world, r), a)
        runUpdate h x (s, r) = first (, r) <$> runUpdate h (runReaderT x r) s
        initWorld = (,) <$> lift initWorld <*> ask
instance (MonadGloss m world err) => MonadGloss (StateT s m) (world, s) err where
        runUpdate :: (err -> IO a) -> StateT s m a -> (world, s) -> IO ((world, s), a)
        runUpdate h x (world0, s) = (\(x',(y,z)) -> ((x',z),y)) <$>
                runUpdate (fmap (, s) . h) (runStateT x s) world0
        initWorld = (,) <$> lift initWorld <*> get

interactWithBackend
        :: Backend a
        => MonadGloss m world err
        => a                            -- ^ Initial state of the backend.
        -> Display                      -- ^ Display config.
        -> Color                        -- ^ Background color.
        -> (world -> IO (Picture, String))        -- ^ A function to produce the current picture.
        -> (Event -> m ()) -- ^ A function to handle input events.
        -> (err -> IO ()) -- ^ A function to handle errors.
        -> (Controller -> IO ())        -- ^ Eat the controller
        -> m ()

interactWithBackend
        backend displayMode background
        worldToPicture
        worldHandleEvent
        handleError
        eatController

 =  do
        worldStart <- initWorld
        viewSR          <- liftIO $ newIORef viewStateInit
        worldSR         <- liftIO $ newIORef worldStart
        renderS         <- liftIO initState
        renderSR        <- liftIO $ newIORef renderS

        let displayFun backendRef = do
                world      <- readIORef worldSR
                (picture, windowTitle)    <- worldToPicture world

                renderS'      <- readIORef renderSR
                viewState     <- readIORef viewSR
                let viewPort  =  viewStateViewPort viewState

                windowSize <- getWindowDimensions backendRef

                displayPicture
                        windowSize
                        background
                        renderS'
                        (viewPortScale viewPort)
                        (applyViewPortToPicture viewPort picture)

                setWindowTitle backendRef windowTitle

                -- perform GC every frame to try and avoid long pauses
                performGC

        let worldHandleEvent' = (fmap fst .) . runUpdate handleError . worldHandleEvent
        let callbacks
             =  [ Callback.Display displayFun

                -- Viewport control with mouse
                , callback_keyMouse worldSR viewSR worldHandleEvent'
                , callback_motion   worldSR worldHandleEvent'
                , callback_reshape  worldSR worldHandleEvent' ]


        -- When we create the window we can pass a function to get a
        -- reference to the backend state. Using this we make a controller
        -- so the client can control the window asynchronously.
        liftIO $ createWindow backend displayMode background callbacks
         $ \  backendRef
           -> eatController
                $ Controller
                { controllerSetRedraw
                   = do postRedisplay backendRef

                , controllerModifyViewPort
                   = \modViewPort
                     -> do viewState       <- readIORef viewSR
                           port'           <- modViewPort $ viewStateViewPort viewState
                           let viewState'  =  viewState { viewStateViewPort = port' }
                           writeIORef viewSR viewState'
                           postRedisplay backendRef
                }


-- | Callback for KeyMouse events.
callback_keyMouse
        :: IORef world                  -- ^ ref to world state
        -> IORef ViewState
        -> (Event -> world -> IO world) -- ^ fn to handle input events
        -> Callback

callback_keyMouse worldRef viewRef eventFn
        = KeyMouse (handle_keyMouse worldRef viewRef eventFn)


handle_keyMouse
        :: IORef a
        -> t
        -> (Event -> a -> IO a)
        -> KeyboardMouseCallback

handle_keyMouse worldRef _ eventFn backendRef key keyState keyMods pos
 = do   ev         <- keyMouseEvent backendRef key keyState keyMods pos
        world      <- readIORef worldRef
        world'     <- eventFn ev world
        writeIORef worldRef world'
        postRedisplay backendRef


-- | Callback for Motion events.
callback_motion
        :: IORef world                  -- ^ ref to world state
        -> (Event -> world -> IO world) -- ^ fn to handle input events
        -> Callback

callback_motion worldRef eventFn
        = Motion (handle_motion worldRef eventFn)


handle_motion
        :: IORef a
        -> (Event -> a -> IO a)
        -> MotionCallback

handle_motion worldRef eventFn backendRef pos
 = do   ev       <- motionEvent backendRef pos
        world    <- readIORef worldRef
        world'   <- eventFn ev world
        writeIORef worldRef world'
        postRedisplay backendRef


-- | Callback for Handle reshape event.
callback_reshape
        :: IORef world
        -> (Event -> world -> IO world)
        -> Callback

callback_reshape worldRef eventFN
        = Reshape (handle_reshape worldRef eventFN)


handle_reshape
        :: IORef world
        -> (Event -> world -> IO world)
        -> ReshapeCallback
handle_reshape worldRef eventFn backendRef (width,height)
 = do   world  <- readIORef worldRef
        world' <- eventFn (EventResize (width, height)) world
        writeIORef worldRef world'
        viewState_reshape backendRef (width, height)
        postRedisplay backendRef
