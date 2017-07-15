-- Copyright 2017 Ondrej Sykora
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module HBodies.Application
    (
      -- * The @ApplicationSettings@ type
      ApplicationSettings(..)

      -- * The @Application@ type
    , Application

    , runEventLoop
    , withApplication

      -- * The application @Action type
    , Action
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Foreign.C.Types (CInt)
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified HBodies.Game as Game
import qualified HBodies.Inputs as Inputs
import qualified HBodies.GLUtils as GLUtils
import qualified HBodies.SDLUtils as SDLUtils
import qualified HBodies.Time as Time
import qualified Linear.V2 as V2
import qualified SDL

-- | Contains settings for the application and the main application window.
data ApplicationSettings = ApplicationSettings
    { -- | The width of the application window (in pixels).
      windowWidth :: !Int
      -- | The height of the application window (in pixels).
    , windowHeight :: !Int
      -- | The title of the application window.
    , windowTitle :: String
    }

-- | Specifies the current state of the application.
data ApplicationState = Active   -- ^ The application is active.
                      | Paused   -- ^ The application is deactivated.
                      | Restart  -- ^ The game should restart from the initial
                                 --   state.
                      | Quit     -- ^ The application is finishing, this is the
                                 --   last iteration of the application event
                                 --   loop.
                      deriving (Eq, Show)

-- | Contains the application data.
data Application = App
    { -- | The application settings.
      getApplicationSettings :: ApplicationSettings
      -- | The application window.
    , getMainWindow :: !SDL.Window
      -- | The renderer in the application window.
    , getRenderer :: !SDL.Renderer
      -- The state of the application event loop.
    , eventLoopState :: IORef.IORef ApplicationState
      -- The list of screens in the application.
    , gameState :: IORef.IORef Game.State
    }

-- | The action requested by the current screen.
data Action
      -- | No special action requested by the screen.
    = NoAction
      -- | The screen requested ending the application.
    | QuitAction
      -- | The screen requested switching to another screen.
    | SwitchToScreen String

-- | Creates a new instance of the application and the application main window.
createApplication :: ApplicationSettings 
                  -- ^ The parameters of the application.
                  -> IO Application
createApplication app_settings = do
    SDL.initializeAll
    window <- SDL.createWindow window_title window_config
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    loop_state <- IORef.newIORef Active
    game_state <- IORef.newIORef Game.new
    return App { getApplicationSettings = app_settings
               , getMainWindow = window
               , getRenderer = renderer
               , eventLoopState = loop_state
               , gameState = game_state }
  where
    window_config = SDL.defaultWindow { SDL.windowOpenGL = Just gl_config
                                      , SDL.windowInitialSize = window_size }
    window_size = V2.V2 (fromIntegral$ windowWidth app_settings)
                        (fromIntegral$ windowHeight app_settings)
    window_title = Text.pack$ windowTitle app_settings
    gl_config = SDL.defaultOpenGL

-- | Closes an application. Destroys the SDL objects.
destroyApplication :: Application -> IO ()
destroyApplication application = do
    SDL.destroyRenderer (getRenderer application)
    SDL.destroyWindow (getMainWindow application)
    SDL.quit

-- | Quits the event loop of the application.
quitEventLoop :: Application
              -> IO ()
quitEventLoop application = do
    IORef.writeIORef (eventLoopState application) Quit

-- | Pauses the event loop of the application. The event loop will still
-- process events, but it will wait instead of polling, and the game will not
-- be updated.
pauseEventLoop :: Application
               -> IO ()
pauseEventLoop application = do
    IORef.writeIORef (eventLoopState application) Paused

-- | Restarts the game.
restartGame :: Application -> IO ()
restartGame application = do
    IORef.writeIORef (eventLoopState application) Restart

-- | Resumes the event loop of the application. The event loop will poll rather
-- than wait for events, and it will update the game loop periodically.
resumeEventLoop :: Application
                -> IO ()
resumeEventLoop application = do
    IORef.writeIORef (eventLoopState application) Active

-- | Renders the application.
render :: Application
       -- ^ The application to be rendered.
       -> IO ()
render application = do
    -- Set up the viewport.
    let app_settings = getApplicationSettings application
        width = windowWidth app_settings
        height = windowHeight app_settings
    GL.viewport $= (GLUtils.glPosition 0 0, GLUtils.glSize width height)

    -- Set up projection matrix.
    let half_width = fromIntegral width / 2.0
        half_height = fromIntegral height / 2.0
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GLU.ortho2D (-half_width) (half_width) (-half_height) (half_height)

    -- Set up model view matrix.
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    -- Clear the screen.
    GL.clearColor $= GL.Color4 0.0 0.0 0.0 (0.0 :: GL.GLfloat)
    GL.clear [GL.ColorBuffer]

    game_state <- IORef.readIORef$ gameState application
    Game.render game_state
    
    let renderer = getRenderer application
    SDL.present renderer

-- | Runs the event loop of the application. Blocks until the application loop
-- is finished.
runEventLoop :: Application
             -> IO ()
runEventLoop application = do
    start_time <- Time.sdlTime
    runEventLoop' application start_time
  where
    runEventLoop' application last_update = do
        -- Read the next event. We use frame limiting to avoid too many frames
        -- per second.
        initial_state <- IORef.readIORef$ eventLoopState application
        maybe_event <- case initial_state of
            Active -> pollEventWithFpsLimit last_update
            Paused -> Just <$> SDL.waitEvent
            Quit   -> return Nothing

        -- Parse the current event and pause/unpause/quit the game if needed.
        case maybe_event of
            Just event -> do
                when (SDLUtils.isQuitEvent event) $do
                    quitEventLoop application
                when (SDLUtils.isDeactivateEvent event) $do
                    pauseEventLoop application
                when (SDLUtils.isRestartEvent event) $do
                    restartGame application
                when (SDLUtils.isActivateEvent event) $do
                    resumeEventLoop application
            Nothing -> return ()

        -- Re-read the state of the application in case there was a quit event.
        updated_state <- IORef.readIORef$ eventLoopState application
        current_time <- Time.sdlTime
        let since_last_frame = Time.diff current_time last_update
        case (initial_state, updated_state) of
            (Active, Active) -> do
                -- The application is active, we need to update the game and
                -- re-render it for each frame.
                inputs <- Inputs.readInputs
                old_state <- IORef.readIORef$ gameState application
                let (new_state, since_this_frame, was_updated) =
                        updateGameState since_last_frame inputs old_state
                IORef.writeIORef (gameState application) new_state
                when was_updated $do
                    -- Only render the game after an update.
                    -- TODO(ondrasej): This is ugly, and more complex than
                    -- necessary. Refactor this code so that it does not
                    -- re-evaluate the next frame condition.
                    render application
                let last_update_time = Time.add current_time (-since_this_frame)
                runEventLoop' application last_update_time
            (_, Active) -> do
                -- The application was just activated. We need to update time
                -- first - there was a SDL.waitEvent in between last_update and
                -- current_time and the time delta could have been very long.
                runEventLoop' application current_time
            (_, Restart) -> do
                IORef.writeIORef (gameState application) Game.new
                IORef.writeIORef (eventLoopState application) Active
                runEventLoop' application current_time
            (_, Paused) -> do
                render application
                runEventLoop' application current_time
            (_, Quit) -> return ()
    -- An adaptive polling/waiting for the next event. If the next update is
    -- more than one milisecond away, the function waits for the next event with
    -- a timeout that leaves enough time for processing the next event.
    -- Otherwise, it polls for the event.
    pollEventWithFpsLimit last_update = do
        current_time <-  Time.sdlTime
        let since_last_frame = Time.diff current_time last_update
            until_next_frame = max Time.zeroDuration
                                   (Time.frameDuration - since_last_frame)
            seconds_to_wait = Time.secondsFromDuration until_next_frame
            ms_to_wait = ceiling$ 1000.0 * seconds_to_wait :: CInt
        if ms_to_wait < 1
            then SDL.pollEvent
            else SDL.waitEventTimeout ms_to_wait
    updateGameState = updateGameState' False
    updateGameState' :: Bool
                     -> Time.Duration
                     -> Inputs.State
                     -> Game.State
                     -> (Game.State, Time.Duration, Bool)
    updateGameState' was_updated delta_time inputs old_state =
        if delta_time < Time.frameDuration
        then (old_state, delta_time, was_updated)
        else updateGameState' True
                              (delta_time - Time.frameDuration)
                              inputs
                              (Game.update Time.frameDuration inputs old_state)

-- | Creates an application based on the provided application settings and runs
-- the provided action with the application object.
withApplication :: ApplicationSettings
                -- ^ The application settings of the new application.
                -> (Application -> IO a)
                -- ^ A function called on the application object created by this
                -- function.
                -> IO a
                -- ^ The value returned by the function 'action'.
withApplication app_settings action = do
    bracket (createApplication app_settings)
            (destroyApplication)
            (action)
