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

-- | Contains the code for running the game.
module HBodies.Game
    (
      -- * Data types for representing the state of the game.
      State(getLastUpdate, getPlayer)

      -- * Functions for intializing the state of the game.
    , new

      -- * Functions for working with the state during the game.
    , render
    , update
    ) where

import Data.Foldable (forM_)
import qualified Data.Map.Strict as Map
import qualified HBodies.Asteroid as Asteroid
import qualified HBodies.Asteroid.State as AsteroidState
import qualified HBodies.Bullet as Bullet
import qualified HBodies.Game.Params as Params
import HBodies.Game.State
import qualified HBodies.GLUtils as GLUtils
import qualified HBodies.Geometry as Geometry
import qualified HBodies.Inputs as Inputs
import qualified HBodies.Particle as Particle
import qualified HBodies.Player as Player
import qualified HBodies.Time as Time
import qualified System.Random as Random

-- | The initial game state with no asteroids.
empty :: State
empty = GameState
    { getLastUpdate = Time.startTime
    , getPlayer = Player.new
    , getAsteroids = Map.empty
    , getLastBulletTime = Time.infinitePast
    , getBullets = []
    , getStateNextAsteroidId = AsteroidState.firstId
    , getParticles = []
    , getRandomGenerator = Random.mkStdGen Params.random_seed }

-- | The initial state of a game with "randomly" placed asteroids. Note that the
-- function is in fact deterministic and it always returns the same
-- configuration, but the result can be changed by changing the random seed.
new :: State
new = new_state
  where
    update_data = runUpdate (Time.durationSeconds 1.0) Inputs.empty empty $do
        forM_ [1..Params.num_asteroids_on_start] $ \_ -> do
            asteroid <- Asteroid.newRandom Params.asteroid_x_range
                                           Params.asteroid_y_range
                                           Params.asteroid_radius_range
                                           Params.asteroid_dx_range
                                           Params.asteroid_dy_range
                                           Params.asteroid_drotation_range
                                           Params.asteroid_vertices_range
            addUpdatedAsteroid asteroid
    new_state = empty
        { getAsteroids = getUpdatedAsteroids update_data
        , getStateNextAsteroidId = getUpdateNextAsteroidId update_data }

-- | Renders the current state of the world.
render :: State
       -- ^ The state of the world.
       -> IO ()
render state = do
    forM_ (getParticles state) Particle.render
    forM_ (getBullets state) Bullet.render
    forM_ (getAsteroids state) Asteroid.render
    Player.render (getPlayer state)

-- | Updates the state of the game by the given amount of time.
update :: Time.Duration
       -- ^ The amount of time by which the game is updaed.
       -> Inputs.State
       -- ^ The state of the inputs during the update time.
       -> State
       -- ^ The previous state of the game.
       -> State
       -- ^ The new state of the game.
update duration inputs old_state = new_state
  where
    new_state = GameState
        { getAsteroids = getUpdatedAsteroids update_data
        , getStateNextAsteroidId = getUpdateNextAsteroidId update_data
        , getBullets = reverse$ getUpdatedBullets update_data
        , getLastBulletTime = last_bullet_time
        , getLastUpdate = new_frame_time
        , getParticles = reverse$ getUpdatedParticles update_data
        , getPlayer = getUpdatedPlayer update_data
        , getRandomGenerator = getUpdateRandomGenerator update_data }
    update_data = runUpdate duration inputs old_state $do
        forM_ (getBullets old_state) Bullet.update
        forM_ (getAsteroids old_state) Asteroid.update
        forM_ (getParticles old_state) Particle.update
        Player.update
    new_frame_time = Time.add (getLastUpdate old_state) duration
    last_bullet_time = if getBulletFired update_data
                           then new_frame_time
                           else getLastBulletTime old_state
