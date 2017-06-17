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

module HBodies.Bullet
    (
      -- * Data types used by the bullets.
      State(getPosition)

      -- * Functions for creating bullets.
    , new

      -- * Functions for working with the state during the game.
    , render
    , update
    ) where

import Control.Monad (unless)
import qualified Data.Foldable as Foldable
import qualified Graphics.Rendering.OpenGL.GL as GL
import HBodies.Bullet.State
import qualified HBodies.Asteroid.State as AsteroidState
import qualified HBodies.Game.Params as Params
import qualified HBodies.Game.State as GameState
import qualified HBodies.Geometry as Geometry
import HBodies.Geometry ((+.))
import qualified HBodies.GLUtils as GLUtils
import qualified HBodies.Player.State as PlayerState
import qualified HBodies.Time as Time

-- | Creates a new bullet for the given player object and the current in-game
-- time.
new :: PlayerState.State
    -- ^ The player object for which the bullet is created.
    -> Time.Time
    -- ^ The current time in the game.
    -> State
new player current_time = State
    { getPosition = player_position
    , getDirection = player_direction +. bullet_direction
    , getEndTime = Time.add current_time Params.bullet_lifespan }
  where
    bullet_direction = Geometry.directionRadial angle Params.bullet_speed 0.0
    player_position = PlayerState.getPosition player
    player_direction = PlayerState.getDirection player
    angle = Geometry.getRotation player_position

-- | Renders the bullet.
render :: State
       -- ^ The bullet to be rendered.
       -> IO ()
render bullet = do
    GL.preservingAttrib [GL.AllServerAttributes] $do
        GL.preservingMatrix $do
            GLUtils.setUpMatrixFromPosition$ getPosition bullet
            GL.renderPrimitive GL.LineLoop $do
                GL.color$ GLUtils.color3D 1.0 1.0 1.0
                GL.vertex$ GLUtils.vertex2D 1.0 0.0
                GL.vertex$ GLUtils.vertex2D (-0.7) (-0.3)
                GL.vertex$ GLUtils.vertex2D 0.7 (-0.3)

-- | Updates the bullet.
update :: State
       -- ^ The original state of the bullet.
       -> GameState.Update ()
update bullet = do
    duration <- GameState.duration
    current_time <- GameState.currentFrameTime
    asteroids <- GameState.asteroids
    let expired = current_time > getEndTime bullet
        sphere = (getPosition bullet, Params.bullet_radius)
        collision = Foldable.find (AsteroidState.isCollision sphere) asteroids
    case collision of
        Just asteroid -> do
            -- TODO(ondrasej): Add damage to the asteroid.
            return ()
        Nothing -> unless expired $do
            let position = getPosition bullet
                direction = getDirection bullet
                new_position =
                    Geometry.updatePosition duration position direction
            GameState.addUpdatedBullet$ bullet { getPosition = new_position }
