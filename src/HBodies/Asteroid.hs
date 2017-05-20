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

-- | Contains code for controlling the asteroids.
module HBodies.Asteroid
    (
      -- * The types used by the asteroid.
      State(getPosition)

      -- * Functions for manipulating the state.
    , newRandom
    , update

      -- * Functions for rendering the asteroid.
    , render
    ) where

import Control.Monad (forM, forM_, when)
import qualified Graphics.Rendering.OpenGL.GL as GL
import HBodies.Asteroid.State
import qualified HBodies.Game.State as GameState
import qualified HBodies.Geometry as Geometry
import qualified HBodies.GLUtils as GLUtils
import qualified HBodies.Player.State as PlayerState
import qualified HBodies.Time as Time
import qualified System.Random as Random

-- | Creates a new asteroid with random parameters. The function uses the random
-- generator from the state update monad.
newRandom :: (Double, Double)
          -- ^ The range for the position of the asteroid on the X axis.
          -> (Double, Double)
          -- ^ The range for the position of the asteroid on the Y axis.
          -> (Double, Double)
          -- ^ The range for the velocity of the asteroid on the X axis.
          -> (Double, Double)
          -- ^ The range for the velocity of the asteroid on the Y axis.
          -> (Double, Double)
          -- ^ The range for the rotation velocity of the asteroid.
          -> (Double, Double)
          -- ^ The range for the radius of the asteroid.
          -> (Int, Int)
          -- ^ The range for the number of vertices of the asteroid.
          -> (Double, Double)
          -- ^ The range for the health of the asteroid.
          -> GameState.Update State
          -- ^ The new asteroid.
newRandom x_range y_range radius_range dx_range dy_range drotation_range
          num_vertices_range health_range = do
    x <- GameState.randomR x_range
    y <- GameState.randomR y_range
    dx <- GameState.randomR dx_range
    dy <- GameState.randomR dy_range
    dr <- GameState.randomR drotation_range
    num_vertices <- GameState.randomR num_vertices_range
    health <- GameState.randomR health_range
    let angle_piece = Geometry.two_pi / (fromIntegral num_vertices)
    vertices <- forM [1..num_vertices] $ \i -> do
        radius <- GameState.randomR radius_range
        let angle = (fromIntegral i) * angle_piece
            x = radius * (cos angle)
            y = radius * (sin angle)
        return$ GLUtils.vertex2D x y
    return$ State
        { getPosition = Geometry.position x y 0.0
        , getVelocity = Geometry.velocity dx dy dr
        , getVertices = vertices
        , getHealth = health
        , getRadius = snd radius_range
        }

-- | Renders the asteroid.
render :: State -> IO ()
render asteroid = do
    GL.preservingMatrix$ GL.preservingAttrib [GL.AllServerAttributes]$ do
        GLUtils.setUpMatrixFromPosition$ getPosition asteroid
        GL.renderPrimitive GL.LineLoop $do
            GL.color$ GLUtils.color3D 0.7 0.7 0.7
            forM_ (getVertices asteroid) GL.vertex

-- | Updates the state of the given asteroid. Computes the new state and adds
-- the updated asteroid to the monad state.
update :: State
       -- ^ The original state of the asteroid.
       -> GameState.Update ()
update asteroid = do
    -- TODO(ondrasej): Move the asteroid + detect collisions with the player.
    player <- GameState.player
    duration <- GameState.duration
    let sphere = collisionSphere asteroid
    when (PlayerState.isCollision sphere player) $do
        GameState.addPlayerDamage 10.0
    let old_position = getPosition asteroid
        old_velocity = getVelocity asteroid
        raw_position =
            Geometry.updatePosition duration old_position old_velocity
        new_position =
            Geometry.boundedPosition (Geometry.keepRotation) raw_position 
        new_velocity = Geometry.boundedVelocity raw_position old_velocity
    GameState.addUpdatedAsteroid$ asteroid { getPosition = new_position
                                           , getVelocity = new_velocity }
