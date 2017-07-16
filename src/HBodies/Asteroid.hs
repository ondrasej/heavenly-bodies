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
import qualified HBodies.Game.Params as Params
import qualified HBodies.Game.State as GameState
import qualified HBodies.Geometry as Geometry
import qualified HBodies.GLUtils as GLUtils
import qualified HBodies.Player.State as PlayerState
import qualified HBodies.Time as Time
import qualified System.Random as Random

-- | Generates a random shape for an asteroid with the given parameters.
randomVertices :: (Int, Int)
               -- ^ The range for the number of vertices of the asteroid.
               -> (Double, Double)
               -- ^ The range for the distance of a vertex from the center of
               -- the asteroid.
               -> GameState.Update [GL.Vertex2 GL.GLdouble]
               -- ^ The vertices of the asteroid.
randomVertices num_vertices_range radius_range = do
    num_vertices <- GameState.randomR num_vertices_range
    let angle_piece = Geometry.two_pi / (fromIntegral num_vertices)
    forM [1..num_vertices] $ \i -> do
        radius <- GameState.randomR radius_range
        let angle = (fromIntegral i) * angle_piece
            x = radius * (cos angle)
            y = radius * (sin angle)
        return$ GLUtils.vertex2D x y

-- | Creates a new asteroid with random parameters. The function uses the random
-- generator from the state update monad.
newRandom :: (Double, Double)
          -- ^ The range for the position of the asteroid on the X axis.
          -> (Double, Double)
          -- ^ The range for the position of the asteroid on the Y axis.
          -> (Double, Double)
          -- ^ The range for the direction of the asteroid on the X axis.
          -> (Double, Double)
          -- ^ The range for the direction of the asteroid on the Y axis.
          -> (Double, Double)
          -- ^ The range for the rotation direction of the asteroid.
          -> (Double, Double)
          -- ^ The range for the radius of the asteroid.
          -> (Int, Int)
          -- ^ The range for the number of vertices of the asteroid.
          -> GameState.Update State
          -- ^ The new asteroid.
newRandom x_range y_range radius_range dx_range dy_range drotation_range
          num_vertices_range = do
    id <- GameState.newAsteroidId
    x <- GameState.randomR x_range
    y <- GameState.randomR y_range
    dx <- GameState.randomR dx_range
    dy <- GameState.randomR dy_range
    dr <- GameState.randomR drotation_range
    vertices <- randomVertices num_vertices_range radius_range
    let radius = snd radius_range
    return$ State
        { getId = id
        , getPosition = Geometry.position x y 0.0
        , getDirection = Geometry.direction dx dy dr
        , getVertices = vertices
        , getHealth = radius * Params.asteroid_health_ratio
        , getRadius = radius
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
    -- TODO(ondrasej): Add a force impulse to the asteroid when it collides with
    -- the player.
    player <- GameState.player
    duration <- GameState.duration
    let sphere = collisionSphere asteroid
        asteroid_id = getId asteroid
    when (PlayerState.isCollision sphere player) $do
        -- TODO(ondrasej): The damage should depend on the mass of the asteroid
        -- and the relative velocity of the player and the asteroid.
        GameState.addPlayerDamage 10.0
        GameState.setAsteroidCollision asteroid
    damage <- GameState.asteroidDamageById asteroid_id
    let old_position = getPosition asteroid
        old_direction = getDirection asteroid
        raw_position =
            Geometry.updatePosition duration old_position old_direction
        new_position =
            Geometry.boundedPosition (Geometry.keepRotation) raw_position 
        new_direction = Geometry.boundedDirection raw_position old_direction
        old_health = getHealth asteroid
        new_health = old_health - damage
    when (new_health > 0.0) $do
        GameState.addUpdatedAsteroid$ asteroid { getPosition = new_position
                                               , getDirection = new_direction
                                               , getHealth = new_health }
