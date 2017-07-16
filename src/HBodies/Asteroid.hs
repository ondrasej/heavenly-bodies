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
import qualified Data.Functor as Functor
import qualified Graphics.Rendering.OpenGL.GL as GL
import HBodies.Asteroid.State
import qualified HBodies.Game.Params as Params
import qualified HBodies.Game.State as GameState
import qualified HBodies.Geometry as Geometry
import HBodies.Geometry ((+.), (*.))
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

-- | Creates a new asteroid with the given parameters. The shape of the new
-- asteroid is created randomly using the random generator from the state
-- update monad.
new :: Geometry.Position
    -- ^ The position of the new asteroid.
    -> Geometry.Direction
    -- ^ The direction of the new asteroid.
    -> Double
    -- ^ The radius of the new asteroid.
    -> (Int, Int)
    -- ^ The range for the number of vertices of the asteroid.
    -> GameState.Update State
    -- ^ The new asteroid.
new pos dir radius num_vertices_range = do
    let radius_range = (0.9 * radius, 1.1 * radius)
    id <- GameState.newAsteroidId
    vertices <- randomVertices num_vertices_range radius_range
    return$ State
        { getId = id
        , getPosition = pos
        , getDirection = dir
        , getVertices = vertices
        , getHealth = radius * Params.asteroid_health_ratio
        , getRadius = radius
        }

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
    if new_health > 0.0
      then do
        GameState.addUpdatedAsteroid$ asteroid { getPosition = new_position
                                               , getDirection = new_direction
                                               , getHealth = new_health }
      else do
        -- The asteroid is "dead". If it was big enoug, we split it into a
        -- couple of smaller asteroids.
        -- TODO(ondrasej): Also add some particles and other effects.
        forM_ [0..Params.asteroid_num_shards - 1] $ \i -> do
            addNewShard asteroid new_position new_direction i

-- | Adds a new shard of a given asteroid to the game. This function should be
-- called from Asteroid.update, and the position & direction are the new
-- position and the direction of the asteroid, so that they don't need to be
-- recomputed again in this function.
addNewShard :: State
            -- ^ The asteroid for which the shard is added.
            -> Geometry.Position
            -- ^ The new position of the asteroid.
            -> Geometry.Direction
            -- ^ The new direction of the asteroid.
            -> Int
            -- ^ The index of the shard.
            -> GameState.Update ()
addNewShard asteroid new_position new_direction i = do
    let radius = getRadius asteroid
        rotation = Geometry.getRotation new_position
        angle_piece = Geometry.two_pi / fromIntegral Params.asteroid_num_shards
        new_radius_range = (radius * fst Params.shard_radius_ratio_range,
                            radius * snd Params.shard_radius_ratio_range)
    new_radius <- GameState.randomR new_radius_range
    when (new_radius > Params.asteroid_min_radius) $do
        shard_angle_noise <- GameState.randomR Params.shard_angle_noise_range
        let shard_angle = fromIntegral i * angle_piece +
                          shard_angle_noise + rotation
        shard_velocity <- GameState.randomR Params.shard_velocity_range
        shard_drotation <- GameState.randomR Params.asteroid_drotation_range
        let shard_radius = (getRadius asteroid) / 2.0
            shard_health = new_radius
            shard_direction = Geometry.directionRadial shard_angle
                                                       shard_velocity
                                                       shard_drotation
            -- Move the shard a bit away from the center of the explosion.
            shard_displacement =
                0.5 * radius *. Geometry.unitDirection shard_direction
            shard_position = Geometry.addDirection new_position
                                                   shard_displacement
        shard <- new shard_position (new_direction +. shard_direction)
                     new_radius Params.asteroid_vertices_range
        GameState.addUpdatedAsteroid shard
