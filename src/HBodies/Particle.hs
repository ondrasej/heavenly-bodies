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

-- | Contains code for controlling particles.
module HBodies.Particle
    (
      -- * The types used by the particles.
      State(getPosition)

      -- * Functions for manipulating the state.
    , newRegular
    , update
  
      -- * Functions for rendering the particles.
    , render
    ) where

import Control.Monad (unless)
import qualified Data.Foldable as Foldable
import Data.Foldable (forM_)
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified HBodies.Asteroid.State as AsteroidState
import qualified HBodies.Game.State as GameState
import qualified HBodies.Geometry as Geometry
import qualified HBodies.GLUtils as GLUtils
import HBodies.Particle.State
import qualified HBodies.Time as Time

-- | Initializes a new particle with the given position, direction, lifespan,
-- color and number of vertices. The shape of the particle will be a regular
-- n-gon with the given number of vertices and the given radius.
newRegular :: Geometry.Position
           -- ^ The position of the particle.
           -> Geometry.Direction
           -- ^ The direction of the particle.
           -> Time.Time
           -- ^ The lifespan of the particle; the particle will disappear after
           -- the given time.
           -> GL.Color3 Double
           -- ^ The color of the particle.
           -> Int
           -- ^ The number of vertices of the particle.
           -> Double
           -- ^ The radius of the particle.
           -> State
           -- ^ The new particle.
newRegular position direction end_time color num_vertices radius = State
    { getPosition = position
    , getDirection = direction
    , getEndTime = end_time
    , getColor = color
    , getRadius = radius
    , getVertices = vertices }
  where
    vertices = map (makeVertex) [0..(num_vertices - 1)]
    piece_angle = Geometry.two_pi / (fromIntegral num_vertices)
    makeVertex n = GLUtils.vertex2D vertex_x vertex_y
      where
        vertex_x = radius * cos angle
        vertex_y = radius * sin angle
        angle = piece_angle * (fromIntegral n)

-- | Renders the particle.
render :: State
       -- ^ The particle to be rendered.
       -> IO ()
render particle = do
    GL.preservingAttrib [GL.AllServerAttributes] $do
        GL.preservingMatrix $do
            GLUtils.setUpMatrixFromPosition$ getPosition particle
            GL.renderPrimitive GL.LineLoop $do
                GL.color$ getColor particle
                forM_ (getVertices particle) GL.vertex

-- | Updates the particle.
update :: State
       -- ^ The original state of the particle.
       -> GameState.Update ()
update particle = do
    duration <- GameState.duration
    current_time <- GameState.currentFrameTime
    asteroids <- GameState.asteroids
    let expired = current_time > getEndTime particle
        sphere = (getPosition particle, getRadius particle)
        collision = Foldable.any (AsteroidState.isCollision sphere) asteroids
    unless (expired || collision) $do
        let position = getPosition particle
            direction = getDirection particle
            new_position = Geometry.updatePosition duration position direction
        GameState.addUpdatedParticle$ particle { getPosition = new_position }
