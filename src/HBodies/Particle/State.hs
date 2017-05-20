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

module HBodies.Particle.State
    (
      State(..)
    ) where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified HBodies.Geometry as Geometry
import qualified HBodies.Time as Time

-- | Contains information about the state of a single particle in the game.
data State = State
    { -- | The position of the particle.
      getPosition :: !Geometry.Position
      -- | The velocity of the particle.
    , getVelocity :: !Geometry.Velocity
      -- | The time at which the particle will disappear.
    , getEndTime :: Time.Time
      -- | The color of the particle.
    , getColor :: GL.Color3 Double
      -- | The effective radius of the particle.
    , getRadius :: !Double
      -- | The geometry of the particle.
      -- TODO(ondrasej): Consider using only a limited set of geometries for the
      -- particles to conserve memory and reduce cache misses.
    , getVertices :: [GL.Vertex2 GL.GLdouble]
    } deriving (Read, Show)
