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

-- | Contains definitions of data types for the state of an asteroid.
module HBodies.Asteroid.State
    (
      State(..)

    , isCollision
    , collisionSphere
    ) where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified HBodies.Geometry as Geometry
import qualified HBodies.Time as Time
import qualified System.Random as Random

-- | The state of an asteroid.
data State = State
    {
      -- | The current position of the asteroid.
      getPosition :: !Geometry.Position
      -- | The current velocity of the asteroid.
    , getVelocity :: !Geometry.Velocity
      -- | The shape of the asteroid. It can be rendered as a line loop along
      -- all the vertices of the asteroid.
    , getVertices :: [GL.Vertex2 GL.GLdouble]
      -- | The current "health" of the asteroid. The health is a value between
      -- 0 and 100. When the health reaches 0, the asteroid falls apart.
    , getHealth :: !Double
      -- | The effective radius of the asteroid. For the purpose of collision
      -- detection, the asteroid is a circle with this radius whose center is
      -- at getPosition.
    , getRadius :: !Double }
    deriving (Read, Show)

-- | Returns a collision sphere for the asteroid.
collisionSphere :: State -> (Geometry.Position, Double)
collisionSphere asteroid = (getPosition asteroid, getRadius asteroid)

-- | Detects collision of a sphere with an asteroid.
isCollision :: (Geometry.Position, Double)
            -- ^ The sphere for which collisions are detected.
            -> State
            -- ^ The asteroid for which collisions are detected.
            -> Bool
            -- ^ True if the asteroid collides with the sphere.
isCollision sphere asteroid =
    Geometry.isCollision (collisionSphere asteroid) sphere