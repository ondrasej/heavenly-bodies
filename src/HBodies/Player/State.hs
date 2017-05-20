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

-- | Contains definitions of data types for the state of the player.
module HBodies.Player.State 
    (
      State(..)

    , isCollision
    , collisionSphere
    ) where

import qualified HBodies.Game.Params as Params
import qualified HBodies.Geometry as Geometry
import qualified HBodies.Time as Time

-- | Contains information about the current state of the player.
data State = State
    { -- | The current position of the player.
      getPosition :: !Geometry.Position
      -- | The current velocity of the player.
    , getVelocity :: !Geometry.Velocity
      -- | The current health of the player. The health is a value between 0 and
      -- 100. When the health reaches 0, the player is dead and the game ends.
    , getHealth :: !Double
      -- | The timestamp of the last frame when the player is invincible.
    , getInvincibilityEnd :: !Time.Time
      -- | The last time when the player fired from the weapon.
    , getLastShot :: !Time.Time }
    deriving (Read, Show)

collisionSphere :: State -> (Geometry.Position, Double)
collisionSphere player = (getPosition player, Params.player_radius)

-- | Returns true if the player collides with the given sphere.
isCollision :: (Geometry.Position, Double)
            -> State
            -> Bool
isCollision sphere player = Geometry.isCollision player_sphere sphere
  where
    player_sphere = (getPosition player, Params.player_radius)
