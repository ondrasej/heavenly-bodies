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

-- | Contains definitions of data types for the bullets shot by the player.
module HBodies.Bullet.State
    (
      State(..)
    ) where

import qualified HBodies.Geometry as Geometry
import qualified HBodies.Time as Time

-- | Contains information about the state of a single bullet.
data State = State
    {
      -- | The current position of the bullet.
      getPosition :: !Geometry.Position
      -- | The direction of the bullet.
    , getDirection :: !Geometry.Direction
      -- | The time at which the bullet will disappear.
    , getEndTime :: !Time.Time
    } deriving (Read, Show)
