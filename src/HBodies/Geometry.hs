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

module HBodies.Geometry
    (
      -- * Data types and helper functions for creating them.
      Position(getX, getY, getRotation)
    , position
    , positionRadial

    , Velocity(getDx, getDy, getDRotation)
    , velocity
    , velocityRadial

      -- * Arithmetic and other computation.
    , addVelocity

    , isCollision

    , normalizeRotation
    , updatePosition

      -- * Bounded arithmetic
    , BoundState(..)
    , boundState
    , screenBoundState
    , boundedPosition
    , boundedVelocity
    , BoundedRotation
    , bounceRotation
    , keepRotation

      -- * Misc stuff related to geometry.
    , two_pi
    ) where

import qualified HBodies.Game.Params as Params
import qualified HBodies.Time as Time

-- | Represents the position of an object in the game space. The position is
-- defined by the X and Y coordinates and its rotation. 
data Position = Pos
    { -- | The position on the X axis.
      getX :: !Double
      -- | The position on the Y axis.
    , getY :: !Double
      -- | The rotation of the object.
    , getRotation :: !Double }
    deriving (Eq, Ord, Read, Show)

-- | Represents the velocity of an object in the game space. The velocity is
-- defined as the velocity along the X and Y axis, and the rotation speed.
data Velocity = Vel
      -- | The velocity along the X axis.
    { getDx :: !Double
      -- | The velocity along the Y axis.
    , getDy :: !Double
      -- | The rotation velocity.
    , getDRotation :: !Double }
    deriving (Eq, Ord, Read, Show)

-- | Creates a new position object with the given coordinates.
position :: Double
         -- ^ The X position.
         -> Double
         -- ^ The Y position.
         -> Double
         -- ^ The rotation.
         -> Position
         -- The new position object.
position x y rotation = Pos { getX = x, getY = y, getRotation = rotation }

-- | Creates a new position object with the given radial coordinates (at the
-- given radius under the given angle from the radius).
positionRadial :: Double
               -- ^ The angle (in radians).
               -> Double
               -- ^ The radius.
               -> Double
               -- ^ The rotation of the object.
               -> Position
               -- ^ The new position object.
positionRadial angle radius rotation = Pos
    { getX = radius * cos angle,
      getY = radius * sin angle,
      getRotation = rotation }

squareDistance :: Position
               -> Position
               -> Double
squareDistance pos1 pos2 = dx * dx + dy * dy
  where
    dx = getX pos1 - getX pos2
    dy = getY pos1 - getY pos2

-- | Returns true if two spheres with the given positions and radiuses collide.
isCollision :: (Position, Double)
            -- ^ The position and the radius of the first sphere.
            -> (Position, Double)
            -- ^ The position and the radius of the second sphere.
            -> Bool
            -- ^ True if the two spheres collide.
isCollision (pos1, rad1) (pos2, rad2) = square_distance < square_radius_sum
  where
    square_distance = squareDistance pos1 pos2
    square_radius_sum = radius_sum * radius_sum
    radius_sum = rad1 + rad2

-- | Creates a new velocity object with the given coordinates.
velocity :: Double
         -- ^ The X delta.
         -> Double
         -- ^ The Y delta.
         -> Double
         -- ^ The rotation delta.
         -> Velocity
         -- ^ The new velocity object.
velocity dx dy drotation = Vel
    { getDx = dx
    , getDy = dy
    , getDRotation = drotation }

-- | Creates a new velocity object with the given radial coordinates.
velocityRadial :: Double
               -- ^ The angle (in radians).
               -> Double
               -- ^ The radius.
               -> Double
               -- ^ The rotation delta.
               -> Velocity
               -- ^ The new velocity object.
velocityRadial angle distance drotation = Vel
    { getDx = distance * cos angle
    , getDy = distance * sin angle
    , getDRotation = drotation }

-- | Addition for velocity objects.
addVelocity :: Velocity
            -- ^ The first velocity object.
            -> Velocity
            -- ^ The second velocity object.
            -> Velocity
            -- ^ The sum of the two velocities.
addVelocity v1 v2 = Vel { getDx = dx, getDy = dy, getDRotation = drotation }
  where
    dx = getDx v1 + getDx v2
    dy = getDy v1 + getDy v2
    drotation = getDRotation v1 + getDRotation v2

-- | Updates the given position with the velocity over a given time interval.
-- Mathematically, updatePosition dt pos v = pos + dt * v.
updatePosition :: Time.Duration
               -- ^ The length of the time interval.
               -> Position
               -- ^ The original position.
               -> Velocity
               -- ^ The velocity.
               -> Position
               -- ^ The updated position.
updatePosition duration position velocity = Pos
    { getX = getX position + delta_time * getDx velocity
    , getY = getY position + delta_time * getDy velocity
    , getRotation = getRotation position + delta_time * getDRotation velocity }
  where
    delta_time = Time.secondsFromDuration duration

-- | A constant for 2*PI.
two_pi :: Double
two_pi = 2 * pi

-- | Normalizes the rotation into the [-pi, pi] radius.
normalizeRotation :: Double -> Double
normalizeRotation rotation
    | rotation < -pi = normalizeRotation$ rotation + two_pi
    | rotation > pi = normalizeRotation$ rotation - two_pi
    | otherwise = rotation

-- | The state of a value with respect to bounds.
data BoundState = UnderBound
                -- ^ The value is smaller than the lower bound.
                | InBound
                -- ^ The value is within the bounds.
                | OverBound
                -- ^ The value is greater than the upper bound.

-- | The type of a function that updates rotation based on the state of the X/Y
-- coordinate bounds.
type BoundedRotation = Double
                     -- ^ The initial rotation.
                     -> BoundState
                     -- ^ The state of the X coordinate bound.
                     -> BoundState
                     -- ^ The state of the Y coordinate bound.
                     -> Double
                     -- ^ The new rotation.

-- | Returns the bound state of the given value with respect to the bounds.
boundState :: Ord a
           => a
           -- ^ The actual value.
           -> a
           -- ^ The lower bound.
           -> a
           -- ^ The upper bound.
           -> BoundState
              -- ^ The state of the value.
boundState value lower_bound upper_bound =
    if value < lower_bound then UnderBound
    else if value > upper_bound then OverBound
    else InBound

-- | Returns the state of the position with respect to screen boundaries.
screenBoundState :: Position
                 -- ^ The input position.
                 -> (BoundState, BoundState)
                 -- ^ The state of the position on the X and Y axes.
screenBoundState pos = (x_state, y_state)
  where
    x_state = boundState x Params.screen_left Params.screen_right
    y_state = boundState y Params.screen_top Params.screen_bottom
    x = getX pos
    y = getY pos

-- | An implementation of BoundedRotation that rotates the object to make a
-- bouncing effect.
bounceRotation :: BoundedRotation
bounceRotation old_rotation UnderBound UnderBound = old_rotation + pi
bounceRotation old_rotation UnderBound InBound    = pi - old_rotation
bounceRotation old_rotation UnderBound OverBound  = old_rotation + pi
bounceRotation old_rotation InBound UnderBound    = -old_rotation
bounceRotation old_rotation InBound InBound       = old_rotation
bounceRotation old_rotation InBound OverBound     = -old_rotation
bounceRotation old_rotation OverBound UnderBound  = old_rotation + pi
bounceRotation old_rotation OverBound InBound     = pi - old_rotation
bounceRotation old_rotation OverBound OverBound   = old_rotation + pi

-- | An implementation of BoundedRotation that keeps the original rotation of
-- the item.
keepRotation :: BoundedRotation
keepRotation old_rotation _ _ = old_rotation

-- | Returns the position clipped to the givne bounds. Implements simple
-- positional-based bouncing.
boundedPosition :: BoundedRotation
                -- ^ A function that updates the position.
                -> Position
                -- ^ The "raw" position.
                -> Position
                -- ^ The bounded position.
boundedPosition boundedRotation pos =
    position new_x new_y (normalizeRotation new_rotation)
  where
    (new_x, new_y) = case bound_state of
        (UnderBound, UnderBound) -> (x_bounced_left, y_bounced_top)
        (UnderBound, InBound)    -> (x_bounced_left, old_y)
        (UnderBound, OverBound)  -> (x_bounced_left, y_bounced_bottom)
        (InBound, UnderBound)    -> (old_x, y_bounced_top)
        (InBound, InBound)       -> (old_x, old_y)
        (InBound, OverBound)     -> (old_x, y_bounced_bottom)
        (OverBound, UnderBound)  -> (x_bounced_right, y_bounced_top)
        (OverBound, InBound)     -> (x_bounced_right, old_y)
        (OverBound, OverBound)   -> (x_bounced_right, y_bounced_bottom)
    new_rotation = boundedRotation old_rotation x_state y_state
    bound_state@(x_state, y_state) = screenBoundState pos

    x_bounced_left = 2.0 * Params.screen_left - old_x
    x_bounced_right = 2.0 * Params.screen_right - old_x
    y_bounced_top = 2.0 * Params.screen_top - old_y
    y_bounced_bottom = 2.0 * Params.screen_bottom - old_y

    old_x = getX pos
    old_y = getY pos
    old_rotation = getRotation pos

-- | Updates the velocity of the player based on the bounds of the screen - if
-- the player hits the edge of the screen, reverse the velocity to make them
-- bounce.
boundedVelocity :: Position
                -- ^ The position of the player in the current frame.
                -> Velocity
                -- ^ The velocity of the player in the previous frame.
                -> Velocity
                -- ^ The new velocity of the player.
boundedVelocity pos old_velocity =
    velocity new_dx new_dy new_drotation
  where
    (new_dx, new_dy) = case screenBoundState pos of
        (InBound, InBound) -> (old_dx, old_dy)
        (InBound, _)       -> (old_dx, -old_dy)
        (_, InBound)       -> (-old_dx, old_dy)
    new_drotation = getDRotation old_velocity
    old_dx = getDx old_velocity
    old_dy = getDy old_velocity
