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

-- | Contains functions for controlling the player. The data type that
-- represents the state of the player is in a separate module
-- "HBodies.Player.State".
module HBodies.Player
    (
      -- * The types used by the player.
      State(getPosition)

      -- * Functions for manipulating the state.
    , new
    , update

      -- * Functions for rendering the player.
    , render
    ) where

import Control.Monad (when)
import qualified Data.Maybe as Maybe
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=))
import qualified HBodies.Asteroid.State as AsteroidState
import qualified HBodies.Bullet as Bullet
import qualified HBodies.Game.Params as Params
import qualified HBodies.Game.State as GameState
import qualified HBodies.Geometry as Geometry
import HBodies.Geometry ((+.))
import qualified HBodies.GLUtils as GLUtils
import qualified HBodies.Inputs as Inputs
import qualified HBodies.Particle as Particle
import HBodies.Player.State
import qualified HBodies.Time as Time

-- | The initial state of the player - at the center of the screen turned to
-- the right.
new :: State
new = State
    { getPosition = Geometry.position 0.0 0.0 0.0
    , getDirection = Geometry.direction 0.0 0.0 0.0
    , getHealth = Params.player_max_health
    , getInvincibilityEnd = Time.infinitePast
    , getLastShot = Time.startTime }

-- | Renders the player.
render :: State
       -- ^ The state of the player being rendered.
       -> IO ()
render player = do
    GL.preservingAttrib [GL.AllServerAttributes]$ do
        -- Draw the ship.
        GL.preservingMatrix$ do
            GLUtils.setUpMatrixFromPosition$ getPosition player
            GL.renderPrimitive GL.Triangles $do
                GL.color$ GLUtils.color3D 0.0 0.0 0.0
                renderPlayerVertices
            GL.renderPrimitive GL.LineLoop $do
                GL.color$ GLUtils.color3D 1.0 1.0 1.0
                renderPlayerVertices
        renderHealthBar
  where
    renderPlayerVertices = do
        GL.vertex$ GLUtils.vertex2D 10.0 0.0
        GL.vertex$ GLUtils.vertex2D (-3.0) (-5.0)
        GL.vertex$ GLUtils.vertex2D (-3.0) 5.0
    renderHealthBar = do
        -- Draw the outline.
        GL.renderPrimitive GL.LineLoop $do
            GL.vertex$ GLUtils.vertex2D Params.health_bar_left
                                        Params.health_bar_top
            GL.vertex$ GLUtils.vertex2D Params.health_bar_right
                                        Params.health_bar_top
            GL.vertex$ GLUtils.vertex2D Params.health_bar_right
                                        Params.health_bar_bottom
            GL.vertex$ GLUtils.vertex2D Params.health_bar_left
                                        Params.health_bar_bottom
        let health_percent = getHealth player / Params.player_max_health
        when (health_percent > 0.0) $do
            -- Draw the inner bar (that actually shows the health). The inside
            -- is filled with black color.
            let bar_left = Params.health_bar_left + Params.health_bar_margin
                bar_top = Params.health_bar_top - Params.health_bar_margin
                bar_bottom = Params.health_bar_bottom + Params.health_bar_margin
                bar_max_width =
                    Params.health_bar_width - 2 * Params.health_bar_margin
                bar_right = bar_left + health_percent * bar_max_width
                emitBarVertices = do
                    GL.vertex$ GLUtils.vertex2D bar_left bar_top
                    GL.vertex$ GLUtils.vertex2D bar_right bar_top
                    GL.vertex$ GLUtils.vertex2D bar_right bar_bottom
                    GL.vertex$ GLUtils.vertex2D bar_left bar_bottom
            GL.blend $= GL.Enabled
            GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
            GL.renderPrimitive GL.Quads $do
                GL.color$ GLUtils.color4D 0.0 0.0 0.0 0.5
                emitBarVertices
            GL.blend $= GL.Disabled
            GL.renderPrimitive GL.LineLoop $do
                GL.color$ GLUtils.color4D 1.0 1.0 1.0 1.0
                emitBarVertices

-- | Updates the state of the player. This function runs in the game update
-- monad. Note that this function reads the damage dealt to the player, so it is
-- important that it is called after all damage-dealing functions.
update :: GameState.Update ()
update = do
    old_state <- GameState.player
    inputs <- GameState.inputs
    duration <- GameState.duration
    damage <- GameState.playerDamage
    current_frame_time <- GameState.currentFrameTime
    -- Update the health and the invincibility status of the player.
    let old_invincibility_end = getInvincibilityEnd old_state
        is_invincible = current_frame_time < old_invincibility_end
        old_health = getHealth old_state
        new_health = if is_invincible then old_health
                     else max (old_health - damage) 0.0
        took_damage = new_health < old_health
        new_invincible_time =
            if took_damage
            then Time.add current_frame_time
                          Params.player_invincibility_duration
            else old_invincibility_end
    -- Update the position of the player.
    let old_position = getPosition old_state
        old_direction = getDirection old_state
        -- Compute the new position and direction as if there was no collision.
        raw_new_position = updatePosition duration
                                          inputs
                                          old_position
                                          old_direction
        raw_new_direction = updatePlayerDirection duration
                                                  inputs
                                                  old_position
                                                  old_direction
    -- Computes the position and the direction of the player after bouncing from
    -- the asteroid.
    -- TODO(ondrasej): Replace it with a more physically-inspired computation.
    -- The current one simply flips the direction of the player from the
    -- bounding sphere of the asteroid, but there are no changes in the momentum
    -- of the asteroid or the player. So, while it works for a fast-moving light
    -- player vs slow-moving heavy asteroid, it would break in all other cases.
    -- This should be replaces by a more realistic computation, i.e. one that
    -- takes momentum into account and that distributes the momentum between the
    -- colliding objects.
    let bounceFromAsteroid asteroid = case maybe_bounce_time of
            Nothing -> (raw_new_position, raw_new_direction)
            Just _  -> (bounced_position, bounced_direction)
          where
            -- Note that only this variable is computed each time the function
            -- is evaluated. Everything below is evaluated only when
            -- maybe_bounce_time is not Nothing.
            maybe_bounce_time = Geometry.collisionTime old_position
                                                       old_direction
                                                       Params.player_radius
                                                       old_asteroid_position
                                                       old_asteroid_direction
                                                       asteroid_radius
            -- Since the player and the code are already in collision, it is
            -- normal that bounce_time is a small negative number.
            bounce_time = Maybe.fromJust maybe_bounce_time
            remaining_time = duration - bounce_time
            bounced_direction = Geometry.bouncedDirection old_direction
                                                          bounce_normal
            bounced_position = Geometry.updatePosition remaining_time
                                                       bounce_player_position
                                                       bounced_direction

            bounce_player_position = Geometry.updatePosition bounce_time
                                                             old_position
                                                             old_direction
            bounce_asteroid_position =
                Geometry.updatePosition bounce_time
                                        old_asteroid_position
                                        old_asteroid_direction
            bounce_normal = Geometry.positionDelta bounce_player_position
                                                   bounce_asteroid_position

            old_asteroid_position = AsteroidState.getPosition asteroid
            old_asteroid_direction = AsteroidState.getDirection asteroid
            asteroid_radius = AsteroidState.getRadius asteroid

    -- If there was a collision with an asteroid, bounce from the asteroid.
    -- TODO(ondrasej): Consider a computation where the player may collide with
    -- more than one asteroid at a time.
    asteroid_collision <- GameState.asteroidCollision
    let (new_position, new_direction) = case asteroid_collision of
            Nothing       -> (raw_new_position, raw_new_direction)
            Just asteroid -> bounceFromAsteroid asteroid

    GameState.updatePlayer$ old_state
        { getPosition = Geometry.boundedPosition Geometry.bounceRotation
                                                 new_position
        , getDirection = Geometry.boundedDirection new_position new_direction
        , getHealth = new_health
        , getInvincibilityEnd = new_invincible_time
        , getLastShot = getLastShot old_state }

    -- Add bullets if the player pressed the 'fire' button.
    -- TODO(ondrasej): Accelerate backwards a little bit when a bullet was
    -- fired.
    last_bullet_time <- GameState.lastBulletTime
    let next_bullet_time = Time.add last_bullet_time Params.bullet_period
        can_fire = current_frame_time >= next_bullet_time
    when (can_fire && Inputs.firePressed inputs) $do  
        let bullet = Bullet.new old_state current_frame_time
        GameState.addNewBullet bullet

    -- Add particles if the player accelerates or decelerates.
    let addParticleIfKeyPressed key_pressed angle = do
        when (key_pressed inputs) $do
            current_time <- GameState.currentFrameTime
            deviation <-
                GameState.randomR Params.engine_particle_deviation_range
            speed <- GameState.randomR Params.engine_particle_speed_range
            lifespan_seconds <-
                GameState.randomR Params.engine_particle_lifespan_range
            brightness <-
                GameState.randomR Params.engine_particle_brightness_range
            let particle_angle =
                    Geometry.getRotation old_position + angle + deviation
                particle_direction_base =
                    Geometry.directionRadial particle_angle speed 0.0
                particle_direction = particle_direction_base +. old_direction
                lifespan = Time.durationSeconds lifespan_seconds
                particle_end_time = Time.add current_time lifespan
                color = GLUtils.color3D brightness brightness brightness
                new_particle =
                    Particle.newRegular old_position
                                        particle_direction
                                        particle_end_time
                                        color
                                        Params.engine_particle_vertices
                                        Params.engine_particle_radius
            GameState.addUpdatedParticle new_particle
    addParticleIfKeyPressed (Inputs.acceleratePressed) pi
    addParticleIfKeyPressed (Inputs.deceleratePressed) 0.0

-- | Updates the position of the player based on the previous direction.
updatePosition :: Time.Duration
               -> Inputs.State
               -> Geometry.Position
               -> Geometry.Direction
               -> Geometry.Position
updatePosition duration inputs old_position old_direction =
    new_position { Geometry.getRotation = new_rotation }
  where
    new_position = Geometry.updatePosition duration
                                           old_position
                                           old_direction
    new_rotation = old_rotation + delta_seconds * rotation_multiplier
    old_rotation = Geometry.getRotation old_position
    delta_seconds = Time.secondsFromDuration duration

    rotation_multiplier = case (left, right) of
        (True, False) -> Params.rotation_speed
        (False, True) -> -Params.rotation_speed
        _ -> 0.0
    left = Inputs.turnLeftPressed inputs
    right = Inputs.turnRightPressed inputs

-- | Updates the direction of the player based on the inputs.
-- TODO(ondrasej): Slow the player down when they are shooting.
-- TODO(ondrasej): Slow the player down when they hit something.
updatePlayerDirection :: Time.Duration
                      -- ^ The duration since the last frame.
                      -> Inputs.State
                      -- ^ The inputs.
                      -> Geometry.Position
                      -- ^ The previous position of the player.
                      -> Geometry.Direction
                      -- ^ The direction of the player in the previous frame.
                      -> Geometry.Direction
                      -- ^ The new direction.
updatePlayerDirection duration inputs old_position old_direction = new_direction
  where
    new_direction = Geometry.direction new_dx new_dy 0.0
    new_dx = Params.slowdown_factor * Geometry.getDx old_direction +
             Params.acceleration * direction_multiplier * cos rotation
    new_dy = Params.slowdown_factor * Geometry.getDy old_direction +
             Params.acceleration * direction_multiplier * sin rotation
    rotation = Geometry.getRotation old_position

    direction_multiplier = case (accelerate, decelerate) of
        (True, False) -> 1.0
        (False, True) -> -1.0
        _ -> 0.0

    accelerate = Inputs.acceleratePressed inputs
    decelerate = Inputs.deceleratePressed inputs
