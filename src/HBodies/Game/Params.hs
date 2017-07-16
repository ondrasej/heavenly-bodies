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

module HBodies.Game.Params where

import qualified HBodies.Time as Time

-- | The acceleration of the player ship (per frame).
acceleration = 2.0

-- | The slowdown factor of the ship (per frame).
slowdown_factor = 0.9999

-- | The speed of the rotation of the player ship.
rotation_speed = 3.0

-- | The boundaries of the screen.
screen_left = -400.0
screen_right = 400.0
screen_top = -300.0
screen_bottom = 300.0

num_asteroids_on_start = 10

asteroid_x_range = (screen_left, screen_right)
asteroid_y_range = (screen_bottom, screen_top)
asteroid_dx_range = (-5.0, 5.0)
asteroid_dy_range = (-5.0, 5.0)
asteroid_drotation_range = (-0.3, 0.3)
asteroid_radius_range = (20.0, 30.0)
asteroid_vertices_range = (6, 20) :: (Int, Int)
-- | The ratio between the radius of an asteroid and its initial health.
asteroid_health_ratio = 4.0
asteroid_mass_ratio = 5.0
asteroid_min_radius = 3.0
asteroid_num_shards = 3 :: Int

-- | The minimal and the maximal ratio for the size of the asteroid shards.
shard_radius_ratio_range = (0.3, 0.6)
shard_velocity_range = (5.0, 20.0)
shard_angle_noise_range = (-0.5, 0.5)

bullet_damage = 6.0
bullet_lifespan = Time.durationSeconds 1.0
bullet_period = Time.durationSeconds 0.1
bullet_speed = 130.0
bullet_radius = 1.0
bullet_mass = 1.0

-- | The maximal health of the player.
player_max_health = 100.0
player_radius = 4.0
player_invincibility_duration = Time.durationSeconds 0.5

-- Parameters of the particles emitted by the engine of the player.
engine_particle_brightness_range = (0.2, 0.4)
engine_particle_lifespan_range = (0.5, 1.0)
engine_particle_speed_range = (30.0, 40.0)
engine_particle_vertices = 3 :: Int
engine_particle_radius = 1.0
engine_particle_deviation_range = (-0.2, 0.2)

-- | The seed used by the random number generator. This seed is used only to
-- initialize the random number generator in a new game state. All other
-- generators in the game are split off this generator.
random_seed :: Int
random_seed = 123456789

-- | The position and dimension of the health bar.
health_bar_left = -385.0
health_bar_bottom = -285.0
health_bar_right = health_bar_left + health_bar_width
health_bar_top = health_bar_bottom + health_bar_height
health_bar_width = 150.0
health_bar_height = 20.0
health_bar_margin = 3.0
