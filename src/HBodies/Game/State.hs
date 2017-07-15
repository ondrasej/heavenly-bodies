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

-- | Contains definitions of the data types for the state of the game, the game
-- update monad, and basic functions for working inside this monad.
module HBodies.Game.State where

import Control.Applicative ((<*>), (<$>))
import qualified Control.Monad.State as MonadState
import qualified Data.Map.Strict as Map
import qualified HBodies.Asteroid.State as AsteroidState
import qualified HBodies.Bullet.State as BulletState
import qualified HBodies.Inputs as Inputs
import qualified HBodies.Particle.State as ParticleState
import qualified HBodies.Player.State as PlayerState
import qualified HBodies.Time as Time
import qualified System.Random as Random

-- | The list of asteroids indexed by their IDs.
type IndexedAsteroids = Map.Map AsteroidState.Id AsteroidState.State

-- | The state of the game.
data State = GameState
    { -- | The list of asteroids in the game.
      getAsteroids :: !IndexedAsteroids
      -- | The list of bullets in the game.
    , getBullets :: ![BulletState.State]
      -- | The time when the last bullet was fired.
    , getLastBulletTime :: !Time.Time
      -- | The ID of the next asteroid created in the game.
    , getStateNextAsteroidId :: !AsteroidState.Id
      -- | The time of the last update (of in-game time).
    , getLastUpdate :: !Time.Time
      -- | The state of the particles in the game.
    , getParticles :: [ParticleState.State]
      -- | The player object.
    , getPlayer :: PlayerState.State
      -- | The random number generator used by the game.
    , getRandomGenerator :: Random.StdGen
    }

-- | Collection of updates to the game state between two frames. This is used
-- as the state in the updaet
data UpdateData = UpdateData
    { -- | The state of the game before the update.
      getCurrentState :: !State
      -- | The amount of in-game time that passed since the last frame.
    , getDuration :: !Time.Duration
      -- | The state of the inputs during the update.
    , getInputState :: !Inputs.State
      -- | The accumulated damage to the player.
    , getPlayerDamage :: !Double
      -- | The asteroid the player collided with in this frame. Contains None
      -- if there was no collision; contains the last asteroid if the player
      -- collided with more than one in the same frame.
    , getAsteroidCollision :: Maybe AsteroidState.State
      -- | The damage inflicted to the asteroids during this frame.
    , getAsteroidDamage :: Map.Map AsteroidState.Id Double
      -- | The states of the asteroids in the following frame.
    , getUpdatedAsteroids :: !IndexedAsteroids
      -- | The states of the bullets in the following frame. Note that the list
      -- is reversed before it is used in the State structure of the next frame
      -- to make the order of the bullets in the list stable.
    , getUpdatedBullets :: ![BulletState.State]
      -- | Whether a bullet was fired this round.
    , getBulletFired :: Bool
      -- | The ID of the next asteroid created in the game.
    , getUpdateNextAsteroidId :: !AsteroidState.Id
      -- | The updated player data.
    , getUpdatedPlayer :: !PlayerState.State
      -- | The state of the particles in the following frame. Note that the
      -- list is reversed before it is used in the State structure of the next
      -- frame to make the order of the particles in the list stable.
    , getUpdatedParticles :: ![ParticleState.State]
      -- | The random generator used during the update. This generator is used
      -- (and changed) by the functions HBodies.Game.State.random and
      -- HBodies.Game.State.randomR.
    , getUpdateRandomGenerator :: Random.StdGen
    }

-- | The monad, in which all state updates are executed.
type Update = MonadState.State UpdateData

addNewBullet :: BulletState.State -> Update ()
addNewBullet bullet = MonadState.modify'$ \d -> 
    d { getUpdatedBullets = bullet:getUpdatedBullets d
      , getBulletFired = True }

-- | Records damage inflicted to the given asteroid in this frame.
addAsteroidDamage :: AsteroidState.Id
                  -- ^ The ID of the asteroid receiving the damage.
                  -> Double
                  -- ^ The damage added to the asteroid.
                  -> Update ()
addAsteroidDamage asteroid_id damage = MonadState.modify'$ \d -> 
    let damages = getAsteroidDamage d
        new_damage = damage + Map.findWithDefault 0.0 asteroid_id damages
        new_damages = Map.insert asteroid_id new_damage damages
    in d { getAsteroidDamage = new_damages }

-- | Adds player damage in the current frame.
addPlayerDamage :: Double
                -- ^ The damage added to the player.
                -> Update ()
addPlayerDamage damage = MonadState.modify'$ \d ->
    d { getPlayerDamage = damage + getPlayerDamage d }

-- | Adds an updated asteroid to the state updates.
addUpdatedAsteroid :: AsteroidState.State
                   -- ^ The updated asteroid object.
                   -> Update ()
addUpdatedAsteroid asteroid = MonadState.modify'$ \d ->
    let asteroids = getUpdatedAsteroids d
        asteroid_id = AsteroidState.getId asteroid
    in d { getUpdatedAsteroids = Map.insert asteroid_id asteroid asteroids }

-- | Adds an updated bullet to state updates.
addUpdatedBullet :: BulletState.State 
                 -- ^ The updated bullet object.
                 -> Update ()
addUpdatedBullet bullet = MonadState.modify'$ \d ->
    d { getUpdatedBullets = bullet:getUpdatedBullets d }

-- | Adds an updated particle to the state updates.
addUpdatedParticle :: ParticleState.State
                   -- ^ The updated particle object.
                   -> Update ()
addUpdatedParticle particle = MonadState.modify'$ \d ->
    d { getUpdatedParticles = particle:getUpdatedParticles d }

-- | Returns the list of asteroids from the previous frame.
asteroids :: Update IndexedAsteroids
asteroids = MonadState.gets$ getAsteroids . getCurrentState

-- | Returns the asteroid with the given ID, or Nothing if no such asteroid was
-- found.
asteroidById :: AsteroidState.Id -> Update (Maybe AsteroidState.State)
asteroidById asteroid_id = Map.lookup asteroid_id <$> asteroids

-- | Returns the asteroid that collided with the player, or Nothing if there was
-- no collision or the collision was not detected yet.
asteroidCollision :: Update (Maybe AsteroidState.State)
asteroidCollision = MonadState.gets$ getAsteroidCollision

-- | Returns the damage inflicted to the given asteroid in the current frame.
-- Returns 0.0 if there was no damage to the asteroid or there is no such
-- asteroid.
asteroidDamageById :: AsteroidState.Id -> Update Double
asteroidDamageById asteroid_id =
    MonadState.gets$ Map.findWithDefault 0.0 asteroid_id . getAsteroidDamage

-- | Returns the "previous" state in the update monad.
currentState :: Update State
currentState = MonadState.gets$ getCurrentState

-- | Returns the in-game time passed since the last frame.
duration :: Update Time.Duration
duration = MonadState.gets$ getDuration

-- | Returns the state of the inputs in the update.
inputs :: Update Inputs.State
inputs = MonadState.gets$ getInputState

-- | Returns the timestamp of the current frame.
currentFrameTime :: Update Time.Time
currentFrameTime = Time.add <$> lastFrameTime <*> duration

-- | Returns the time when the last bullet was fired.
lastBulletTime :: Update Time.Time
lastBulletTime = MonadState.gets$ getLastBulletTime . getCurrentState

-- | Returns the timestamp of the last frame.
lastFrameTime :: Update Time.Time
lastFrameTime = MonadState.gets$ getLastUpdate . getCurrentState

-- | Returns a new asteroid ID.
newAsteroidId :: Update AsteroidState.Id
newAsteroidId = MonadState.state$ \d ->
    let id = getUpdateNextAsteroidId d
    in (id, d { getUpdateNextAsteroidId = AsteroidState.nextId id })

-- | Returns the state of the player in the previous frame.
player :: Update PlayerState.State
player = MonadState.gets$ getPlayer . getCurrentState

-- | Returns the playr damage accumulated so far in the current frame.
playerDamage :: Update Double
playerDamage = MonadState.gets$ getPlayerDamage

-- | Generates a random value using the random generator in the monad.
random :: Random.Random a => Update a
random = MonadState.state$ \d ->
    let generator = getUpdateRandomGenerator d
        (v, generator') = Random.random generator
    in (v, d { getUpdateRandomGenerator = generator' })

-- | Generates a random value from the given range using the random
-- generator in the monad.
randomR :: Random.Random a => (a, a) -> Update a
randomR range = MonadState.state$ \d ->
    let generator = getUpdateRandomGenerator d
        (v, generator') = Random.randomR range generator
    in (v, d { getUpdateRandomGenerator = generator' })

-- | Sets the asteroid the player collided with.
setAsteroidCollision :: AsteroidState.State
                     -- ^ The asteroid the player collided with.
                     -> Update ()
setAsteroidCollision asteroid = MonadState.modify'$ \d ->
    d { getAsteroidCollision = Just asteroid }

-- | Returns the updated data of the asteroid with the given ID. Returns Nothing
-- if no such asteroid exists or if the asteroid was not updated yet.
updatedAsteroidById :: AsteroidState.Id -> Update (Maybe AsteroidState.State)
updatedAsteroidById asteroid_id =
    Map.lookup asteroid_id <$> MonadState.gets getUpdatedAsteroids

-- | Modifies the player state in the update.
updatePlayer :: PlayerState.State
             -- ^ The new state of the player.
             -> Update ()
updatePlayer player = MonadState.modify'$ \d -> d { getUpdatedPlayer = player }

-- | Runs a game update action. Collects the updates produced by the action into
-- an empty UpdateData structure.
runUpdate :: Time.Duration
          -- ^ The amount of in-game time that passed since the last frame.
          -> Inputs.State
          -- ^ The state of the inputs during the update.
          -> State
          -- ^ The previous state of the game.
          -> Update a
          -- ^ The update action.
          -> UpdateData
          -- ^ The updates produced by the action.
runUpdate duration inputs old_state code = MonadState.execState code empty
  where
    empty = UpdateData
        { getCurrentState = old_state
        , getDuration = duration
        , getInputState = inputs
        , getPlayerDamage = 0.0
        , getAsteroidCollision = Nothing
        , getAsteroidDamage = Map.empty
        , getUpdatedAsteroids = Map.empty
        , getUpdateNextAsteroidId = getStateNextAsteroidId old_state
        , getUpdatedBullets = []
        , getBulletFired = False
        , getUpdatedParticles = []
        , getUpdatedPlayer = getPlayer old_state
        , getUpdateRandomGenerator = getRandomGenerator old_state }
