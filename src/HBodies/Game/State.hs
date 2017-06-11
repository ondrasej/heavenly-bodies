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

import Control.Applicative ((<$>))
import qualified Control.Monad.State as MonadState
import qualified HBodies.Asteroid.State as AsteroidState
import qualified HBodies.Inputs as Inputs
import qualified HBodies.Particle.State as ParticleState
import qualified HBodies.Player.State as PlayerState
import qualified HBodies.Time as Time
import qualified System.Random as Random

-- | The state of the game.
data State = GameState
    { -- | The list of asteroids in the game.
      getAsteroids :: [AsteroidState.State]
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
      -- | The states of the asteroids in the following frame. Note that the
      -- list is reversed before it is used in the State structure of the next
      -- frame to make the order of the asteroids in the list stable.
    , getUpdatedAsteroids :: ![AsteroidState.State]
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

-- | Adds player damage in the current frame.
addPlayerDamage :: Double
                -- ^ The damage added to the player.
                -> Update ()
addPlayerDamage damage = do
    d <- MonadState.get
    let new_damage = damage + getPlayerDamage d
    MonadState.put$ d { getPlayerDamage = new_damage }

-- | Adds an updated asteroid to the state updates.
addUpdatedAsteroid :: AsteroidState.State
                   -- ^ The updated asteroid object.
                   -> Update ()
addUpdatedAsteroid asteroid = do
    d <- MonadState.get
    let asteroids = getUpdatedAsteroids d
    MonadState.put$ d { getUpdatedAsteroids = asteroid:asteroids }

-- | Adds an updated particle to the state updates.
addUpdatedParticle :: ParticleState.State
                   -- ^ The updated particle object.
                   -> Update ()
addUpdatedParticle particle = do
    d <- MonadState.get
    let particles = getUpdatedParticles d
    MonadState.put$ d { getUpdatedParticles = particle:particles }

-- | Returns the list of asteroids from the previous frame.
asteroids :: Update [AsteroidState.State]
asteroids = do
    d <- MonadState.get
    return$ getAsteroids$ getCurrentState d

asteroidCollision :: Update (Maybe AsteroidState.State)
asteroidCollision = do
    d <- MonadState.get
    return$ getAsteroidCollision d

-- | Returns the "previous" state in the update monad.
currentState :: Update State
currentState = getCurrentState <$> MonadState.get

-- | Returns the in-game time passed since the last frame.
duration :: Update Time.Duration
duration = getDuration <$> MonadState.get

-- | Returns the state of the inputs in the update.
inputs :: Update Inputs.State
inputs = getInputState <$> MonadState.get

-- | Returns the timestamp of the current frame.
currentFrameTime :: Update Time.Time
currentFrameTime = do
    d <- MonadState.get
    duration <- duration
    let lastFrame = getLastUpdate$ getCurrentState d
    return$ Time.add lastFrame duration

-- | Returns the timestamp of the last frame.
lastFrameTime :: Update Time.Time
lastFrameTime = do
    d <- MonadState.get
    return$ getLastUpdate$ getCurrentState d

-- | Returns a new asteroid ID.
newAsteroidId :: Update AsteroidState.Id
newAsteroidId = do
    d <- MonadState.get
    let id = getUpdateNextAsteroidId d
    MonadState.put$ d { getUpdateNextAsteroidId = AsteroidState.nextId id }
    return id

-- | Returns the state of the player in the previous frame.
player :: Update PlayerState.State
player = do
    d <- MonadState.get
    return$ getPlayer$ getCurrentState d

-- | Returns the playr damage accumulated so far in the current frame.
playerDamage :: Update Double
playerDamage = getPlayerDamage <$> MonadState.get

-- | Generates a random value using the random generator in the monad.
random :: Random.Random a => Update a
random = do
    d <- MonadState.get
    let generator = getUpdateRandomGenerator d
        (v, generator') = Random.random generator
    MonadState.put$ d { getUpdateRandomGenerator = generator' }
    return v

-- | Generates a random value from the given range using the random
-- generator in the monad.
randomR :: Random.Random a => (a, a) -> Update a
randomR range = do
    d <- MonadState.get
    let generator = getUpdateRandomGenerator d
        (v, generator') = Random.randomR range generator
    MonadState.put$ d { getUpdateRandomGenerator = generator' }
    return v

-- | Sets the asteroid the player collided with.
setAsteroidCollision :: AsteroidState.State
                     -- ^ The asteroid the player collided with.
                     -> Update ()
setAsteroidCollision asteroid = do
    d <- MonadState.get
    return$ getAsteroidCollision d
    MonadState.put$ d { getAsteroidCollision = Just asteroid }

-- | Modifies the player state in the update.
updatePlayer :: PlayerState.State
             -- ^ The new state of the player.
             -> Update ()
updatePlayer player = MonadState.modify doUpdate
  where
    doUpdate d = d { getUpdatedPlayer = player }

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
runUpdate duration inputs old_state code = update
  where
    (_, update) = MonadState.runState code empty_update
    empty_update = UpdateData
        { getCurrentState = old_state
        , getDuration = duration
        , getInputState = inputs
        , getPlayerDamage = 0.0
        , getAsteroidCollision = Nothing
        , getUpdatedAsteroids = []
        , getUpdateNextAsteroidId = getStateNextAsteroidId old_state
        , getUpdatedParticles = []
        , getUpdatedPlayer = getPlayer old_state
        , getUpdateRandomGenerator = getRandomGenerator old_state }
