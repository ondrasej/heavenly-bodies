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

module HBodies.Time
    ( -- * Type definitions.
      Time
    , Duration

      -- * Useful constants.
    , startTime
    , infinitePast
    , frameDuration
    , zeroDuration

      -- * Reading time
    , sdlTime

      -- * Conversion from and to seconds.
    , timeSeconds
    , secondsFromTime
    , durationSeconds
    , secondsFromDuration

      -- * Time arithmetics.
    , add
    , diff
    ) where

import qualified SDL

-- | Data type for a timestamp.
newtype Time = T Double
    deriving (Eq, Ord, Read, Show)

-- | Data type for duration.
newtype Duration = D Double
    deriving (Eq, Ord, Read, Show)

instance Num Duration where
  (D d1) + (D d2) = D$ d1 + d2
  (D d1) - (D d2) = D$ d1 - d2
  _ * _ = error "Durations can't be multiplied!"
  abs (D d) = D$ abs d
  negate (D d) = D$ -d
  signum (D d) = D$ signum d
  fromInteger _ = error$ "Durations can't be created from integers. " ++
                         "Use durationSeconds instead"

infinity :: Double
infinity = read "Infinity"

-- | The time at the start of the game.
startTime :: Time
startTime = T 0.0

-- | A time in the past that happened earlier than any valid timestamp.
infinitePast :: Time
infinitePast = T (-infinity)

-- | Returns the current timestamp, as returned by SDL.
sdlTime :: IO Time
sdlTime = do
    time_seconds <- SDL.time :: IO Double
    return$ timeSeconds time_seconds

-- | The duration of length zero.
zeroDuration :: Duration
zeroDuration = D 0.0

-- | The duration of a single frame. Set up for 60 FPS.
frameDuration :: Duration
frameDuration = D$ 1.0 / 60.0

-- | Creates a new time stamp representing the timestamp 'seconds' seconds
-- after the start of the game.
timeSeconds :: Double -> Time
timeSeconds seconds = T seconds

-- | Returns the number of seconds between 'time' and start time.
secondsFromTime :: Time -> Double
secondsFromTime time@(T seconds) = seconds

-- | Creates a new duration object representing the given number of seconds.
durationSeconds :: Double -> Duration
durationSeconds seconds = D seconds

-- | Returns the number of seconds of the duration.
secondsFromDuration :: Duration -> Double
secondsFromDuration (D seconds) = seconds

-- | Advances the time by the given duration.
add :: Time -> Duration -> Time
add (T time_seconds) (D duration_seconds) = T$ time_seconds + duration_seconds

-- | Returns the duration between two time stamps.
diff :: Time -> Time -> Duration
diff (T s1) (T s2) = D (s1 - s2)
