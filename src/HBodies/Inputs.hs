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

module HBodies.Inputs
    (
      State( turnLeftPressed
           , turnRightPressed
           , acceleratePressed
           , deceleratePressed
           , firePressed )
    , empty
    , readInputs
    ) where

import qualified SDL

-- | Contains the inputs of the game.
data State = Inputs
    { -- | True if the player pressed the left key.
      turnLeftPressed :: !Bool
      -- | True if the player pressed the right key.
    , turnRightPressed :: !Bool
      -- | True if the player pressed the acceleration key.
    , acceleratePressed :: !Bool
      -- | True if the player pressed the deceleration key.
    , deceleratePressed :: !Bool
      -- | True if the player pressed the firing button.
    , firePressed :: !Bool }
    deriving (Read, Show)

empty :: State
empty = Inputs
    { turnLeftPressed = False
    , turnRightPressed = False
    , acceleratePressed = False
    , deceleratePressed = False
    , firePressed = False }

-- | Reads the current state of the inputs.
readInputs :: IO State
readInputs = do
    key_state <- SDL.getKeyboardState
    return$ Inputs
        { turnLeftPressed = key_state SDL.ScancodeLeft
        , turnRightPressed = key_state SDL.ScancodeRight
        , acceleratePressed = key_state SDL.ScancodeUp
        , deceleratePressed = key_state SDL.ScancodeDown
        , firePressed = key_state SDL.ScancodeSpace }
