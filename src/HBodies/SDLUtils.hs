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

module HBodies.SDLUtils
    (
      -- * Functions for inspecting SDL events.
      isActivateEvent,
      isDeactivateEvent,
      isQuitEvent
    ) where

import Control.Exception
import Control.Monad
import Foreign.C.String
import Foreign.C.Types

import qualified SDL

-- | Returns true if the provided event is an SDL quit event.
isQuitEvent :: SDL.Event -> Bool
isQuitEvent event = isQuitEventPayload (SDL.eventPayload event)
  where
    isQuitEventPayload SDL.QuitEvent = True
    isQuitEventPayload (SDL.KeyboardEvent key_event) =
        key_motion == SDL.Released && keycode == SDL.KeycodeEscape
      where
        key_motion = SDL.keyboardEventKeyMotion key_event
        keysym = SDL.keyboardEventKeysym key_event
        keycode = SDL.keysymKeycode keysym
    isQuitEventPayload _ = False

-- | Returns true if the application was deactivated (lost focus).
isDeactivateEvent :: SDL.Event -> Bool
isDeactivateEvent event = isDeactivateEventPayload (SDL.eventPayload event)
  where
    isDeactivateEventPayload (SDL.WindowLostKeyboardFocusEvent _) = True
    isDeactivateEventPayload _ = False

-- | Returns true if the application was activated (gained focus).
isActivateEvent :: SDL.Event -> Bool
isActivateEvent event = isActivateEventPayload (SDL.eventPayload event)
  where
    isActivateEventPayload (SDL.WindowGainedKeyboardFocusEvent _) = True
    isActivateEventPayload _ = False
