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

module HBodies.Screen
    (
      Screen(..)
    , ScreenWrapper
    ) where

import qualified HBodies.Application as Application

-- | The interface for the screens in the game.
class Screen screen where
    render :: screen -> IO ()
    update :: screen -> Double -> (screen, [Application.Action])

data ScreenWrapper = SWrapper
    { renderFunction :: IO ()
    , updateFunction :: Double -> (ScreenWrapper, [Application.Action])
    }

instance Screen ScreenWrapper where
    render = renderFunction
    update = updateFunction

screenWrapper :: Screen screen => screen -> ScreenWrapper
screenWrapper screen = SWrapper { renderFunction = render screen
                                , updateFunction = update screen }
