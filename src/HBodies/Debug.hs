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

-- | Contains helper functions for debugging the code.
module HBodies.Debug
    (
      -- * Tracing utilities.
      traceShowTag
    ) where

import Debug.Trace as Trace

-- | An alternative for traceShow that displays the given value with the given
-- label. This is equivalent to trace (tag ++ ": " ++ show value) value.
traceShowTag :: Show a
             => String
             -- ^ The label displayed in front of the value.
             -> a
             -- ^ The displayed value.
             -> a
             -- ^ The value is returned unchanged.
traceShowTag tag value = Trace.trace (tag ++ ": " ++ show value) value
