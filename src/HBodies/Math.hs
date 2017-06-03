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

module HBodies.Math
    (
      quadraticEquation
    ) where

-- | Computes the solutions of a quadratic equation:
-- - When the equation has two solutions, the smaller of them comes first.
-- - When the equation has only one solution, both elements of the pair are
--   equal to this solution. 
-- - When the equation has no solutions, both elements of the pair contain NaN.
-- - When the equation has infinitely many solutions, both elements of the pair
--   contain the same solution (0.0).
quadraticEquation :: Double
                  -- ^ The coefficient at the quadratic term of the equation.
                  -> Double
                  -- ^ The coefficient at the linear term of the equation.
                  -> Double
                  -- ^ The constant of the equation.
                  -> (Double, Double)
                  -- ^ The solution(s) of the equation.
quadraticEquation 0.0 0.0 0.0 = (0.0, 0.0)
quadraticEquation 0.0 0.0 _ = (nan, nan)
  where
    nan = 0.0 / 0.0
quadraticEquation 0.0 b c = (s, s)
  where
    s = -(c / b)
quadraticEquation a b c = (min s1 s2, max s1 s2)
  where
    s1 = n1 / base
    s2 = n2 / base
    n1 = (-b) + disc_sqrt
    n2 = (-b) - disc_sqrt
    disc_sqrt = sqrt disc
    disc = b * b - 4 * a * c
    base = 2 * a
