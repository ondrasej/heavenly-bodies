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

import HBodies.Math
import Test.Hspec

main = hspec $do
  describe "quadraticEquation" $do
    it "Solves quadratic equations" $do
      let (s1, s2) = quadraticEquation 1.0 (-3.0) (-4.0)
      s1 `shouldBe` (-1.0)
      s2 `shouldBe` 4.0
    it "Solves linear equations" $do
      let (s1, s2) = quadraticEquation 0.0 2.0 (-10.0)
      s1 `shouldBe` 5.0
      s2 `shouldBe` 5.0
    it "Returns NaN when the equation has no solution" $do
      let (s1, s2) = quadraticEquation 1.0 0.0 1.0
      s1 `shouldSatisfy` (isNaN)
      s2 `shouldSatisfy` (isNaN)
      let (s1, s2) = quadraticEquation 0.0 0.0 1.0
      s1 `shouldSatisfy` (isNaN)
      s2 `shouldSatisfy` (isNaN)
    it "Returns 0.0 when the equation has infinitely many solutions" $do
      let (s1, s2) = quadraticEquation 0.0 0.0 0.0
      s1 `shouldBe` 0.0
      s2 `shouldBe` 0.0
