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

import HBodies.Geometry
import qualified HBodies.Time as Time
import Test.Hspec

main = hspec $do
  describe "position" $do
    it "Creates a new position" $do
      let pos = position 1.0 2.0 3.0
      getX pos `shouldBe` 1.0
      getY pos `shouldBe` 2.0
      getRotation pos `shouldBe` 3.0

      let otherPos = position (-14.0) 3.0 0.1
      getX otherPos `shouldBe` (-14.0)
      getY otherPos `shouldBe` 3.0
      getRotation otherPos `shouldBe` 0.1

  describe "radialPosition" $do
    it "Creates a new position" $do
      let pos = positionRadial 0.0 1.0 3.0
      getX pos `shouldBe` 1.0
      getY pos `shouldBe` 0.0
      getRotation pos `shouldBe` 3.0

      let otherPos = positionRadial (pi / 2.0) 2.0 4.0
      abs (getX otherPos) `shouldSatisfy` (< 1e-15)
      getY otherPos `shouldBe` 2.0
      getRotation otherPos `shouldBe` 4.0

  describe "addPosition" $do
    it "Adds positions" $do
      let p1 = position 1.0 2.0 3.0
          p2 = position 4.0 5.0 6.0
          p = addPosition p1 p2
      getX p `shouldBe` 5.0
      getY p `shouldBe` 7.0
      getRotation p `shouldBe` 9.0

  describe "isCollision" $do
    let p1 = position 0.0 0.0 0.0
        p2 = position 0.0 10.0 0.0
    it "Detects collisions" $do
      isCollision (p1, 6.0) (p2, 6.0) `shouldBe` True
    it "Does not report false negatives" $do
      isCollision (p1, 1.0) (p2, 2.0) `shouldBe` False
    it "Does not report collision on touch" $do
      isCollision (p1, 5.0) (p2, 5.0) `shouldBe` False

  describe "velocity" $do
    it "Creates a new velocity" $do
      let vel = velocity 1.0 2.0 3.0
      getDx vel `shouldBe` 1.0
      getDy vel `shouldBe` 2.0
      getDRotation vel `shouldBe` 3.0

  describe "velocityRadial" $do
    it "Creates a new position" $do
      let vel = velocityRadial 0.0 1.0 2.0
      getDx vel `shouldBe` 1.0
      getDy vel `shouldBe` 0.0
      getDRotation vel `shouldBe` 2.0

      let otherVel = velocityRadial (pi / 2.0) 2.0 4.0
      abs (getDx otherVel) `shouldSatisfy` (< 1e-15)
      getDy otherVel `shouldBe` 2.0
      getDRotation otherVel `shouldBe` 4.0

  describe "addVelocity" $do
    it "Adds velocities" $do
      let v1 = velocity 1.0 2.0 3.0
          v2 = velocity 4.0 5.0 6.0
          v = addVelocity v1 v2
      getDx v `shouldBe` 5.0
      getDy v `shouldBe` 7.0
      getDRotation v `shouldBe` 9.0

  describe "normalizeRotation" $do
    it "Does not change rotation that is in bounds" $do
      normalizeRotation 0.0 `shouldBe` 0.0
      normalizeRotation 1.0 `shouldBe` 1.0

    it "Updates rotation above bound" $do
      normalizeRotation 4.0 `shouldBe` 4.0 - 2*pi
      normalizeRotation 10.0 `shouldBe` 10.0 - 4*pi

    it "Updates rotation below bound" $do
      normalizeRotation (-4.0) `shouldBe` 2*pi - 4.0
      normalizeRotation (-10.0) `shouldBe` 4*pi - 10.0

  describe "updatePosition" $do
    it "Updates the position" $do
      let pos = position 0.0 0.0 0.0
          vel = velocity 1.0 2.0 3.0
          dt = Time.durationSeconds 1.0
          updated = updatePosition dt pos vel
      getX updated `shouldBe` 1.0
      getY updated `shouldBe` 2.0
      getRotation updated `shouldBe` 3.0
