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

import Control.Monad (forM_)
import HBodies.Geometry
import qualified HBodies.Time as Time
import Test.Hspec

isAlmost c x = abs (x - c) < 1e-15

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

  describe "isCollision" $do
    let p1 = position 0.0 0.0 0.0
        p2 = position 0.0 10.0 0.0
    it "Detects collisions" $do
      isCollision (p1, 6.0) (p2, 6.0) `shouldBe` True
    it "Does not report false negatives" $do
      isCollision (p1, 1.0) (p2, 2.0) `shouldBe` False
    it "Does not report collision on touch" $do
      isCollision (p1, 5.0) (p2, 5.0) `shouldBe` False

  describe "collisionTime" $do
    it "Computes collision time when they collide" $do
      let p1 = position 0.0 0.0 0.0
          d1 = direction 1.0 0.0 0.0
          r1 = 1.0
          p2 = position 5.0 0.0 0.0
          d2 = direction (-1.0) 0.0 0.0
          r2 = 2.0
          ctime = collisionTime p1 d1 r1 p2 d2 r2
      ctime `shouldBe` Just (Time.durationSeconds 1.0)
    it "Computes collision time when they touch" $do
      let p1 = position 0.0 0.0 0.0
          d1 = direction 0.0 0.0 0.0
          r1 = 1.0
          p2 = position 10.0 2.0 0.0
          d2 = direction (-1.0) 0.0 0.0
          r2 = 1.0
          ctime = collisionTime p1 d1 r1 p2 d2 r2
      ctime `shouldBe` Just (Time.durationSeconds 10.0)
    it "Detects when they miss each other in time" $do
      let p1 = position 10.0 0.0 0.0
          d1 = direction (-1.0) 0.0 0.0
          r1 = 0.5
          p2 = position 0.0 5.0 0.0
          d2 = direction 0.0 (-1.0) 0.0
          r2 = 0.5
      collisionTime p1 d1 r1 p2 d2 r2 `shouldBe` Nothing
    it "Detects when they move in opposite directions" $do
      let p1 = position 0.0 0.0 0.0
          d1 = direction 0.0 (-1.0) 0.0
          r1 = 0.5
          p2 = position 0.0 5.0 0.0
          d2 = direction 0.0 1.0 0.0
          r2 = 0.5
          ctime = collisionTime p1 d1 r1 p2 d2 r2
      ctime `shouldBe` Just (Time.durationSeconds (-3.0))
    it "Detects when they move in the same direction" $do
      let p1 = position 0.0 0.0 0.0
          d1 = direction 1.0 0.0 0.0
          r1 = 0.1
          p2 = position 1.0 0.0 0.0
          d2 = direction 1.0 0.0 0.0
          r2 = 0.1
      collisionTime p1 d1 r1 p2 d2 r2 `shouldBe` Nothing

  describe "direction" $do
    it "Creates a new direction" $do
      let dir = direction 1.0 2.0 3.0
      getDx dir `shouldBe` 1.0
      getDy dir `shouldBe` 2.0
      getDRotation dir `shouldBe` 3.0

  describe "directionRadial" $do
    it "Creates a new position" $do
      let dir = directionRadial 0.0 1.0 2.0
      getDx dir `shouldBe` 1.0
      getDy dir `shouldBe` 0.0
      getDRotation dir `shouldBe` 2.0

      let otherDir = directionRadial (pi / 2.0) 2.0 4.0
      abs (getDx otherDir) `shouldSatisfy` (< 1e-15)
      getDy otherDir `shouldBe` 2.0
      getDRotation otherDir `shouldBe` 4.0

  describe "positionDelta" $do
    it "computes the delta" $do
      let p1 = position 1.0 2.0 0.5
          p2 = position 4.0 3.0 0.0
          d = positionDelta p1 p2
      getDx d `shouldBe` 3.0
      getDy d `shouldBe` 1.0
      getDRotation d `shouldBe` (-0.5)

  describe "addDirection" $do
    it "Adds direction to a position" $do
      let p = position 1.0 2.0 3.0
          d = direction (-2.0) 3.0 (-1.0)
          p' = addDirection p d
      getX p' `shouldBe` (-1.0)
      getY p' `shouldBe` 5.0
      getRotation p' `shouldBe` 2.0

  describe "+." $do
    it "Adds directions" $do
      let d1 = direction 1.0 2.0 3.0
          d2 = direction 4.0 5.0 6.0
          d = d1 +. d2
      getDx d `shouldBe` 5.0
      getDy d `shouldBe` 7.0
      getDRotation d `shouldBe` 9.0

  describe "-." $do
    it "Computes the difference" $do
      let d1 = direction 1.0 5.0 3.0
          d2 = direction (-2.0) 3.0 1.5
          d = d1 -. d2
      getDx d `shouldBe` 3.0
      getDy d `shouldBe` 2.0
      getDRotation d `shouldBe` 1.5

  describe "*." $do
    it "Multiplies direction"$ do
      let d = direction 1.0 2.0 (1.5 * pi)
          md = 2.0 *. d
      getDx md `shouldBe` 2.0
      getDy md `shouldBe` 4.0
      getDRotation md `shouldBe` 3.0 * pi

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
          dir = direction 1.0 2.0 3.0
          dt = Time.durationSeconds 1.0
          updated = updatePosition dt pos dir
      getX updated `shouldBe` 1.0
      getY updated `shouldBe` 2.0
      getRotation updated `shouldBe` 3.0

  describe "bouncedDirection" $do
    it "Bounces from axis aligned planes" $do
      let dir = direction 0.0 (-1.0) 2.0
          norm = direction 0.0 1.0 0.0
          bounced = bouncedDirection dir norm
      getDx bounced `shouldBe` 0.0
      getDy bounced `shouldBe` 1.0
      getDRotation bounced `shouldBe` 2.0
    it "Bounces from axis aligned planes 2" $do
      let dir = direction 0.0 1.0 3.0
          norm = direction 0.0 1.0 0.0
          bounced = bouncedDirection dir norm
      getDx bounced `shouldBe` 0.0
      getDy bounced `shouldBe` (-1.0)
      getDRotation bounced `shouldBe` 3.0
    it "Bounces from unaligned planes" $do
      let dir = direction 1.0 0.0 0.0
          norm = direction (-1.0) 1.0 0.0
          bounced = bouncedDirection dir norm
      getDx bounced `shouldSatisfy` isAlmost 0.0
      getDy bounced `shouldSatisfy` isAlmost 1.0
      getDRotation bounced `shouldBe` 0.0

  describe "directionNorm" $do
    it "Computes the norm" $do
      let dir = direction 3.0 4.0 1.0
      directionNorm dir `shouldBe` 5.0
    it "Can handle zero direction" $do
      let dir = direction 0.0 0.0 1.0
      directionNorm dir `shouldBe` 0.0

  describe "unitDirection" $do
    it "Preserves the direction" $do
      let dir = direction 3.0 4.0 1.0
          unit = unitDirection dir
      getDx unit `shouldBe` 3.0 / 5.0
      getDy unit `shouldBe` 4.0 / 5.0
      getDRotation unit `shouldBe` 1.0
    it "Does not crash with zero direction" $do
      let dir = direction 0.0 0.0 0.0
      unitDirection dir `shouldBe` dir
    it "Normalizes the length to 1" $do
      let dirs = [direction 1.0 2.0 3.0,
                  direction (-4.0) 2.1 3.2,
                  direction 10.0 1.0 0.0]
      forM_ dirs $ \dir -> do
          directionNorm (unitDirection dir) `shouldSatisfy` isAlmost 1.0

  describe "dotProduct" $do
    it "Computes the dot product" $do
      let d1 = direction 1.0 10.0 2.0
          d2 = direction 0.5 1.0 3.0
      dotProduct d1 d2 `shouldBe` 10.5
