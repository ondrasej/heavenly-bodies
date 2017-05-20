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

import Control.Monad
import HBodies.Time
import Test.Hspec

main = hspec $do
  describe "startTime" $do
    it "Starts at zero" $do
      startTime `shouldBe` (timeSeconds 0.0)

  describe "timeSeconds and secondsFromTime" $do
    it "timeSeconds . secondsFromTime is identity" $do
      let times = [1.0, 2.0, 3.0, 0.5]
      forM_ times $ \time -> do
        time `shouldBe` (secondsFromTime . timeSeconds) time

    it "supports equality" $do
      timeSeconds 1.0 `shouldBe` timeSeconds 1.0
      timeSeconds 1.0 `shouldNotBe` timeSeconds 2.0

    it "supports ordering" $do
      timeSeconds 1.0 `shouldSatisfy` (< timeSeconds 2.0)
      timeSeconds 1.0 `shouldSatisfy` (<= timeSeconds 1.0)

  describe "durationSeconds and secondsFromDuration" $do
    it "durationSeconds . secondsFromDuration is identity" $do
      let durations = [1.0, 2.0, 3.0, -5.0, 6.0, 0.0]
      forM_ durations $ \duration -> do
          duration `shouldBe` (secondsFromDuration . durationSeconds) duration

    it "supports equality" $do
      durationSeconds 1.0 `shouldBe` durationSeconds 1.0
      durationSeconds 1.0 `shouldNotBe` durationSeconds 2.0

    it "supports ordering" $do
      durationSeconds 0.0 `shouldSatisfy` (< durationSeconds 2.0)
      durationSeconds 1.0 `shouldSatisfy` (<= durationSeconds 1.0)

  describe "add" $do
    it "adds time" $do
      let t = timeSeconds 1.0
          d = durationSeconds 5.0
      add t d `shouldBe` timeSeconds 6.0

  describe "diff" $do
    it "computes the difference" $do
      let t1 = timeSeconds 10.0
          t2 = timeSeconds 3.0
      diff t1 t2 `shouldBe` durationSeconds 7.0
