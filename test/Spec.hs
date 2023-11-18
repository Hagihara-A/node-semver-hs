module Main (main) where

import MaxSatisfying (maxSatisfyingTest)
import MinSatisfying (minSatisfyingTest)
import RangeExclude (rangeExcludeTest)
import RangeInclude (rangeIncludeTest)
import Test.HUnit

testRoot :: Test
testRoot =
  TestList
    [rangeExcludeTest, rangeIncludeTest, maxSatisfyingTest, minSatisfyingTest]

main :: IO ()
main = runTestTTAndExit testRoot
