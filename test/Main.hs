module Main (main) where

import MaxSatisfying (maxSatisfyingTest)
import MinSatisfying (minSatisfyingTest)
import RangeExclude (rangeExcludeTest)
import RangeInclude (rangeIncludeTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

testRoot :: TestTree
testRoot =
  testGroup
    "node-semver"
    [rangeExcludeTest, rangeIncludeTest, maxSatisfyingTest, minSatisfyingTest]

main :: IO ()
main = defaultMain testRoot
