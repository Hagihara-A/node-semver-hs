module Main where

import RangeExclude (rangeExcludeTest)
import RangeInclude (rangeIncludeTest)
import Test.HUnit

testRoot :: Test
testRoot =
  TestList
    [rangeExcludeTest, rangeIncludeTest]

main :: IO ()
main = runTestTTAndExit testRoot
