module Main where

import Data.SemVer (version)
import Data.SemVer.Node (satisfies)
import RangeExclude (rangeExcludeTest)
import RangeInclude (rangeIncludeTest)
import RangeParse (rangeParseTest)
import Test.HUnit

testRoot :: Test
testRoot =
  TestList
    [rangeExcludeTest, rangeIncludeTest, rangeParseTest]

main :: IO ()
main = runTestTTAndExit testRoot
