module MaxSatisfying (maxSatisfyingTest) where

import Data.SemVer.Node (maxSatisfying)
import Data.Text (Text, intercalate, unpack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Utils (parseR, parseV)

maxSatisfyingTest :: TestTree
maxSatisfyingTest =
  testGroup
    "max satisfying"
    ( map
        ( \(vers, range, expect) ->
            let constr = parseR range
                vers' = map parseV vers
             in testCase
                  ((unpack . mconcat) [expect, " is the max version satisfying ", range, " in ", intercalate ", " vers])
                  (maxSatisfying constr vers' @=? Just (parseV expect))
        )
        fixtures
    )

fixtures :: [([Text], Text, Text)]
fixtures =
  [ (["1.2.3", "1.2.4"], "1.2", "1.2.4"),
    (["1.2.4", "1.2.3"], "1.2", "1.2.4"),
    (["1.2.3", "1.2.4", "1.2.5", "1.2.6"], "~1.2.3", "1.2.6")
  ]