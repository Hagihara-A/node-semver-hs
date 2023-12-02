module MinSatisfying (minSatisfyingTest) where

import Data.SemVer.Node (minSatisfying)
import Data.Text (Text, intercalate, unpack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Utils (parseR, parseV)

minSatisfyingTest :: TestTree
minSatisfyingTest =
  testGroup
    "min satisfying"
    ( map
        ( \(vers, range, expect) ->
            let constr = parseR range
                vers' = map parseV vers
             in testCase
                  ((unpack . mconcat) [expect, " is the min version satisfyingw ", range, " in ", intercalate ", " vers])
                  (minSatisfying constr vers' @=? Just (parseV expect))
        )
        fixtures
    )

fixtures :: [([Text], Text, Text)]
fixtures =
  [ (["1.2.3", "1.2.4"], "1.2", "1.2.3"),
    (["1.2.4", "1.2.3"], "1.2", "1.2.3"),
    (["1.2.3", "1.2.4", "1.2.5", "1.2.6"], "~1.2.3", "1.2.3")
  ]