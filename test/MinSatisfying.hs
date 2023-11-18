module MinSatisfying (minSatisfyingTest) where

import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.SemVer (fromText)
import Data.SemVer.Node (minSatisfying, parseRange)
import Data.Text (Text)
import Test.HUnit

minSatisfyingTest :: Test
minSatisfyingTest =
  TestLabel
    "min satisfying"
    ( TestList
        ( map
            ( \(vers, range, expect) ->
                let constr = fromRight undefined (parseRange range)
                    vers' = map (fromRight undefined . fromText) vers
                 in fromJust (minSatisfying constr vers') ~?= (fromRight undefined . fromText) expect
            )
            fixtures
        )
    )

fixtures :: [([Text], Text, Text)]
fixtures =
  [ (["1.2.3", "1.2.4"], "1.2", "1.2.3"),
    (["1.2.4", "1.2.3"], "1.2", "1.2.3"),
    (["1.2.3", "1.2.4", "1.2.5", "1.2.6"], "~1.2.3", "1.2.3")
  ]