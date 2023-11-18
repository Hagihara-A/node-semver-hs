module MaxSatisfying (maxSatisfyingTest) where

import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.SemVer (fromText)
import Data.SemVer.Node (maxSatisfying, parseRange)
import Data.Text (Text)
import Test.HUnit

maxSatisfyingTest :: Test
maxSatisfyingTest =
  TestLabel
    "max satisfying"
    ( TestList
        ( map
            ( \(vers, range, expect) ->
                let constr = fromRight undefined (parseRange range)
                    vers' = map (fromRight undefined . fromText) vers
                 in fromJust (maxSatisfying constr vers') ~?= (fromRight undefined . fromText) expect
            )
            fixtures
        )
    )

fixtures :: [([Text], Text, Text)]
fixtures =
  [ (["1.2.3", "1.2.4"], "1.2", "1.2.4"),
    (["1.2.4", "1.2.3"], "1.2", "1.2.4"),
    (["1.2.3", "1.2.4", "1.2.5", "1.2.6"], "~1.2.3", "1.2.6")
  ]