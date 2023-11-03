import Data.SemVer (version)
import Data.SemVer.Parser (parse)
import Test.HUnit
import Data.ByteString qualified as B

testRoot :: Test
testRoot = TestList [testParseSemVer]

testParseSemVer :: Test
testParseSemVer =
  TestLabel "Test parsing simple semver" $
    TestList
      [ parse "1.2.3" ~=? version 1 2 3 [] [],
        parse "1 . 2 .   3" ~=? version 1 2 3 [] [] ]

main :: IO ()
main = runTestTTAndExit testRoot
