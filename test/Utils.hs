module Utils where

import Data.Either (fromRight)
import Data.SemVer (Version, fromText)
import Data.SemVer.Constraint (Constraint)
import Data.SemVer.Node (parseRange)
import Data.Text (Text)

parseV :: Text -> Version
parseV = fromRight undefined . fromText

parseR :: Text -> Constraint
parseR = fromRight undefined . parseRange

