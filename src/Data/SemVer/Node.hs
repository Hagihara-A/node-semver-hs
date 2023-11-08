-- | This is a almost node-semver compliant parsing and comparing module.
module Data.SemVer.Node
  ( parseRange,
  )
where

import Data.Maybe (fromJust)
import Data.SemVer
  ( Identifier,
    Version,
    numeric,
    textual,
    version,
  )
import Data.SemVer.Constraint
  ( Constraint (..),
  )
import Data.SemVer.Node.Internal
import Data.SemVer.Node.Parser (parser)
import Data.Text (Text, pack, unpack)

parseRange :: Text -> Either String Constraint
parseRange s = rangeSetToConstraint <$> parser (unpack s)

rangeSetToConstraint :: RangeSet -> Constraint
rangeSetToConstraint [] = CAny
rangeSetToConstraint (r1 : rr) =
  foldr (COr . toConstraint) (toConstraint r1) rr

class ToConstraint a where
  toConstraint :: a -> Constraint

instance ToConstraint Range where
  toConstraint (RangeHyphen hyphen) = toConstraint hyphen
  toConstraint (RangeSimples simples) = foldr (CAnd . toConstraint) CAny simples
  toConstraint RangeVoid = CAny

instance ToConstraint Hyphen where
  toConstraint
    (Hyphen p1 p2) =
      case upper of
        Nothing -> CGtEq lower
        Just v -> case p2 of
          Partial3 {} -> CAnd (CGtEq lower) (CLtEq v)
          _ -> CAnd (CGtEq lower) (CLt v)
      where
        lower = minPartial p1
        upper = leastUpperBound p2

instance ToConstraint Simple where
  toConstraint (SimplePrimitive primitive) = toConstraint primitive
  toConstraint (SimplePartial partial) = toConstraint partial
  toConstraint (SimpleTilde tilde) = toConstraint tilde
  toConstraint (SimpleCaret caret) = toConstraint caret

instance ToConstraint Primitive where
  toConstraint (Primitive CompLt p) = CLt (minPartial p)
  toConstraint (Primitive CompGt p) = case upperBound p of
    -- Left (P3 v) -> CGt v
    Nothing -> CLt $ version 0 0 0 pre0 []
    Just v -> case p of
      Partial3 {} -> CGt v
      _ -> CGtEq v
  toConstraint (Primitive CompGte p) = CGtEq (minPartial p)
  toConstraint (Primitive CompLte p) = case leastUpperBound p of
    -- Left (P3 v) -> CLtEq v
    Nothing -> CAny
    Just v -> case p of
      Partial3 {} -> CLtEq v
      _ -> CLt v
  toConstraint (Primitive CompEq p) = toConstraint p

instance ToConstraint Partial where
  toConstraint
    ( Partial3
        nr1
        nr2
        nr3
        (Qualifier pre build)
      ) = CEq (version nr1 nr2 nr3 (partsToId pre) (partsToId build))
  toConstraint (Partial2 nr1 nr2) = CAnd (CGtEq lower) (CLt upper)
    where
      lower = version nr1 nr2 0 [] []
      upper = version nr1 (nr2 + 1) 0 [] []
  toConstraint (Partial1 nr1) = CAnd (CGtEq lower) (CLt upper)
    where
      lower = version nr1 0 0 [] []
      upper = version (nr1 + 1) 0 0 pre0 []
  toConstraint Partial0 = CAny

instance ToConstraint Tilde where
  toConstraint
    (Tilde (Partial3 nr1 nr2 nr3 (Qualifier pre _))) =
      CAnd (CGtEq lower) (CLt upper)
      where
        lower = version nr1 nr2 nr3 (partsToId pre) []
        upper = version nr1 (nr2 + 1) 0 pre0 []
  toConstraint (Tilde p) = toConstraint p

instance ToConstraint Caret where
  toConstraint
    (Caret (Partial3 0 0 nr3 (Qualifier pre _))) =
      CAnd (CGtEq lower) (CLt upper)
      where
        lower = version 0 0 nr3 (partsToId pre) []
        upper = version 0 0 (nr3 + 1) pre0 []
  toConstraint
    (Caret (Partial3 0 nr2 nr3 (Qualifier pre _))) =
      CAnd (CGtEq lower) (CLt upper)
      where
        lower = version 0 nr2 nr3 (partsToId pre) []
        upper = version 0 (nr2 + 1) 0 pre0 []
  toConstraint
    (Caret (Partial3 nr1 nr2 nr3 (Qualifier pre _))) =
      CAnd (CGtEq lower) (CLt upper)
      where
        lower = version nr1 nr2 nr3 (partsToId pre) []
        upper = version (nr1 + 1) 0 0 pre0 []
  toConstraint
    (Caret (Partial2 0 nr2)) =
      CAnd (CGtEq lower) (CLtEq upper)
      where
        lower = version 0 nr2 0 [] []
        upper = version 0 (nr2 + 1) 0 pre0 []
  toConstraint
    (Caret (Partial2 nr1 nr2)) =
      CAnd (CGtEq lower) (CLtEq upper)
      where
        lower = version nr1 nr2 0 [] []
        upper = version (nr1 + 1) 0 0 pre0 []
  toConstraint
    (Caret (Partial1 nr1)) =
      CAnd (CGtEq lower) (CLtEq upper)
      where
        lower = version nr1 0 0 [] []
        upper = version (nr1 + 1) 0 0 pre0 []
  toConstraint (Caret Partial0) = CAny

-- | Example: 1.2 -> 1.3.0
-- Note that this is not least upper bound. Least upper bound is 1.3.0-0.
upperBound :: Partial -> Maybe Version
upperBound Partial0 = Nothing
upperBound (Partial1 nr1) = Just $ version (nr1 + 1) 0 0 [] []
upperBound (Partial2 nr1 nr2) = Just $ version nr1 (nr2 + 1) 0 [] []
upperBound (Partial3 nr1 nr2 nr3 (Qualifier pre _)) =
  Just $ version nr1 nr2 nr3 (partsToId pre) []

-- | Example: 1.2 -> 1.3.0-0
leastUpperBound :: Partial -> Maybe Version
leastUpperBound Partial0 = Nothing
leastUpperBound (Partial1 nr1) = Just $ version (nr1 + 1) 0 0 pre0 []
leastUpperBound (Partial2 nr1 nr2) = Just $ version nr1 (nr2 + 1) 0 pre0 []
leastUpperBound (Partial3 nr1 nr2 nr3 (Qualifier pre _)) =
  Just $ version nr1 nr2 nr3 (partsToId pre) [] -- In this case, given partial contains exactly one element. Thus, least upper bound is same as the element.

minPartial :: Partial -> Version
minPartial Partial0 = version 0 0 0 [] []
minPartial (Partial1 nr1) = version nr1 0 0 [] []
minPartial (Partial2 nr1 nr2) = version nr1 nr2 0 [] []
minPartial (Partial3 nr1 nr2 nr3 (Qualifier pre _)) = version nr1 nr2 nr3 (partsToId pre) []

pre0 :: [Identifier]
pre0 = [numeric 0]

partsToId :: Parts -> [Identifier]
partsToId = map partToIdentifier

partToIdentifier :: Part -> Identifier
partToIdentifier (PartNr nr) = numeric nr
partToIdentifier (PartId str) = (fromJust . textual . pack) str
