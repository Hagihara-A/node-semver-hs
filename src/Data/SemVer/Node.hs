-- | This is a almost node-semver compliant parsing and comparing module.
module Data.SemVer.Node
  ( parseRange,
    maxSatisfying,
    minSatisfying,
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
    satisfies,
  )
import Data.SemVer.Node.AST
import Data.SemVer.Node.Parser (parser)
import Data.Text (Text)

maxSatisfying :: Constraint -> [Version] -> Maybe Version
maxSatisfying constr vers = findSatisfying max (filter (`satisfies` constr) vers)

minSatisfying :: Constraint -> [Version] -> Maybe Version
minSatisfying constr vers = findSatisfying min (filter (`satisfies` constr) vers)

findSatisfying :: (Foldable t) => (a -> a -> a) -> t a -> Maybe a
findSatisfying selector =
  foldr
    ( \e acc -> case acc of
        Nothing -> Just e
        Just acc' -> Just $ selector e acc'
    )
    Nothing

parseRange :: Text -> Either String Constraint
parseRange txt = rangeSetToConstraint <$> parser txt

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
    Nothing -> CLt leastVersion
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
      ) = CEq (version' nr1 nr2 nr3 pre build)
  toConstraint (Partial2 nr1 nr2) = CAnd (CGtEq lower) (CLt upper)
    where
      lower = versionCore nr1 nr2 0
      upper = versionCore nr1 (nr2 + 1) 0
  toConstraint (Partial1 nr1) = CAnd (CGtEq lower) (CLt upper)
    where
      lower = versionCore nr1 0 0
      upper = versionCorePre0 (nr1 + 1) 0 0
  toConstraint Partial0 = CAny

instance ToConstraint Tilde where
  toConstraint
    (Tilde (Partial3 nr1 nr2 nr3 (Qualifier pre _))) =
      CAnd (CGtEq lower) (CLt upper)
      where
        lower = version' nr1 nr2 nr3 pre []
        upper = versionCorePre0 nr1 (nr2 + 1) 0
  toConstraint (Tilde p) = toConstraint p

instance ToConstraint Caret where
  toConstraint
    (Caret (Partial3 0 0 nr3 (Qualifier pre _))) =
      CAnd (CGtEq lower) (CLt upper)
      where
        lower = version' 0 0 nr3 pre []
        upper = versionCorePre0 0 0 (nr3 + 1)
  toConstraint
    (Caret (Partial3 0 nr2 nr3 (Qualifier pre _))) =
      CAnd (CGtEq lower) (CLt upper)
      where
        lower = version' 0 nr2 nr3 pre []
        upper = versionCorePre0 0 (nr2 + 1) 0
  toConstraint
    (Caret (Partial3 nr1 nr2 nr3 (Qualifier pre _))) =
      CAnd (CGtEq lower) (CLt upper)
      where
        lower = version' nr1 nr2 nr3 pre []
        upper = versionCorePre0 (nr1 + 1) 0 0
  toConstraint
    (Caret (Partial2 0 nr2)) =
      CAnd (CGtEq lower) (CLtEq upper)
      where
        lower = versionCore 0 nr2 0
        upper = versionCorePre0 0 (nr2 + 1) 0
  toConstraint
    (Caret (Partial2 nr1 nr2)) =
      CAnd (CGtEq lower) (CLtEq upper)
      where
        lower = versionCore nr1 nr2 0
        upper = versionCorePre0 (nr1 + 1) 0 0
  toConstraint
    (Caret (Partial1 nr1)) =
      CAnd (CGtEq lower) (CLtEq upper)
      where
        lower = versionCore nr1 0 0
        upper = versionCorePre0 (nr1 + 1) 0 0
  toConstraint (Caret Partial0) = CAny

-- | Example: 1.2 -> 1.3.0
-- Note that this is not least upper bound. Least upper bound is 1.3.0-0.
upperBound :: Partial -> Maybe Version
upperBound Partial0 = Nothing
upperBound (Partial1 nr1) = Just $ versionCore (nr1 + 1) 0 0
upperBound (Partial2 nr1 nr2) = Just $ versionCore nr1 (nr2 + 1) 0
upperBound (Partial3 nr1 nr2 nr3 (Qualifier pre _)) =
  Just $ version' nr1 nr2 nr3 pre []

-- | Example: 1.2 -> 1.3.0-0
leastUpperBound :: Partial -> Maybe Version
leastUpperBound Partial0 = Nothing
leastUpperBound (Partial1 nr1) = Just $ versionCorePre0 (nr1 + 1) 0 0
leastUpperBound (Partial2 nr1 nr2) = Just $ versionCorePre0 nr1 (nr2 + 1) 0
leastUpperBound (Partial3 nr1 nr2 nr3 (Qualifier pre _)) =
  Just $ version' nr1 nr2 nr3 pre [] -- In this case, given partial contains exactly one element. Thus, least upper bound is same as the element.

minPartial :: Partial -> Version
minPartial Partial0 = versionCore 0 0 0
minPartial (Partial1 nr1) = versionCore nr1 0 0
minPartial (Partial2 nr1 nr2) = versionCore nr1 nr2 0
minPartial (Partial3 nr1 nr2 nr3 (Qualifier pre _)) = version' nr1 nr2 nr3 pre []

partsToId :: Parts -> [Identifier]
partsToId = map partToIdentifier
  where
    partToIdentifier :: Part -> Identifier
    partToIdentifier (PartNr nr) = numeric nr
    partToIdentifier (PartId str) = (fromJust . textual) str

version' :: Int -> Int -> Int -> Parts -> Parts -> Version
version' a b c pre build = version a b c (partsToId pre) (partsToId build)

versionCorePre0 :: Int -> Int -> Int -> Version
versionCorePre0 a b c = version a b c pre0 []
  where
    pre0 = [numeric 0]

versionCore :: Int -> Int -> Int -> Version
versionCore a b c = version a b c [] []

leastVersion :: Version
leastVersion = versionCorePre0 0 0 0