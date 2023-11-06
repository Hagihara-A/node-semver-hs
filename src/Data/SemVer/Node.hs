-- | This is a almost node-semver compliant parsing and comparing module.
module Data.SemVer.Node
  ( satisfies,
    -- validRange,
    parseRange,
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
  ( Constraint (CAnd, CAny, CEq, CGtEq, CLt, CLtEq, COr),
  )
import Data.SemVer.Node.Internal
import Data.SemVer.Node.Parser (parser)
import Data.Text (pack)

parseRange = parser

rangeSetToConstraint :: RangeSet -> Constraint
rangeSetToConstraint [] = CAny
rangeSetToConstraint (head : rest) =
  foldr (COr . toConstraint) (toConstraint head) rest

class ToConstraint a where
  toConstraint :: a -> Constraint

instance ToConstraint Range where
  toConstraint (RangeHyphen hyphen) = toConstraint hyphen
  toConstraint (RangeSimples simples) = foldr (CAnd . toConstraint) CAny simples
  toConstraint RangeVoid = CAny

instance ToConstraint Hyphen where
  toConstraint (Hyphen p1 p2) = CAnd (CGtEq $ infPartial p1) (CLt $ supPartial p2)

instance ToConstraint Simple where
  toConstraint (SimplePrimitive primitive) = toConstraint primitive
  toConstraint (SimplePartial partial) = toConstraint partial
  toConstraint (SimpleTilde tilde) = toConstraint tilde
  toConstraint (SimpleCaret caret) = toConstraint caret

instance ToConstraint Primitive where
  toConstraint (Primitive CompLt p) = CLt (infPartial p)
  toConstraint (Primitive CompGt p) = CGtEq (supPartial p)
  toConstraint (Primitive CompGte p) = CGtEq (infPartial p)
  toConstraint (Primitive CompLte p) = CLt (supPartial p)
  toConstraint (Primitive CompEq p) = toConstraint p

instance ToConstraint Partial where
  toConstraint
    ( Partial3
        (XrNr nr1)
        (XrNr nr2)
        (XrNr nr3)
        (Qualifier pre build)
      ) = CEq (version nr1 nr2 nr3 (partsToId pre) (partsToId build))
  toConstraint p = CAnd (CGtEq $ infPartial p) (CLt $ supPartial p)

instance ToConstraint Tilde where
  toConstraint
    ( Tilde
        ( Partial3
            (XrNr nr1)
            (XrNr nr2)
            (XrNr nr3)
            (Qualifier pre _)
          )
      ) = CAnd (CGtEq $ version nr1 nr2 nr3 (partsToId pre) []) (CLt $ version nr1 (nr2 + 1) 0 [numeric 0] [])
  toConstraint (Tilde p) = toConstraint p

instance ToConstraint Caret where
  toConstraint
    ( Caret
        ( Partial3
            (XrNr 0)
            (XrNr 0)
            (XrNr nr3)
            (Qualifier pre _)
          )
      ) = CAnd (CGtEq $ version 0 0 nr3 (partsToId pre) []) (CLt $ version 0 0 (nr3 + 1) [numeric 0] [])
  toConstraint
    ( Caret
        ( Partial3
            (XrNr 0)
            (XrNr nr2)
            (XrNr nr3)
            (Qualifier pre _)
          )
      ) = CAnd (CGtEq $ version 0 nr2 nr3 (partsToId pre) []) (CLt $ version 0 (nr2 + 1) 0 [numeric 0] [])
  toConstraint
    ( Caret
        ( Partial3
            (XrNr nr1)
            (XrNr nr2)
            (XrNr nr3)
            (Qualifier pre _)
          )
      ) = CAnd (CGtEq $ version nr1 nr2 nr3 (partsToId pre) []) (CLt $ version (nr1 + 1) 0 0 [numeric 0] [])
  toConstraint
    ( Caret
        ( Partial3
            (XrNr 0)
            (XrNr nr2)
            _
            _
          )
      ) = CAnd (CGtEq $ version 0 nr2 0 [] []) (CLtEq $ version 0 (nr2 + 1) 0 [numeric 0] [])
  toConstraint
    ( Caret
        ( Partial3
            (XrNr nr1)
            (XrNr nr2)
            _
            _
          )
      ) = CAnd (CGtEq $ version nr1 nr2 0 [] []) (CLtEq $ version (nr1 + 1) 0 0 [numeric 0] [])
  toConstraint
    ( Caret
        ( Partial2
            (XrNr 0)
            (XrNr nr2)
          )
      ) = CAnd (CGtEq $ version 0 nr2 0 [] []) (CLtEq $ version 0 (nr2 + 1) 0 [numeric 0] [])
  toConstraint
    ( Caret
        ( Partial2
            (XrNr nr1)
            (XrNr nr2)
          )
      ) = CAnd (CGtEq $ version nr1 nr2 0 [] []) (CLtEq $ version (nr1 + 1) 0 0 [numeric 0] [])
  toConstraint
    ( Caret
        ( Partial3
            (XrNr 0)
            _
            _
            _
          )
      ) = CLt $ version 1 0 0 [numeric 0] []
  toConstraint
    ( Caret
        ( Partial3
            (XrNr nr1)
            _
            _
            _
          )
      ) = CAnd (CGtEq $ version nr1 0 0 [] []) (CLt $ version (nr1 + 1) 0 0 [numeric 0] [])
  toConstraint
    ( Caret
        ( Partial2
            (XrNr nr1)
            _
          )
      ) = CAnd (CGtEq $ version nr1 0 0 [] []) (CLt $ version (nr1 + 1) 0 0 [numeric 0] [])
  toConstraint
    ( Caret
        ( Partial1
            (XrNr nr1)
          )
      ) = CAnd (CGtEq $ version nr1 0 0 [] []) (CLt $ version (nr1 + 1) 0 0 [numeric 0] [])
  toConstraint (Caret (Partial3 {})) = CAny
  toConstraint (Caret Partial2 {}) = CAny
  toConstraint (Caret Partial1 {}) = CAny

supPartial :: Partial -> Version
supPartial (Partial1 (XrNr nr1)) = version (nr1 + 1) 0 0 [numeric 0] []
supPartial (Partial1 (XrNr nr1)) = version (nr1 + 1) 0 0 [numeric 0] []

infPartial :: Partial -> Version
infPartial = undefined

partsToId :: Parts -> [Identifier]
partsToId = map partToIdentifier

partToIdentifier :: Part -> Identifier
partToIdentifier (PartNr nr) = numeric nr
partToIdentifier (PartId str) = (fromJust . textual . pack) str

-- validRange :: RangeSet -> RangeSet
-- validRange = map normalizeRange

-- normalizeHyphen :: Hyphen -> Range
-- normalizeHyphen = undefined

-- normalizePrimitive :: Primitive -> Primitive
-- normalizePrimitive = undefined

-- normalizeRange :: Range -> Range
-- normalizeRange (RangeHyphen hyphen) = undefined
-- normalizeRange (RangeSimples simples) = undefined
-- normalizeRange RangeVoid = RangeSimples [SimplePartial (Partial1 XrStar)]

-- normalizeSimple :: Simple -> Range
-- normalizeSimple (SimplePrimitive primitive) = undefined
-- normalizeSimple (SimplePartial partial) = undefined
-- normalizeSimple (SimpleTilde (Tilde partial)) = undefined
-- normalizeSimple (SimpleCaret (Caret partial)) = undefined

-- normalizeTildePartial :: Partial -> Range
-- normalizeTildePartial (Partial1 (XrNr nr1)) =
--   RangeSimples
--     [ SimplePrimitive (Primitive CompGte undefined),
--       SimplePrimitive (Primitive CompLt undefined)
--     ]
-- normalizeTildePartial (Partial1 _) =
--   RangeSimples
--     [ SimplePartial (Partial1 XrStar)
--     ]
-- normalizeTildePartial (Partial2 (XrNr nr1) (XrNr nr2)) = undefined
-- normalizeTildePartial (Partial2 (XrNr nr1) _) =
--   RangeSimples
--     [ SimplePrimitive (Primitive CompGte undefined),
--       SimplePrimitive (Primitive CompLt undefined)
--     ]
-- normalizeTildePartial (Partial2 _ _) =
--   RangeSimples
--     [ SimplePartial (Partial1 XrStar)
--     ]
-- normalizeTildePartial (Partial3) = undefined
-- normalizeTildePartial _ = undefined

satisfies :: Version -> RangeSet -> Bool
satisfies = undefined

-- normalizePartial :: Partial -> Range
-- normalizePartial (Partial1 (XrNr nr)) = undefined
-- normalizePartial (Partial1 _) = RangeVoid
