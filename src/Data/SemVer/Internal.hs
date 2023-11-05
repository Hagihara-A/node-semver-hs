module Data.SemVer.Internal where

type RangeSet = [Range]

data Range
  = RangeHyphen Hyphen
  | RangeSimples Simples
  deriving (Show, Eq)

type Simples = [Simple]

data Hyphen = Hyphen Partial Partial
  deriving (Show, Eq)

data Simple
  = SimplePrimitive Primitive
  | SimplePartial Partial
  | SimpleTilde Tilde
  | SimpleCaret Caret
  deriving (Show, Eq)

data Primitive
  = Primitive Compare Partial
  deriving (Show, Eq)

data Compare
  = CompLt
  | CompGt
  | CompGte
  | CompLte
  | CompEq
  deriving (Show, Eq)

data Partial
  = Partial1 Xr
  | Partial2 Xr Xr
  | Partial3 Xr Xr Xr
  | Partial4 Xr Xr Xr Qualifier
  deriving (Show, Eq)

data Xr
  = XrNr Nr
  | XrAny
  deriving (Show, Eq)

data Nr = NrZero | NrDigits Digits
  deriving (Show, Eq)

data Tilde = Tilde Partial
  deriving (Show, Eq)

data Caret = Caret Partial
  deriving (Show, Eq)

data Qualifier = Qualifier (Maybe Pre) (Maybe Build)
  deriving (Show, Eq)

type Pre = Parts

type Build = Parts

-- TODO: Make this non-empty
type Parts = [Part]

data Part = PartNr Nr | PartId IdentifierCharacters
  deriving (Show, Eq)

type IdentifierCharacters = String

type Digits = Int

parseError :: [Token] -> a
parseError tk = error ("Parse Error" ++ (show tk))

data Token
  = TokenIdentifier IdentifierCharacters
  | TokenDigits Digits
  | TokenZero
  | TokenDot
  | TokenPlus
  | TokenHyphen
  | TokenHyphenSep
  | TokenTilde
  | TokenCaret
  | TokenStar
  | TokenLt
  | TokenGt
  | TokenGte
  | TokenLte
  | TokenEq
  | TokenSpaces
  | TokenOr
  | TokenX
  | TokenEOF
  deriving (Show, Eq)
