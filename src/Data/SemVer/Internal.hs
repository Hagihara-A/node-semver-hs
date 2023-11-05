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
  = Partial0
  | Partial0Q Qualifier -- x.x.x-pre
  | Partial1 Nr -- 1.x
  | Partial1Q Nr Qualifier -- 1.x.x-pre
  | Partial2 Nr Nr -- 1.2.x
  | Partial2Q Nr Nr Qualifier -- 1.2.x-pre
  | Partial3 Nr Nr Nr Qualifier -- 1.2.3-pre
  deriving (Show, Eq)

data Xr
  = XrNr Nr
  | XrAny
  deriving (Show, Eq)

type Nr = Digits

data Tilde = Tilde Partial
  deriving (Show, Eq)

data Caret = Caret Partial
  deriving (Show, Eq)

data Qualifier = Qualifier Pre Build
  deriving (Show, Eq)

type Pre = Parts

type Build = Parts

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
