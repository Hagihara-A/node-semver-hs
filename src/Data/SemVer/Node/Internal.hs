-- |
-- This module is for parsing and lexing purpose.
-- You do not have to use this module 99% of cases.
module Data.SemVer.Node.Internal where

type RangeSet = [Range]

data Range
  = RangeHyphen Hyphen
  | RangeSimples Simples
  | RangeVoid
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
  = Partial1 Xr -- 1
  | Partial2 Xr Xr -- 1.2
  | Partial3 Xr Xr Xr Qualifier -- 1.2.3-pre
  deriving (Show, Eq)

data Xr
  = XrNr Nr
  | XrX
  | Xr_x
  | XrStar
  deriving (Show, Eq)

type Nr = Digits

newtype Tilde = Tilde Partial
  deriving (Show, Eq)

newtype Caret = Caret Partial
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
parseError tk = error ("Parse Error" ++ show tk)

data Token
  = TokenIdentifier IdentifierCharacters
  | TokenDigits Digits
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
  | TokenOr
  | TokenX
  | Token_x
  | TokenSpaces
  deriving (Show, Eq)
