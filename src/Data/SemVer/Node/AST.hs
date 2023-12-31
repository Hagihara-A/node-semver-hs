{-# LANGUAGE StrictData #-}

module Data.SemVer.Node.AST where

import Data.SemVer.Node.Token (Digits, IdentifierCharacters)

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
  = Partial0
  | Partial1 Nr -- 1
  | Partial2 Nr Nr -- 1.2
  | Partial3 Nr Nr Nr Qualifier -- 1.2.3-pre
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
