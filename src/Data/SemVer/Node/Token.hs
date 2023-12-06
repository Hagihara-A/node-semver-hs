{-# LANGUAGE StrictData #-}

module Data.SemVer.Node.Token where

import Data.Text qualified

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
  | TokenEOF
  deriving (Show, Eq)

type IdentifierCharacters = Data.Text.Text

type Digits = Int