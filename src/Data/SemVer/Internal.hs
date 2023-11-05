module Data.SemVer.Internal where

import Data.Char (isSpace)

data ValidSemVer = ValidSemVer VersionCore PreRelease Build
  deriving (Show, Eq)

data VersionCore = VersionCore Major Minor Patch
  deriving (Show, Eq)

type Major = NumericIdentifier

type Minor = NumericIdentifier

type Patch = NumericIdentifier

type PreRelease = DotSeparatedPreReleaseIdentifiers

type DotSeparatedPreReleaseIdentifiers = [PreReleaseIdentifier]

type Build = DotSepBuildIdentifiers

type DotSepBuildIdentifiers = [BuildIdentifier]

data PreReleaseIdentifier
  = PreReleaseIdAlphaNum AlphaNumericIdentifier
  | PreReleaseIdNum NumericIdentifier
  deriving (Show, Eq)

data BuildIdentifier
  = BuildIdAlphaNum AlphaNumericIdentifier
  | BuildIdDigits Digits
  deriving (Show, Eq)

data AlphaNumericIdentifier
  = AlphaNumId1 NonDigit IdentifierCharacters
  | AlphaNumId2 IdentifierCharacters NonDigit IdentifierCharacters
  deriving (Show, Eq)

data NumericIdentifier = NumIdZero | NumId PositiveDigit Digits
  deriving (Show, Eq)

type IdentifierCharacters = [IdentifierCharacter]

data IdentifierCharacter = IdCharDigit Digit | IdCharNonDigit NonDigit
  deriving (Show, Eq)

data NonDigit = NonDigitLetter Letter | NonDigitHyphen
  deriving (Show, Eq)

type Digits = [Digit]

data Digit
  = DigitZero
  | DigitPositive PositiveDigit
  deriving (Show, Eq)

type PositiveDigit = Int

type Letter = String


data Token
  = TokenIdentifier String
  | TokenPositiveDigit Int
  | TokenZero
  | TokenDot
  | TokenPlus
  | TokenHyphen
  | TokenLetter Letter
  | TokenPositiveDigit PositiveDigit
  deriving (Show, Eq)

lexer :: Char -> Token
lexer '.' = TokenDot
lexer '+' = TokenPlus
lexer '-' = TokenHyphen
lexer '0' = TokenZero
lexer '1' = TokenPositiveDigit Token1
lexer '2' = TokenPositiveDigit Token2
lexer '3' = TokenPositiveDigit Token3
lexer '4' = TokenPositiveDigit Token4
lexer '5' = TokenPositiveDigit Token5
lexer '6' = TokenPositiveDigit Token6
lexer '7' = TokenPositiveDigit Token7
lexer '8' = TokenPositiveDigit Token8
lexer '9' = TokenPositiveDigit Token9
lexer 'a' = TokenLetter Token_a
lexer 'b' = TokenLetter Token_b
lexer 'c' = TokenLetter Token_c
lexer 'd' = TokenLetter Token_d
lexer 'e' = TokenLetter Token_e
lexer 'f' = TokenLetter Token_f
lexer 'g' = TokenLetter Token_g
lexer 'h' = TokenLetter Token_h
lexer 'i' = TokenLetter Token_i
lexer 'j' = TokenLetter Token_j
lexer 'k' = TokenLetter Token_k
lexer 'l' = TokenLetter Token_l
lexer 'm' = TokenLetter Token_m
lexer 'n' = TokenLetter Token_n
lexer 'o' = TokenLetter Token_o
lexer 'p' = TokenLetter Token_p
lexer 'q' = TokenLetter Token_q
lexer 'r' = TokenLetter Token_r
lexer 's' = TokenLetter Token_s
lexer 't' = TokenLetter Token_t
lexer 'u' = TokenLetter Token_u
lexer 'v' = TokenLetter Token_v
lexer 'w' = TokenLetter Token_w
lexer 'x' = TokenLetter Token_x
lexer 'y' = TokenLetter Token_y
lexer 'z' = TokenLetter Token_z
lexer 'A' = TokenLetter TokenA
lexer 'B' = TokenLetter TokenB
lexer 'C' = TokenLetter TokenC
lexer 'D' = TokenLetter TokenD
lexer 'E' = TokenLetter TokenE
lexer 'F' = TokenLetter TokenF
lexer 'G' = TokenLetter TokenG
lexer 'H' = TokenLetter TokenH
lexer 'I' = TokenLetter TokenI
lexer 'J' = TokenLetter TokenJ
lexer 'K' = TokenLetter TokenK
lexer 'L' = TokenLetter TokenL
lexer 'M' = TokenLetter TokenM
lexer 'N' = TokenLetter TokenN
lexer 'O' = TokenLetter TokenO
lexer 'P' = TokenLetter TokenP
lexer 'Q' = TokenLetter TokenQ
lexer 'R' = TokenLetter TokenR
lexer 'S' = TokenLetter TokenS
lexer 'T' = TokenLetter TokenT
lexer 'U' = TokenLetter TokenU
lexer 'V' = TokenLetter TokenV
lexer 'W' = TokenLetter TokenW
lexer 'X' = TokenLetter TokenX
lexer 'Y' = TokenLetter TokenY
lexer 'Z' = TokenLetter TokenZ
lexer _ = error "no token match"

tokenize :: String -> [Token]
tokenize =
  foldl
    (\acc c -> if isSpace c then acc else acc ++ [lexer c])
    []