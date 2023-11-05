{
module Data.SemVer.Lexer where
import Data.SemVer.Internal
}

%wrapper "basic"

tokens :-
    $white { const TokenSpaces }
    [\-\+]^[\-a-zA-Z0-9]+ { TokenIdentifier }
    [1-9][0-9]* { TokenDigits . read }
    0 { const TokenZero }
    "." { const TokenDot }
    "-" { const TokenHyphen }
    " - " { const TokenHyphenSep }
    "+" { const TokenPlus }
    "~" { const TokenTilde }
    "^" { const TokenCaret }
    "*" { const TokenStar }
    "<" { const TokenLt }
    ">" { const TokenGt }
    ">=" { const TokenGte }
    "<=" { const TokenLte }
    "=" { const TokenEq }
    "||" { const TokenOr }
    x | X { const TokenX }

