{
module Data.SemVer.Node.Lexer(lexer) where
import Data.SemVer.Node.Internal
}

%wrapper "basic"

tokens :-
    [\-\+]^[\-a-zA-Z0-9]+ { TokenIdentifier }
    [1-9][0-9]* { TokenDigits . read }
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

