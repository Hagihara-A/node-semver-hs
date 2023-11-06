{
module Data.SemVer.Node.Lexer(lexer) where
import Data.SemVer.Node.Internal
}

%wrapper "basic"

tokens :-
    $white+ { const TokenSpaces }
    [\-\+]^[\-a-zA-Z0-9]+ { TokenIdentifier }
    "0" { const $ TokenDigits 0 }
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
    X { const TokenX }
    x { const Token_x }

{
lexer = alexScanTokens
}
