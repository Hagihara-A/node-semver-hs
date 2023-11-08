{
module Data.SemVer.Node.Lexer where
import Data.SemVer.Node.Internal
}

%wrapper "monad"

tokens :-
    $white+ { tk $ const TokenSpaces }
    [\-\+]^[\-a-zA-Z0-9]+ { tk TokenIdentifier }
    "0" { tk $ const $ TokenDigits 0 }
    [1-9][0-9]* { tk $ TokenDigits . read }
    "." { tk $ const TokenDot }
    "-" { tk $ const TokenHyphen }
    " - " { tk $ const TokenHyphenSep }
    "+" { tk $ const TokenPlus }
    "~" { tk $ const TokenTilde }
    "^" { tk $ const TokenCaret }
    "*" { tk $ const TokenStar }
    "<" { tk $ const TokenLt }
    ">" { tk $ const TokenGt }
    ">=" { tk $ const TokenGte }
    "<=" { tk $ const TokenLte }
    "=" { tk $ const TokenEq }
    $white* "||" $white* { tk $ const TokenOr }
    X { tk $ const TokenX }
    x { tk $ const Token_x }

{
alexEOF :: Alex Token
alexEOF = pure TokenEOF

tk :: (String -> Token) -> AlexInput -> Int -> Alex Token
tk tokenizer inp n = pure $ tokenizer (readStr inp n)

readStr :: AlexInput -> Int -> String
readStr (_, _, _, str) n = take n str
}
