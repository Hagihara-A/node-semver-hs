{
module Data.SemVer.Node.Lexer where
import Data.SemVer.Node.Internal
}

tokens :-
    $white+ { token TokenSpaces }
    [\-\+]^[\-a-zA-Z0-9]+ { readTokenIdentifier }
    "0" { token (TokenDigits 0) }
    [1-9][0-9]* { readTokenDigits }
    "." { token TokenDot }
    "-" { token TokenHyphen }
    " - " { token TokenHyphenSep }
    "+" { token TokenPlus }
    "~" { token TokenTilde }
    "^" { token TokenCaret }
    "*" { token TokenStar }
    "<" { token TokenLt }
    ">" { token TokenGt }
    ">=" { token TokenGte }
    "<=" { token TokenLte }
    "=" { token TokenEq }
    $white* "||" $white* { token TokenOr }
    X { token TokenX }
    x { token Token_x }

{
alexMonadScan :: Alex Token
alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError AlexInput{alexInPosn = (AlexPn _ line column)} ->
      alexError $ "lexical error at line " ++ show line ++ ", column " ++ show column
    AlexSkip inp' _ -> do
      alexSetInput inp'
      alexMonadScan
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) len
}
