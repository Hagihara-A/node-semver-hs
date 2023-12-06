{
module Data.SemVer.Node.Parser(parser) where
import Data.SemVer.Node.Lexer(alexMonadScan)
import Data.SemVer.Node.Internal
import Data.SemVer.Node.AST
import Data.SemVer.Node.Token
}
%expect 0
%monad { Alex } { >>= } { pure }
%lexer { lexer } { TokenEOF }
%name analyze range_set
%error { parseError }
%tokentype { Token }
%token
    identifier_characters { TokenIdentifier $$ }
    digits { TokenDigits $$ }
    '.' { TokenDot }
    '+' { TokenPlus }
    '-' { TokenHyphen }
    ' - ' { TokenHyphenSep }
    spaces { TokenSpaces }
    '~' { TokenTilde }
    '^' { TokenCaret }
    '*' { TokenStar }
    '<' { TokenLt }
    '>' { TokenGt }
    '>=' { TokenGte}
    '<=' { TokenLte }
    '=' { TokenEq }
    '||' { TokenOr }
    '_x' { Token_x }
    X { TokenX }

%%

range_set :: { RangeSet }
    : range { [$1] }
    | range_set logical_or range { $3:$1 }

logical_or
    : '||' { () }

range :: { Range }
    : hyphen { RangeHyphen $1 }
    | simples { RangeSimples $1 }
    | {- empty -} { RangeVoid }

simples :: { Simples }
    : simple { [$1] }
    | simples spaces simple { $3:$1 }

hyphen :: { Hyphen }
    : partial ' - ' partial { Hyphen $1 $3}

simple :: { Simple }
    : primitive { SimplePrimitive $1 }
    | partial { SimplePartial $1 }
    | tilde { SimpleTilde $1 }
    | caret { SimpleCaret $1 }

primitive :: { Primitive }
    : compare partial { Primitive $1 $2}

compare :: { Compare }
    : '<' { CompLt }
    | '>' { CompGt }
    | '>=' { CompGte }
    | '<=' { CompLte }
    | '=' { CompEq }

partial :: { Partial }
    : x { Partial0 }
    | x '.' xr { Partial0 } 
    | x '.' xr '.' xr { Partial0 } 
    | nr { Partial1 $1 }
    | nr '.' x { Partial1 $1 }
    | nr '.' x '.' xr { Partial1 $1 }
    | nr '.' nr  { Partial2 $1 $3 }
    | nr '.' nr '.' x  { Partial2 $1 $3 }
    | nr '.' nr '.' nr qualifier { Partial3 $1 $3 $5 $6 }

xr
    : x { () } 
    | nr { () }
x
    : '_x' { () }
    | X { () }
    | '*' { () }



nr :: { Nr }
    : digits { $1 }

tilde :: { Tilde }
    : '~' partial { Tilde $2 }
    | '~' spaces partial { Tilde $3 }

caret :: { Caret }
    : '^' partial { Caret $2 }

qualifier :: { Qualifier }
    : {- empty -} { Qualifier [] [] }
    | '-' pre { Qualifier $2 [] }
    | '+' build { Qualifier [] $2 }
    | '-' pre '+' build { Qualifier $2 $4 }

pre :: { Pre }
    : parts { $1 }

build :: { Build }
    : parts { $1 }

parts :: { Parts }
    : parts '.' part { $3:$1 }
    | part { [$1] }

part :: { Part }
    : nr { PartNr $1 }
    | identifier_characters { PartId $1 }

{
parser s = runAlex s analyze

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}