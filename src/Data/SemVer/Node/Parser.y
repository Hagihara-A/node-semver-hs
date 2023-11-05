{
module Data.SemVer.Node.Parser(parser) where
import Data.SemVer.Node.Internal
import Data.SemVer.Node.Lexer(lexer)
}

%error { parseError }
%tokentype { Token }
%token
    identifier_characters { TokenIdentifier $$ }
    digits { TokenDigits $$ }
    '.' { TokenDot }
    '+' { TokenPlus }
    '-' { TokenHyphen }
    ' - ' { TokenHyphenSep }
    '~' { TokenTilde }
    '^' { TokenCaret }
    '*' { TokenStar }
    '<' { TokenLt }
    '>' { TokenGt }
    '>=' { TokenGte}
    '<=' { TokenLte }
    '=' { TokenEq }
    '||' { TokenOr }
    x { TokenX }
    X { TokenX }

%%

range_set :: { RangeSet }
    : range { [$1] }

range :: { Range }
    : hyphen { RangeHyphen $1 }
    | simples { RangeSimples $1 }

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
    : {- empty -} { Partial0 }
    | any { Partial0 } -- x

    | nr { Partial1 $1 } -- 1
    | nr '.' any { Partial1 $1 } -- 1.x
    | nr '.' any '.' any { Partial1 $1 } -- 1.x.x
    | nr '.' any '.' any qualifier { Partial1Q $1 $6 } -- 1.x.x-pre

    | nr '.' nr '.' any  { Partial2 $1 $3 } -- 1.2.x
    | nr '.' nr '.' any qualifier  { Partial2Q $1 $3 $6 } -- 1.2.x-pre

    | nr '.' nr '.' nr  qualifier { Partial3 $1 $3 $5 $6 } -- 1.2.3-pre

any
    : x { () }
    | X { () }
    | '*' { () }

nr :: { Nr }

tilde :: { Tilde }
    : '~' partial { Tilde $2 }
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
}