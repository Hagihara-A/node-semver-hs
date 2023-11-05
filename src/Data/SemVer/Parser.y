{
module Data.SemVer.Parser(fromString) where
import Data.SemVer(version)
import Data.SemVer.Internal
import Data.SemVer.Lexer(alexScanTokens)
}

%name parse range_set
%error { parseError }
%tokentype { Token }
%token
    identifier_characters { TokenIdentifier $$ }
    digits { TokenDigits $$ }
    '0' { TokenZero }
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
    spaces { TokenSpaces }
    x { TokenX }
    X { TokenX }

%%

range_set :: { RangeSet }
    : range { [$1] }
    | range_set logical_or range { $3:$1 }

logical_or :: { () }
    : spaces '||' spaces { () }
    | spaces '||' { () }
    | '||' spaces { () } 
    | '||' { () }

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
    : xr { Partial1 $1 }
    | xr '.' xr { Partial2 $1 $3}
    | xr '.' xr '.' xr { Partial3 $1 $3 $5}
    | xr '.' xr '.' xr qualifier { Partial4 $1 $3 $5 $6}

xr :: { Xr }
    : X { XrAny }
    | x { XrAny }
    | '*' { XrAny }
    | nr { XrNr $1 }

nr :: { Nr }
    : '0' { NrZero }
    | digits { NrDigits $1 }

tilde :: { Tilde }
    : '~' partial { Tilde $2 }
caret :: { Caret }
    : '^' partial { Caret $2 }

qualifier :: { Qualifier }
    : {- empty -} { Qualifier Nothing Nothing }
    | '-' pre { Qualifier (Just $2) Nothing }
    | '+' build { Qualifier (Nothing) (Just $2) }
    | '-' pre '+' build { Qualifier (Just $2) (Just $4) }

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
fromString = parse . alexScanTokens
}