{
module Data.SemVer.Parser(parse) where
import Data.SemVer(version)
import Data.SemVer.Internal
}

%name analyze 'valid semver'
%tokentype { Token }
%error { parseError }

%token
    'letter' { TokenLetter $$ }
    'positive digit' { TokenPositiveDigit $$ }
    '0' { TokenZero }
    '.' { TokenDot }
    '+' { TokenPlus }
    '-' { TokenHyphen }


%%

'valid semver' :: { ValidSemVer }
    : 'version core' { ValidSemVer $1 [] [] }
    | 'version core' '-' 'pre-release' { ValidSemVer $1 $3 []}
    | 'version core' '+' 'build' { ValidSemVer $1 [] $3 }
    | 'version core' '-' 'pre-release' '+' 'build' { ValidSemVer $1 $3 $5} 

'version core' :: { VersionCore }
     : 'major' '.' 'minor' '.' 'patch' {VersionCore $1 $3 $5}

'major' : 'numeric identifier' {$1}

'minor' : 'numeric identifier' {$1}

'patch' : 'numeric identifier' {$1}

'pre-release' :: { PreRelease }
    : 'dot-separated pre-release identifiers' {$1}

'dot-separated pre-release identifiers' :: { DotSeparatedPreReleaseIdentifiers }
    : 'pre-release identifier' {[$1]}
    | 'pre-release identifier' '.' 'dot-separated pre-release identifiers' {$1:$3}

'build' : 'dot-separated build identifiers' {$1}

'dot-separated build identifiers' :: { DotSepBuildIdentifiers }
    : 'build identifier' {[$1]}
    | 'build identifier' '.' 'dot-separated build identifiers' { $1:$3 }

'pre-release identifier' :: { PreReleaseIdentifier }
    : 'alphanumeric identifier' { PreReleaseIdAlphaNum $1 }
    | 'numeric identifier' { PreReleaseIdNum $1}

'build identifier' :: { BuildIdentifier }
    : 'alphanumeric identifier' {BuildIdAlphaNum $1}
    | 'digits' {BuildIdDigits $1}

'alphanumeric identifier' 
    : 'non-digit' {AlphaNumId1 $1 []}
    | 'non-digit' 'identifier characters' {AlphaNumId1 $1 $2}
    | 'identifier characters' 'non-digit' { AlphaNumId2 $1 $2 []}
    | 'identifier characters' 'non-digit' 'identifier characters' { AlphaNumId2 $1 $2 $3}

'numeric identifier' 
    : '0' { NumIdZero }
    | 'positive digit' { NumId $1 [] }
    | 'positive digit' 'digits' { NumId $1 $2}

'identifier characters'
    : 'identifier character' { [$1] }
    | 'identifier character' 'identifier characters' { $1:$2 }

'identifier character' 
    : 'digit' { IdCharDigit $1 }
    | 'non-digit' { IdCharNonDigit $1}

'non-digit' :: { NonDigit }
    : 'letter' { NonDigitLetter $1 }
    | '-' { NonDigitHyphen }

'digits' :: { Digits }
    : 'digit' {[$1]}
    | 'digit' 'digits' {$1:$2}

'digit' :: { Digit }
    : '0' { DigitZero }
    | 'positive digit' {DigitPositive $1}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> ValidSemVer
parse = analyze . tokenize
}