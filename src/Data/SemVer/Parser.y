{
module Data.SemVer.Parser(parse) where
import Data.SemVer(version)
import Data.SemVer.Internal
}

%name analyze valid_semver
%error { parseError }

%token
    letter { TokenIdentifier $$ }
    positive_digit { TokenPositiveDigit $$ }
    '0' { TokenZero }
    '.' { TokenDot }
    '+' { TokenPlus }
    '-' { TokenHyphen }


%%

valid_semver :: { ValidSemVer }
    : version_core { ValidSemVer $1 [] [] }
    | version_core '-' pre_release { ValidSemVer $1 $3 []}
    | version_core '+' build { ValidSemVer $1 [] $3 }
    | version_core '-' pre_release '+' build { ValidSemVer $1 $3 $5} 

version_core :: { VersionCore }
     : major '.' minor '.' patch {VersionCore $1 $3 $5}

major : numeric_identifier {$1}

minor : numeric_identifier {$1}

patch : numeric_identifier {$1}

pre_release :: { PreRelease }
    : dot_separated_pre_release_identifiers {$1}

dot_separated_pre_release_identifiers :: { DotSeparatedPreReleaseIdentifiers }
    : pre_release_identifier {[$1]}
    | pre_release_identifier '.' dot_separated_pre_release_identifiers {$1:$3}

build : dot_separated_build_identifiers {$1}

dot_separated_build_identifiers :: { DotSepBuildIdentifiers }
    : build_identifier {[$1]}
    | build_identifier '.' dot_separated_build_identifiers { $1:$3 }

pre_release_identifier :: { PreReleaseIdentifier }
    : alphanumeric_identifier { PreReleaseIdAlphaNum $1 }
    | numeric_identifier { PreReleaseIdNum $1}

build_identifier :: { BuildIdentifier }
    : alphanumeric_identifier {BuildIdAlphaNum $1}
    | digits {BuildIdDigits $1}

alphanumeric_identifier 
    : non_digit {AlphaNumId1 $1 []}
    | non_digit identifier_characters {AlphaNumId1 $1 $2}
    | identifier_characters non_digit { AlphaNumId2 $1 $2 []}
    | identifier_characters non_digit identifier_characters { AlphaNumId2 $1 $2 $3}

version_identifier
    : x { VerIdAny }
    | X { VerIdAny }
    | '*' { VerIdAny }
    | numeric_identifier { VerIdNum $1 }

numeric_identifier
    : '0' { NumIdZero }
    | positive_digit { NumId $1 [] }
    | positive_digit digits { NumId $1 $2}

identifier_characters
    : identifier_character { [$1] }
    | identifier_character identifier_characters { $1:$2 }

identifier_character 
    : digit { IdCharDigit $1 }
    | non_digit { IdCharNonDigit $1}

non_digit :: { NonDigit }
    : letter { NonDigitLetter $1 }
    | '-' { NonDigitHyphen }

digits :: { Digits }
    : digit {[$1]}
    | digit digits {$1:$2}

digit :: { Digit }
    : '0' { DigitZero }
    | positive_digit { DigitPositive $1 }

{
parse = makeParse analyze
}