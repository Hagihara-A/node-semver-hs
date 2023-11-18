-- |
-- This module is for parsing and lexing purpose.
-- You do not have to use this module 99% of cases.
module Data.SemVer.Node.Internal where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify')
import Data.Bits qualified
import Data.Char (ord)
import Data.Text qualified
import Data.Text.Read (decimal)
import Data.Word (Word8)

type RangeSet = [Range]

data Range
  = RangeHyphen Hyphen
  | RangeSimples Simples
  | RangeVoid
  deriving (Show, Eq)

type Simples = [Simple]

data Hyphen = Hyphen Partial Partial
  deriving (Show, Eq)

data Simple
  = SimplePrimitive Primitive
  | SimplePartial Partial
  | SimpleTilde Tilde
  | SimpleCaret Caret
  deriving (Show, Eq)

data Primitive
  = Primitive Compare Partial
  deriving (Show, Eq)

data Compare
  = CompLt
  | CompGt
  | CompGte
  | CompLte
  | CompEq
  deriving (Show, Eq)

data Partial
  = Partial0
  | Partial1 Nr -- 1
  | Partial2 Nr Nr -- 1.2
  | Partial3 Nr Nr Nr Qualifier -- 1.2.3-pre
  deriving (Show, Eq)

type Nr = Digits

newtype Tilde = Tilde Partial
  deriving (Show, Eq)

newtype Caret = Caret Partial
  deriving (Show, Eq)

data Qualifier = Qualifier Pre Build
  deriving (Show, Eq)

type Pre = Parts

type Build = Parts

type Parts = [Part]

data Part = PartNr Nr | PartId IdentifierCharacters
  deriving (Show, Eq)

type IdentifierCharacters = Data.Text.Text

type Digits = Int

data Token
  = TokenIdentifier IdentifierCharacters
  | TokenDigits Digits
  | TokenDot
  | TokenPlus
  | TokenHyphen
  | TokenHyphenSep
  | TokenTilde
  | TokenCaret
  | TokenStar
  | TokenLt
  | TokenGt
  | TokenGte
  | TokenLte
  | TokenEq
  | TokenOr
  | TokenX
  | Token_x
  | TokenSpaces
  | TokenEOF
  deriving (Show, Eq)

-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

type AlexInput =
  ( AlexPosn, -- current position,
    Char, -- previous char
    [Byte], -- pending bytes on current char
    Data.Text.Text -- current input string
  )

type Byte = Word8

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _, _) = c

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (p, c, b : bs, s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) = case Data.Text.uncons s of
  Just (c, cs) ->
    let p' = alexMove p c
     in case utf8Encode' c of
          (b, bs) -> p' `seq` Just (b, (p', c, bs, cs))
  Nothing ->
    Nothing

-- alex provides the following functions to the user:
-- alexScan ::
--   AlexInput -> -- The current input
--   Int -> -- The "start code"
--   AlexReturn action -- The return value

-- data AlexReturn action
--   = AlexEOF
--   | AlexError
--       !AlexInput -- Remaining input
--   | AlexSkip
--       !AlexInput -- Remaining input
--       !Int -- Token length
--   | AlexToken
--       !AlexInput -- Remaining input
--       !Int -- Token length
--       action -- action value

-- type AlexAction result = AlexInput -> Int -> Alex result
-- { ... }  :: AlexAction result
-- type AlexAction result = AlexInput -> Int -> Alex result

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
  (x, xs) -> (fromIntegral x, map fromIntegral xs)
  where
    go oc
      | oc <= 0x7f =
          ( oc,
            []
          )
      | oc <= 0x7ff =
          ( 0xc0 + (oc `Data.Bits.shiftR` 6),
            [ 0x80 + oc Data.Bits..&. 0x3f
            ]
          )
      | oc <= 0xffff =
          ( 0xe0 + (oc `Data.Bits.shiftR` 12),
            [ 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f),
              0x80 + oc Data.Bits..&. 0x3f
            ]
          )
      | otherwise =
          ( 0xf0 + (oc `Data.Bits.shiftR` 18),
            [ 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f),
              0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f),
              0x80 + oc Data.Bits..&. 0x3f
            ]
          )

data AlexPosn
  = AlexPn
      -- | the address (number of characters preceding the token)
      !Int
      -- | the line number of this token
      !Int
      -- | the column number of this token
      !Int
  deriving (Eq, Show, Ord)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a + 1) l (c + alex_tab_size - ((c - 1) `mod` alex_tab_size))
  where
    alex_tab_size = 8
alexMove (AlexPn a l _) '\n' = AlexPn (a + 1) (l + 1) 1
alexMove (AlexPn a l c) _ = AlexPn (a + 1) l (c + 1)

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p, c, _, s) = (p, c, [], s)

-- -----------------------------------------------------------------------------
-- Monad

data AlexState = AlexState
  { alex_pos :: !AlexPosn, -- position at current input location
    alex_inp :: Data.Text.Text,
    alex_chr :: !Char,
    alex_bytes :: [Byte],
    alex_scd :: !Int -- the current startcode
  }

runAlex :: Data.Text.Text -> Alex a -> Either String a
runAlex input__ f = evalStateT f initAlexState
  where
    initAlexState =
      AlexState
        { alex_bytes = [],
          alex_pos = alexStartPos,
          alex_inp = input__,
          alex_chr = '\n',
          alex_scd = 0
        }

type Alex a = StateT AlexState (Either String) a

alexGetInput :: Alex AlexInput
alexGetInput = gets (\s -> (alex_pos s, alex_chr s, alex_bytes s, alex_inp s))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos, c, bs, inp) =
  modify'
    (\s -> s {alex_pos = pos, alex_chr = c, alex_bytes = bs, alex_inp = inp})

alexError :: String -> Alex a
alexError message = lift $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = gets alex_scd

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = modify' (\s -> s {alex_scd = sc})

alexEOF :: Alex Token
alexEOF = pure TokenEOF

token :: Token -> AlexInput -> Int -> Alex Token
token tk _ _ = pure tk

readTokenDigits :: AlexInput -> Int -> Alex Token
readTokenDigits (_, _, _, txt) len = do
  let taken = Data.Text.take len txt
  lift (TokenDigits . fst <$> decimal taken)

readTokenIdentifier :: AlexInput -> Int -> Alex Token
readTokenIdentifier (_, _, _, txt) len = do
  let taken = Data.Text.take len txt
  pure (TokenIdentifier taken)

takeInputText :: AlexInput -> Int -> Data.Text.Text
takeInputText (_, _, _, txt) _ = txt
