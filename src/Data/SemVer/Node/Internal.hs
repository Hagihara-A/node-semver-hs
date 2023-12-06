{-# LANGUAGE StrictData #-}

module Data.SemVer.Node.Internal where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify')
import Data.Bits qualified
import Data.Char (ord)
import Data.SemVer.Node.Token (Token (..))
import Data.Text qualified
import Data.Text.Read (decimal)
import Data.Word (Word8)

-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

data AlexInput = AlexInput
  { -- | current position,
    alexInPosn :: AlexPosn,
    -- | previous char
    alexInPrevC :: Char,
    -- | pending bytes on current char
    alexInBytes :: [Byte],
    -- | current input string
    alexInText :: Data.Text.Text
  }

type Byte = Word8

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexInPrevC

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte AlexInput {alexInBytes = [], alexInText = txt, alexInPosn = p} =
  do
    (c, cs) <- Data.Text.uncons txt
    let p' = alexMove p c
        (b, bs) = utf8Encode' c
     in Just (b, AlexInput {alexInPosn = p', alexInPrevC = c, alexInBytes = bs, alexInText = cs})
alexGetByte inp@AlexInput {alexInBytes = b : bs} = Just (b, inp {alexInBytes = bs})

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
ignorePendingBytes inp = inp {alexInBytes = []}

-- -----------------------------------------------------------------------------
-- Monad

data AlexState = AlexState
  { alexStateInp :: AlexInput,
    alex_scd :: !Int -- the current startcode
  }

runAlex :: Data.Text.Text -> Alex a -> Either String a
runAlex input__ f = evalStateT f initAlexState
  where
    initAlexState =
      AlexState
        { alex_scd = 0,
          alexStateInp =
            AlexInput
              { alexInPosn = alexStartPos,
                alexInBytes = [],
                alexInPrevC = '\n',
                alexInText = input__
              }
        }

type Alex a = StateT AlexState (Either String) a

alexGetInput :: Alex AlexInput
alexGetInput = gets alexStateInp

alexSetInput :: AlexInput -> Alex ()
alexSetInput inp =
  modify'
    (\s -> s {alexStateInp = inp})

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
readTokenDigits AlexInput {alexInText = txt} len = do
  let taken = Data.Text.take len txt
  lift (TokenDigits . fst <$> decimal taken)

readTokenIdentifier :: AlexInput -> Int -> Alex Token
readTokenIdentifier AlexInput {alexInText = txt} len = do
  let taken = Data.Text.take len txt
  pure (TokenIdentifier taken)

parseError :: Token -> Alex a
parseError _ = do
  (AlexInput {alexInPosn = (AlexPn _ line column)}) <- alexGetInput
  alexError ("parse error at line " ++ show line ++ ", column " ++ show column)
