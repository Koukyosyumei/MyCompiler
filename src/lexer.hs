module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

data Token
  = TokEOF
  | TokDEF
  | TokEXTERN
  | TokIDENTIFIER String
  | TokNUMBER Int
  | TokChar Char
  | TokIF
  | TokTHEN
  | TokELSE
  | TokFOR
  | TokIN
  deriving (Eq, Show)

-- | getTok - Return the next token from standard input
getTok :: String -> Int -> (Token, Int)
getTok s i =
  if (length s) <= i
    then (TokEOF, i)
    else if isSpace (s !! i)
           then getTok s (i + 1)
           else if isAlpha (s !! i)
                  then getAlpha s i
                  else if isDigit (s !! i)
                         then getDigit s i
                         else if (s !! i) == '.'
                                then getDigit s i
                                else if (s !! i) == '#'
                                       then getComment s i
                                       else (TokChar (s !! i), i + 1)

getAlpha :: String -> Int -> (Token, Int)
getAlpha s i =
  case (fst alphaStr) of
    "def" -> (TokDEF, snd alphaStr)
    "extern" -> (TokEXTERN, snd alphaStr)
    "if" -> (TokIF, snd alphaStr)
    "then" -> (TokTHEN, snd alphaStr)
    "else" -> (TokELSE, snd alphaStr)
    "for" -> (TokFOR, snd alphaStr)
    "in" -> (TokIN, snd alphaStr)
    identifier -> (TokIDENTIFIER identifier, snd alphaStr)
  where
    alphaStr = getAlphaStr s i

getAlphaStr :: String -> Int -> (String, Int)
getAlphaStr s i =
  if isAlphaNum (s !! i)
    then ([s !! i] ++ (fst nextAS), (snd nextAS))
    else ("", i)
  where
    nextAS = getAlphaStr s (i + 1)

getDigit :: String -> Int -> (Token, Int)
getDigit s i =
  let digitStr = getDigitStr s i
   in (TokNUMBER (read (fst digitStr) :: Int), snd digitStr)

getDigitStr :: String -> Int -> (String, Int)
getDigitStr s i =
  if isDigit (s !! i) || (s !! i) == '.'
    then ([s !! i] ++ (fst nextDS), snd nextDS)
    else ("", i)
  where
    nextDS = getDigitStr s (i + 1)

getComment :: String -> Int -> (Token, Int)
getComment s i =
  if (snd nextCS) < i
    then (TokEOF, i)
    else getTok s (i + 1)
  where
    nextCS = getCommentStr s (i + 1)

getCommentStr :: String -> Int -> (Char, Int)
getCommentStr s i =
  if (length s < (i - 1)) && ((s !! i) /= '\n') && ((s !! i) /= '\r')
    then getCommentStr s (i + 1)
    else (s !! i, i)
