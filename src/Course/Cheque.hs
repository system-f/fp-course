{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Prelude (fromIntegral)

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"

dollars ::
  Chars
  -> Chars
dollars chars =
    cardinal dollarCount ++ " " ++ pluralize "dollar" dollarCount ++
    " and " ++
    cardinal centCount ++ " " ++ pluralize "cent" centCount
  where
    cardinal :: Integer -> Chars
    cardinal n
      | n <    0  = "negative " ++ cardinal (negate n)
      | n ==   0  = "zero"
      | n ==   1  = "one"
      | n ==   2  = "two"
      | n ==   3  = "three"
      | n ==   4  = "four"
      | n ==   5  = "five"
      | n ==   6  = "six"
      | n ==   7  = "seven"
      | n ==   8  = "eight"
      | n ==   9  = "nine"
      | n ==  10  = "ten"
      | n ==  11  = "eleven"
      | n ==  12  = "twelve"
      | n ==  13  = "thirteen"
      | n ==  14  = "fourteen"
      | n ==  15  = "fifteen"
      | n ==  16  = "sixteen"
      | n ==  17  = "seventeen"
      | n ==  18  = "eighteen"
      | n ==  19  = "nineteen"
      | n ==  20  = "twenty"
      | n ==  30  = "thirty"
      | n ==  40  = "forty"
      | n ==  50  = "fifty"
      | n ==  60  = "sixty"
      | n ==  70  = "seventy"
      | n ==  80  = "eighty"
      | n ==  90  = "ninety"
      | n <  100  = let
                      (q, r) = n `quotRem` 10
                    in
                      cardinal (q * 10) ++ "-"
                                        ++ cardinal r
      | n < 1000  = let
                      (q, r) = n `quotRem` 100
                    in
                      cardinal q ++ " hundred"
                                 ++ if r > 0 then " and " ++ cardinal r else ""
      | otherwise = let
                      i = truncLog 1000 n
                      (q, r) = n `quotRem` (1000 ^ i)
                    in
                      cardinal q ++ " "
                                 ++ headOr (show' i) (drop i illion)
                                 ++ if r > 0 then " " ++ cardinal r else ""

    truncLog :: Integer -> Integer -> Integer
    truncLog base n = go 0 n
      where
        go z x | x < base  = z
               | otherwise = go (z + 1) (x `quot` base)

    (^) :: Integer -> Integer -> Integer
    base ^ exp | exp < 0   = unexpectedNegativeExponent
               | otherwise = go 1 exp
      where
        go z 0 = z
        go z n | n > 0 = go (z * base) (n - 1)
               | otherwise = unexpectedNegativeExponent

    unexpectedNegativeExponent = error "Unexpected negative exponent"

    dollarCount :: Integer
    dollarCount = foldLeft (\z x -> z * 10 + fromIntegral x) 0
                . mapInts
                . takeWhile (/= '.')
                $ chars

    centCount :: Integer
    centCount = fromIntegral
              . go 0 10
              . take 2
              . mapInts
              . dropWhile (/= '.')
              $ chars
      where
        go :: Int -> Int -> List Int -> Int
        go z _ Nil       = z
        go z k (x :. xs) = if k > 0
                             then go (z + x * k) (k `div` 10) xs
                             else z

    mapInts :: List Char -> List Int
    mapInts = foldRight
                (\x z -> if isDigit x then digitToInt x :. z else z)
                Nil

    pluralize :: Integral a => Chars -> a -> Chars
    pluralize x 1 = x
    pluralize x _ = x ++ "s"
