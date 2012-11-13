{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its trascribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module L10.Cheque where

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Instances

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  [String]
illion =
  let preillion ::
        [String -> String]
      preillion =
        [
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
        [String]
      postillion =
        [
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
  in [
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
     ] ++ liftM2 ((++) =<<) preillion postillion

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
  deriving (Show, Eq, Enum, Bounded)

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Maybe Digit
fromChar '0' =
  Just Zero
fromChar '1' =
  Just One
fromChar '2' =
  Just Two
fromChar '3' =
  Just Three
fromChar '4' =
  Just Four
fromChar '5' =
  Just Five
fromChar '6' =
  Just Six
fromChar '7' =
  Just Seven
fromChar '8' =
  Just Eight
fromChar '9' =
  Just Nine
fromChar _ =
  Nothing

dollars ::
  String
  -> String
dollars =
  error "todo"

-- Examples of input and output.
test ::
  IO ()
test =
  mapM_ (\(i, o) -> let r = dollars i
                    in when (o /= r) (mapM_ putStrLn [i, '\t':o, '\t':r]))
    [
      ("0", "zero dollars and zero cents")
    , ("1", "one dollar and zero cents")
    , ("0.1", "zero dollars and ten cents")
    , ("1.", "one dollar and zero cents")
    , ("0.", "zero dollars and zero cents")
    , ("0.0", "zero dollars and zero cents")
    , (".34", "zero dollars and thirty-four cents")
    , ("0.3456789", "zero dollars and thirty-four cents")
    , ("1.0", "one dollar and zero cents")
    , ("1.01", "one dollar and one cent")
    , ("a1a", "one dollar and zero cents")
    , ("a1a.a0.7b", "one dollar and seven cents")
    , ("100", "one hundred dollars and zero cents")
    , ("100.0", "one hundred dollars and zero cents")
    , ("100.00", "one hundred dollars and zero cents")
    , ("100.00000", "one hundred dollars and zero cents")
    , ("1000456.13", "one million four hundred and fifty-six dollars and thirteen cents")
    , ("1001456.13", "one million one thousand four hundred and fifty-six dollars and thirteen cents")
    , ("16000000456.13", "sixteen billion four hundred and fifty-six dollars and thirteen cents")
    , ("100.45", "one hundred dollars and forty-five cents")
    , ("100.07", "one hundred dollars and seven cents")
    , ("9abc9def9ghi.jkl9mno", "nine hundred and ninety-nine dollars and ninety cents")
    , ("12345.67", "twelve thousand three hundred and forty-five dollars and sixty-seven cents")
    , ("456789123456789012345678901234567890123456789012345678901234567890.12", "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents")
    ]