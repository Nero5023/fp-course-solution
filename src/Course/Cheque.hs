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
import Course.State
import qualified Data.Set as S

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
  deriving (Eq, Enum, Bounded, Show)

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
  deriving (Eq, Show)

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
dollars xs = let (int, decimal) = splitByDot $ filterNum xs
                 intStr = showDollorOrCents int
                 decimalStr = showDollorOrCents decimal
                 dollarStr = if intStr == "one " then "dollar and " else "dollars and "
                 centStr = if decimalStr == "one " then "cent" else "cents"
             in intStr ++ dollarStr ++ decimalStr ++ centStr


-- Filter the input to the right type of number
-- The output is in "xxxxx.xx" type
filterNum :: Chars -> Chars
filterNum xs =
  eval ((filtering (\x -> State $ \(hasDot, numFromDot) -> 
                              ((isDigit x || ((x == '.') && not hasDot)) && numFromDot < 2, 
                                nextState x (hasDot, numFromDot))) xs) >>=
    (\numStr -> State $ \(is, num) -> (fill numStr is num, (True, 2)))) (False, 0)
  where nextState x (hasDot, numFromDot) =
          let nextHasDot = 
                (hasDot || (x == '.'))
              nextNum =
                if hasDot && isDigit x then numFromDot + 1 else numFromDot
              in (nextHasDot, nextNum)
        fill str hasDot numFromDot
          | not hasDot = str ++ ".00"
          | numFromDot < 2 = str ++ replicate (2 - numFromDot) '0'
          | otherwise = str

-- Split the number string to (int, decimal)
splitByDot :: Chars -> (Chars, Chars)
splitByDot ('.':.rs) = (Nil, rs)
splitByDot (x:.xs)   = let (int, decimal) = splitByDot xs
                        in (x:.int, decimal)
splitByDot _         = error "Input must have '.'"



-- Fill the length of string to 3 multiple and make it group every 3 char

groupToEvery3 :: Chars -> List Chars
groupToEvery3 str = reverse $ group3r (reverse str)
  where group3r (x:.y:.z:.xs) = (z:.y:.x:.Nil) :. group3r xs
        group3r (x:.y:.Nil)   = (y:.x:.Nil) :. Nil
        group3r (x:.Nil)      = (x:.Nil) :. Nil
        group3r Nil           = Nil

-- make the right num str to Digits
toDigit3s :: Chars -> List Digit3
toDigit3s str = map ch3ToDigit3 (groupToEvery3 str)
  where ch3ToDigit3 xs = 
           case sequence $ map fromChar xs of Full (x:.y:.z:.Nil) -> D3 x y z
                                              Full (x:.y:.Nil) -> D2 x y
                                              Full (x:.Nil) -> D1 x
                                              Full _ -> error "Unexcepted error"
                                              Empty -> error "Unexcepted error"


-- show the lower case of digit
showLowD :: Digit -> Chars
showLowD digit = toLower <$> showDigit digit
  
-- connect the decade and unit
(+|) :: Chars -> Digit -> Chars
(+|) str Zero = str
(+|) str digit = str ++ ('-' :. showLowD digit)

-- Show the digit3
showDigit3 :: Digit3 -> Chars
showDigit3 (D1 a) = showLowD a 
showDigit3 (D2 Zero b) = showLowD b
showDigit3 (D2 One b) = 
  case b of Zero -> "ten"
            One -> "eleven"
            Two -> "twelve"
            Three -> "thirteen"
            Four -> "fourteen"
            Five -> "fifteen"
            Six -> "sixteen"
            Seven -> "seventeen"
            Eight -> "eighteen"
            Nine -> "nineteen"
showDigit3 (D2 Two b) = "twenty" +| b
showDigit3 (D2 Three b) = "thirty" +| b
showDigit3 (D2 Four b) = "forty" +| b
showDigit3 (D2 Five b) = "fifty" +| b
showDigit3 (D2 Six b) = "sixty" +| b
showDigit3 (D2 Seven b) = "seventy" +| b
showDigit3 (D2 Eight b) = "eighty" +| b
showDigit3 (D2 Nine b) = "ninety" +| b
showDigit3 (D3 Zero Zero Zero) = ""
showDigit3 (D3 Zero a b) = showDigit3 (D2 a b)
showDigit3 (D3 a Zero Zero) = showLowD a ++ " hundred"
showDigit3 (D3 a b c) = showLowD a ++ " hundred and " ++ showDigit3 (D2 b c)

-- show the int or decim 
showDollorOrCents :: Chars -> Chars
showDollorOrCents Nil = "zero "
showDollorOrCents xs = join $ reverse $ zipWith connect (reverse $ map showDigit3 $ toDigit3s xs) illion
  where connect "" _ = "" :: Chars
        connect num ill = num ++ " " ++ ill ++ if ill =="" then "" else " "