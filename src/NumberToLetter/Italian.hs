module NumberToLetter.Italian (numberToLetter) where

import NumberToLetter.ConversionRules
import Control.Arrow ((>>>))
import Data.List (isSuffixOf)

numberToLetter = (resultOf $ joinWith " "
    [ specialCase 0 "zero"
    , suffixPowerOfTen 9 numberToLetter " miliardi" "un miliarde"
    , suffixPowerOfTen 6 numberToLetter " milioni" "un milione"
    , joinWith ""
        [ joinWith ""
            [ suffixPowerOfTen 3 numberToLetter "mila" "mille"
            , conditionalElement " " afterThreeSignificantThousandDigits
            ] LastToFirst
        ,  suffixPowerOfTen 2 numberToLetter "cento" "cento"
        ,  specialCase 3 "tre"
        ,  literal twenty_to_ninety 20 99 10
        ,  literal ten_to_nineteen 10 19 1
        ,  literal one_to_nine 1 9 1
        ] FirstToLast
    ] FirstToLast)
    >>> replace ("ta" ++ "o") "to"
    >>> replace ("ta" ++ "u") "tu"
    >>> replace ("ti" ++ "o") "to"
    >>> replace ("ti" ++ "u") "tu"

afterThreeSignificantThousandDigits number =
    let thousands = number `quot` 1000
    in thousands > 100 && (thousands `rem` 100) > 0

one_to_nine =
    [ "uno"
    , "due"
    , "tré"
    , "quattro"
    , "cinque"
    , "sei"
    , "sette"
    , "otto"
    , "nove"
    ]
ten_to_nineteen =
    [ "dieci"
    , "undici"
    , "dodici"
    , "tredici"
    , "quattordici"
    , "quindici"
    , "sedici"
    , "diciasette"
    , "diciotto"
    , "diciannove"
    ]
twenty_to_ninety =
    [ "venti"
    , "trenta"
    , "quaranta"
    , "cinquanta"
    , "sessanta"
    , "settanta"
    , "ottanta"
    , "novanta"
    ]
