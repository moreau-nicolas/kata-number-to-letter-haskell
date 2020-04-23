module NumberToLetter.Dutch (numberToLetter) where

import NumberToLetter.ConversionRules
import Control.Arrow ((>>>))

numberToLetter = (resultOf $ joinWith " "
    [ specialCase 0 "nul"
    , suffixPowerOfTen 9 numberToLetter " miljard" "een miljard"
    , suffixPowerOfTen 6 numberToLetter " miljoen" "een miljoen"
    , suffixPowerOfTen 3 numberToLetter "duizend" "duizend"
    , suffixPowerOfTen 2 numberToLetter "honderd" "honderd"
    , joinWith "en"
        [ literal one_to_nine 1 9 1
        , literal twenty_to_ninety 20 99 10
        ] LastToFirst
    , literal ten_to_nineteen 10 19 1
    ] FirstToLast)
    >>> replace "tweeen" "tweeën"
    >>> replace "drieen" "drieën"

one_to_nine =
    [ "een"
    , "twee"
    , "drie"
    , "vier"
    , "vijf"
    , "zes"
    , "zeven"
    , "acht"
    , "negen"
    ]
ten_to_nineteen =
    [ "tien"
    , "elf"
    , "twaalf"
    , "dertien"
    , "veertien"
    , "vijftien"
    , "zestien"
    , "zeventien"
    , "achttien"
    , "negentien"
    ]
twenty_to_ninety =
    [ "twintig"
    , "dertig"
    , "veertig"
    , "vijftig"
    , "zestig"
    , "zeventig"
    , "tachtig"
    , "negentig"
    ]
