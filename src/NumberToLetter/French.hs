module NumberToLetter.French (numberToLetter) where

import NumberToLetter.ConversionRules
import Control.Arrow ((>>>))

numberToLetter = (resultOf $ joinWith "-"
    [ specialCase 0 "zéro"
    , suffixPowerOfTen 9 numberToLetter "-milliards" "un-milliard"
    , suffixPowerOfTen 6 numberToLetter "-millions" "un-million"
    , suffixPowerOfTen 3 numberToLetter "-mille" "mille"
    , specialCase 900 "neuf-cents"
    , specialCase 800 "huit-cents"
    , specialCase 700 "sept-cents"
    , specialCase 600 "six-cents"
    , specialCase 500 "cinq-cents"
    , specialCase 400 "quatre-cents"
    , specialCase 300 "trois-cents"
    , specialCase 200 "deux-cents"
    , suffixPowerOfTen 2 numberToLetter "-cent" "cent"
    , specialCase 80 "quatre-vingts"
    , specialCase 71 "soixante-et-onze"
    , specialCase 61 "soixante-et-un"
    , specialCase 51 "cinquante-et-un"
    , specialCase 41 "quarante-et-un"
    , specialCase 31 "trente-et-un"
    , specialCase 21 "vingt-et-un"
    , literal ["quatre-vingt"] 80 99 1
    , literal ["soixante"] 60 79 1
    , literal twenty_to_fifty 20 59 10
    , literal eleven_to_sixteen 11 16 1
    , literal ["dix"] 10 19 1
    , literal one_to_nine 1 9 1
    ] FirstToLast)
    >>> replace "cents-mille" "cent-mille"
    >>> replace "quatre-vingts-mille" "quatre-vingt-mille"

one_to_nine =
    [ "un"
    , "deux"
    , "trois"
    , "quatre"
    , "cinq"
    , "six"
    , "sept"
    , "huit"
    , "neuf"
    ]
eleven_to_sixteen =
    [ "onze"
    , "douze"
    , "treize"
    , "quatorze"
    , "quinze"
    , "seize"
    ]
twenty_to_fifty =
    [ "vingt"
    , "trente"
    , "quarante"
    , "cinquante"
    ]
