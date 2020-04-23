module NumberToLetter.English (numberToLetter) where

import NumberToLetter.ConversionRules

numberToLetter = resultOf $ joinWith " "
    [ specialCase 0 "zero"
    , suffixPowerOfTen 9 numberToLetter " billion" "one billion"
    , suffixPowerOfTen 6 numberToLetter " million" "one million"
    , suffixPowerOfTen 3 numberToLetter " thousand" "one thousand"
    , suffixPowerOfTen 2 numberToLetter " hundred" "one hundred"
    , joinWith "-"
        [ literal twenty_to_ninety 20 99 10
        , literal one_to_nine 1 9 1
        ] FirstToLast
    , literal ten_to_nineteen 10 19 1
    ] FirstToLast

one_to_nine =
    [ "one"
    , "two"
    , "three"
    , "four"
    , "five"
    , "six"
    , "seven"
    , "eight"
    , "nine"
    ]
ten_to_nineteen =
    [ "ten"
    , "eleven"
    , "twelve"
    , "thirteen"
    , "fourteen"
    , "fifteen"
    , "sixteen"
    , "seventeen"
    , "eighteen"
    , "nineteen"
    ]
twenty_to_ninety =
    [ "twenty"
    , "thirty"
    , "forty"
    , "fifty"
    , "sixty"
    , "seventy"
    , "eighty"
    , "ninety"
    ]
