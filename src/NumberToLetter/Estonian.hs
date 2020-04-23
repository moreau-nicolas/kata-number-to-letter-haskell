module NumberToLetter.Estonian (numberToLetter) where

import NumberToLetter.ConversionRules

numberToLetter = resultOf $ joinWith " "
    [ specialCase 0 "null"
    , suffixPowerOfTen 9 numberToLetter " miljardit" "miljard"
    , suffixPowerOfTen 6 numberToLetter " miljonit" "miljon"
    , suffixPowerOfTen 3 numberToLetter " tuhat" "tuhat"
    , suffixPowerOfTen 2 numberToLetter "sada" "sada"
    , literal eleven_to_nineteen 11 19 1
    , suffixPowerOfTen 1 numberToLetter "kümmend" "kümme"
    , literal one_to_nine 1 9 1
    ] FirstToLast

one_to_nine =
    [ "üks"
    , "kaks"
    , "kolm"
    , "neli"
    , "viis"
    , "kuus"
    , "seitse"
    , "kaheksa"
    , "üheksa"
    ]
eleven_to_nineteen = map (++"teist") one_to_nine
