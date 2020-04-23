module NumberToLetter.German (numberToLetter) where

import NumberToLetter.ConversionRules

numberToLetter = resultOf $ joinWith ""
    [ specialCase 0 "null"
    , suffixPowerOfTen 9 numberToLetter " Milliarden" "eine Milliarde"
    , suffixPowerOfTen 6 numberToLetter " Millionen" "eine Million"
    , suffixPowerOfTen 3 numberToLetter "tausend" "tausend"
    , suffixPowerOfTen 2 numberToLetter "hundert" "hundert"
    , specialCase 1 "eins"
    , joinWith "und"
        [ literal one_to_nine 1 9 1
        , literal twenty_to_ninety 20 99 10
        ] LastToFirst
    , literal ten_to_nineteen 10 19 1
    ] FirstToLast


one_to_nine =
    [ "ein"
    , "zwei"
    , "drei"
    , "vier"
    , "fünf"
    , "sechs"
    , "sieben"
    , "acht"
    , "neun"
    ]
ten_to_nineteen =
    [ "zehn"
    , "elf"
    , "zwölf"
    , "dreizehn"
    , "vierzehn"
    , "fünfzehn"
    , "sechzehn"
    , "siebzehn"
    , "achtzehn"
    , "neunzehn"
    ]
twenty_to_ninety =
    [ "zwanzig"
    , "dreißig"
    , "vierzig"
    , "fünfzig"
    , "sechzig"
    , "siebzig"
    , "achtzig"
    , "neunzig"
    ]
