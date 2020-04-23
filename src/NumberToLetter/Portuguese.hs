module NumberToLetter.Portuguese (numberToLetter) where

import NumberToLetter.ConversionRules

numberToLetter = resultOf $ joinWith " e "
    [ specialCase 0 "zero"
    , suffixPowerOfTen 9 numberToLetter " bilhões" "um bilhão"
    , suffixPowerOfTen 6 numberToLetter " milhões" "um milhão"
    , suffixPowerOfTen 3 numberToLetter " mil" "mil"
    , specialCase 100 "cem"
    , literal hundreds 100 999 100
    , literal twenty_to_ninety 20 99 10
    , literal ten_to_nineteen 10 19 1
    , literal one_to_nine 1 9 1
    ] FirstToLast

one_to_nine =
    [ "um"
    , "dois"
    , "três"
    , "quatro"
    , "cinco"
    , "seis"
    , "sete"
    , "oito"
    , "nove"
    ]
ten_to_nineteen =
    [ "dez"
    , "onze"
    , "doze"
    , "treze"
    , "catorze"
    , "quinze"
    , "dezasseis"
    , "dezassete"
    , "dezoito"
    , "dezanove"
    ]
twenty_to_ninety =
    [ "vinte"
    , "trinta"
    , "quarenta"
    , "cinquenta"
    , "sexenta"
    , "setenta"
    , "oitenta"
    , "noventa"
    ]
hundreds =
    [ "cento"
    , "duzentos"
    , "trezentos"
    , "quatrocentos"
    , "quinhentos"
    , "seiscentos"
    , "setecentos"
    , "oitocentos"
    , "novecentos"
    ]
