module NumberToLetter.FrenchSpec (spec) where

import NumberToLetter.French (numberToLetter)
import Test.Hspec

spec :: Spec
spec = do
    describe "Convert number to letter in French" $ do
        it "numbers up to sixteen are simple constants" $ do
            numberToLetter 0 `shouldBe` "zéro"
            numberToLetter 1 `shouldBe` "un"
            numberToLetter 2 `shouldBe` "deux"
            numberToLetter 3 `shouldBe` "trois"
            numberToLetter 4 `shouldBe` "quatre"
            numberToLetter 5 `shouldBe` "cinq"
            numberToLetter 6 `shouldBe` "six"
            numberToLetter 7 `shouldBe` "sept"
            numberToLetter 8 `shouldBe` "huit"
            numberToLetter 9 `shouldBe` "neuf"
            numberToLetter 10 `shouldBe` "dix"
            numberToLetter 11 `shouldBe` "onze"
            numberToLetter 12 `shouldBe` "douze"
            numberToLetter 13 `shouldBe` "treize"
            numberToLetter 14 `shouldBe` "quatorze"
            numberToLetter 15 `shouldBe` "quinze"
            numberToLetter 16 `shouldBe` "seize"
        it "numbers have a dash between words" $ do
            numberToLetter 17 `shouldBe` "dix-sept"
            numberToLetter 18 `shouldBe` "dix-huit"
            numberToLetter 19 `shouldBe` "dix-neuf"
            numberToLetter 20 `shouldBe` "vingt"
            numberToLetter 22 `shouldBe` "vingt-deux"
            numberToLetter 23 `shouldBe` "vingt-trois"
            numberToLetter 27 `shouldBe` "vingt-sept"
            numberToLetter 29 `shouldBe` "vingt-neuf"
            numberToLetter 30 `shouldBe` "trente"
            numberToLetter 42 `shouldBe` "quarante-deux"
            numberToLetter 68 `shouldBe` "soixante-huit"
            numberToLetter 81 `shouldBe` "quatre-vingt-un"
        it "seventies are based on sixty" $ do
            numberToLetter 74 `shouldBe` "soixante-quatorze"
            numberToLetter 79 `shouldBe` "soixante-dix-neuf"
        it "nineties are based on eighty" $ do
            numberToLetter 91 `shouldBe` "quatre-vingt-onze"
            numberToLetter 99 `shouldBe` "quatre-vingt-dix-neuf"
        it "one is omitted before one hundred and one thousand" $ do
            numberToLetter 101 `shouldBe` "cent-un"
            numberToLetter 199 `shouldBe` "cent-quatre-vingt-dix-neuf"
            numberToLetter 1000 `shouldBe` "mille"
            numberToLetter 1007 `shouldBe` "mille-sept"
        it "some numbers ending with one have an extra word" $ do
            numberToLetter 21 `shouldBe` "vingt-et-un"
            numberToLetter 31 `shouldBe` "trente-et-un"
            numberToLetter 41 `shouldBe` "quarante-et-un"
            numberToLetter 51 `shouldBe` "cinquante-et-un"
            numberToLetter 61 `shouldBe` "soixante-et-un"
            numberToLetter 71 `shouldBe` "soixante-et-onze"
            numberToLetter 21000 `shouldBe` "vingt-et-un-mille"
            numberToLetter 31000 `shouldBe` "trente-et-un-mille"
            numberToLetter 41000 `shouldBe` "quarante-et-un-mille"
            numberToLetter 51000 `shouldBe` "cinquante-et-un-mille"
            numberToLetter 61000 `shouldBe` "soixante-et-un-mille"
            numberToLetter 71000 `shouldBe` "soixante-et-onze-mille"
        it "several hundreds ends with an s when not followed by another word" $ do
            numberToLetter 100 `shouldBe` "cent"
            numberToLetter 200 `shouldBe` "deux-cents"
            numberToLetter 203 `shouldBe` "deux-cent-trois"
            numberToLetter 300 `shouldBe` "trois-cents"
            numberToLetter 302 `shouldBe` "trois-cent-deux"
            numberToLetter 400 `shouldBe` "quatre-cents"
            numberToLetter 500 `shouldBe` "cinq-cents"
            numberToLetter 600 `shouldBe` "six-cents"
            numberToLetter 700 `shouldBe` "sept-cents"
            numberToLetter 800 `shouldBe` "huit-cents"
            numberToLetter 900 `shouldBe` "neuf-cents"
            numberToLetter 999 `shouldBe` "neuf-cent-quatre-vingt-dix-neuf"
            numberToLetter 200000 `shouldBe` "deux-cent-mille"
        it "eighty ends with an s when not followed by another number" $ do
            numberToLetter 80 `shouldBe` "quatre-vingts"
            numberToLetter 84 `shouldBe` "quatre-vingt-quatre"
            numberToLetter 90 `shouldBe` "quatre-vingt-dix"
            numberToLetter 180 `shouldBe` "cent-quatre-vingts"
            numberToLetter 1984 `shouldBe` "mille-neuf-cent-quatre-vingt-quatre"
            numberToLetter 280000 `shouldBe` "deux-cent-quatre-vingt-mille"
            numberToLetter 380000000 `shouldBe` "trois-cent-quatre-vingts-millions"
            numberToLetter 480000000000 `shouldBe` "quatre-cent-quatre-vingts-milliards"
        it "thousands are not pluralized" $ do
            numberToLetter 2000 `shouldBe` "deux-mille"
            numberToLetter 999999 `shouldBe` "neuf-cent-quatre-vingt-dix-neuf-mille-neuf-cent-quatre-vingt-dix-neuf"
        it "millions and billions are pluralized" $ do
            numberToLetter 1000000 `shouldBe` "un-million"
            numberToLetter 2000000 `shouldBe` "deux-millions"
            numberToLetter 1000000000 `shouldBe` "un-milliard"
            numberToLetter 4000000000 `shouldBe` "quatre-milliards"
