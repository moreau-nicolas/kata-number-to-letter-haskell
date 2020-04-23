module NumberToLetter.GermanSpec (spec) where

import NumberToLetter.German (numberToLetter)
import Test.Hspec

spec :: Spec
spec = do
    describe "Convert number to letter in German" $ do
        it "numbers up to twenty are simple constants" $ do
            numberToLetter 0 `shouldBe` "null"
            numberToLetter 1 `shouldBe` "eins"
            numberToLetter 2 `shouldBe` "zwei"
            numberToLetter 3 `shouldBe` "drei"
            numberToLetter 4 `shouldBe` "vier"
            numberToLetter 5 `shouldBe` "fünf"
            numberToLetter 6 `shouldBe` "sechs"
            numberToLetter 7 `shouldBe` "sieben"
            numberToLetter 8 `shouldBe` "acht"
            numberToLetter 9 `shouldBe` "neun"
            numberToLetter 10 `shouldBe` "zehn"
            numberToLetter 11 `shouldBe` "elf"
            numberToLetter 12 `shouldBe` "zwölf"
            numberToLetter 13 `shouldBe` "dreizehn"
            numberToLetter 14 `shouldBe` "vierzehn"
            numberToLetter 15 `shouldBe` "fünfzehn"
            numberToLetter 16 `shouldBe` "sechzehn"
            numberToLetter 17 `shouldBe` "siebzehn"
            numberToLetter 18 `shouldBe` "achtzehn"
            numberToLetter 19 `shouldBe` "neunzehn"
        it "numbers under a million have no space between words" $ do
            numberToLetter 20 `shouldBe` "zwanzig"
            numberToLetter 21 `shouldBe` "einundzwanzig"
            numberToLetter 22 `shouldBe` "zweiundzwanzig"
            numberToLetter 23 `shouldBe` "dreiundzwanzig"
            numberToLetter 27 `shouldBe` "siebenundzwanzig"
            numberToLetter 29 `shouldBe` "neunundzwanzig"
            numberToLetter 30 `shouldBe` "dreißig"
            numberToLetter 42 `shouldBe` "zweiundvierzig"
            numberToLetter 51 `shouldBe` "einundfünfzig"
            numberToLetter 68 `shouldBe` "achtundsechzig"
            numberToLetter 74 `shouldBe` "vierundsiebzig"
            numberToLetter 84 `shouldBe` "vierundachtzig"
            numberToLetter 99 `shouldBe` "neunundneunzig"
            numberToLetter 100 `shouldBe` "hundert"
            numberToLetter 101 `shouldBe` "hunderteins"
            numberToLetter 199 `shouldBe` "hundertneunundneunzig"
            numberToLetter 200 `shouldBe` "zweihundert"
            numberToLetter 202 `shouldBe` "zweihundertzwei"
            numberToLetter 300 `shouldBe` "dreihundert"
            numberToLetter 800 `shouldBe` "achthundert"
            numberToLetter 999 `shouldBe` "neunhundertneunundneunzig"
            numberToLetter 1000 `shouldBe` "tausend"
            numberToLetter 2000 `shouldBe` "zweitausend"
            numberToLetter 999999 `shouldBe` "neunhundertneunundneunzigtausendneunhundertneunundneunzig"
        it "numbers over a million have a space" $ do
            numberToLetter 1000000 `shouldBe` "eine Million"
            numberToLetter 2000000 `shouldBe` "zwei Millionen"
            numberToLetter 1000000000 `shouldBe` "eine Milliarde"
            numberToLetter 4000000000 `shouldBe` "vier Milliarden"
