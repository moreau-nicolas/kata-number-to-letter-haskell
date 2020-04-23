module NumberToLetter.EstonianSpec (spec) where

import NumberToLetter.Estonian (numberToLetter)
import Test.Hspec

spec :: Spec
spec = do
    describe "Convert number to letter in Estonian" $ do
        it "numbers up to ten are simple constants" $ do
            numberToLetter 0 `shouldBe` "null"
            numberToLetter 1 `shouldBe` "üks"
            numberToLetter 2 `shouldBe` "kaks"
            numberToLetter 3 `shouldBe` "kolm"
            numberToLetter 4 `shouldBe` "neli"
            numberToLetter 5 `shouldBe` "viis"
            numberToLetter 6 `shouldBe` "kuus"
            numberToLetter 7 `shouldBe` "seitse"
            numberToLetter 8 `shouldBe` "kaheksa"
            numberToLetter 9 `shouldBe` "üheksa"
            numberToLetter 10 `shouldBe` "kümme"
        it "numbers between eleven and nineteen have a common suffix" $ do
            numberToLetter 11 `shouldBe` "üksteist"
            numberToLetter 12 `shouldBe` "kaksteist"
            numberToLetter 13 `shouldBe` "kolmteist"
            numberToLetter 14 `shouldBe` "neliteist"
            numberToLetter 15 `shouldBe` "viisteist"
            numberToLetter 16 `shouldBe` "kuusteist"
            numberToLetter 17 `shouldBe` "seitseteist"
            numberToLetter 18 `shouldBe` "kaheksateist"
            numberToLetter 19 `shouldBe` "üheksateist"
        it "multiples of ten under a hundred have a common suffix" $ do
            numberToLetter 20 `shouldBe` "kakskümmend"
            numberToLetter 30 `shouldBe` "kolmkümmend"
            numberToLetter 40 `shouldBe` "nelikümmend"
            numberToLetter 50 `shouldBe` "viiskümmend"
            numberToLetter 60 `shouldBe` "kuuskümmend"
            numberToLetter 70 `shouldBe` "seitsekümmend"
            numberToLetter 80 `shouldBe` "kaheksakümmend"
            numberToLetter 90 `shouldBe` "üheksakümmend"
        it "numbers have a space between words" $ do
            numberToLetter 21 `shouldBe` "kakskümmend üks"
            numberToLetter 22 `shouldBe` "kakskümmend kaks"
            numberToLetter 23 `shouldBe` "kakskümmend kolm"
            numberToLetter 24 `shouldBe` "kakskümmend neli"
            numberToLetter 25 `shouldBe` "kakskümmend viis"
            numberToLetter 26 `shouldBe` "kakskümmend kuus"
            numberToLetter 27 `shouldBe` "kakskümmend seitse"
            numberToLetter 28 `shouldBe` "kakskümmend kaheksa"
            numberToLetter 29 `shouldBe` "kakskümmend üheksa"
            numberToLetter 42 `shouldBe` "nelikümmend kaks"
        it "one is omitted before a hundred and a thousand" $ do
            numberToLetter 100 `shouldBe` "sada"
            numberToLetter 1000 `shouldBe` "tuhat"
        it "hundreds have a common suffix" $ do
            numberToLetter 200 `shouldBe` "kakssada"
            numberToLetter 300 `shouldBe` "kolmsada"
            numberToLetter 400 `shouldBe` "nelisada"
            numberToLetter 500 `shouldBe` "viissada"
            numberToLetter 600 `shouldBe` "kuussada"
            numberToLetter 700 `shouldBe` "seitsesada"
            numberToLetter 800 `shouldBe` "kaheksasada"
            numberToLetter 900 `shouldBe` "üheksasada"
        it "thousands have a space" $ do
            numberToLetter 2000 `shouldBe` "kaks tuhat"
            numberToLetter 3000 `shouldBe` "kolm tuhat"
            numberToLetter 4000 `shouldBe` "neli tuhat"
            numberToLetter 5000 `shouldBe` "viis tuhat"
            numberToLetter 6000 `shouldBe` "kuus tuhat"
            numberToLetter 7000 `shouldBe` "seitse tuhat"
            numberToLetter 8000 `shouldBe` "kaheksa tuhat"
            numberToLetter 9000 `shouldBe` "üheksa tuhat"
        it "millions and billions are pluralized" $ do
            numberToLetter 1000000 `shouldBe` "miljon"
            numberToLetter 2000000 `shouldBe` "kaks miljonit"
            numberToLetter 1000000000 `shouldBe` "miljard"
            numberToLetter 4000000000 `shouldBe` "neli miljardit"
