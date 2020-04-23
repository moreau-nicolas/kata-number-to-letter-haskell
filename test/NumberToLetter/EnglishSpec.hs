module NumberToLetter.EnglishSpec (spec) where

import NumberToLetter.English (numberToLetter)
import Test.Hspec

spec :: Spec
spec = do
    describe "Convert number to letter in English" $ do
        it "numbers up to twenty are simple constants" $ do
            numberToLetter 0  `shouldBe` "zero"
            numberToLetter 1  `shouldBe` "one"
            numberToLetter 2  `shouldBe` "two"
            numberToLetter 3  `shouldBe` "three"
            numberToLetter 4  `shouldBe` "four"
            numberToLetter 5  `shouldBe` "five"
            numberToLetter 6  `shouldBe` "six"
            numberToLetter 7  `shouldBe` "seven"
            numberToLetter 8  `shouldBe` "eight"
            numberToLetter 9  `shouldBe` "nine"
            numberToLetter 10 `shouldBe`  "ten"
            numberToLetter 11 `shouldBe`  "eleven"
            numberToLetter 12 `shouldBe`  "twelve"
            numberToLetter 13 `shouldBe`  "thirteen"
            numberToLetter 14 `shouldBe`  "fourteen"
            numberToLetter 15 `shouldBe`  "fifteen"
            numberToLetter 16 `shouldBe`  "sixteen"
            numberToLetter 17 `shouldBe`  "seventeen"
            numberToLetter 18 `shouldBe`  "eighteen"
            numberToLetter 19 `shouldBe`  "nineteen"
        it "numbers up to one hundred have a dash between words" $ do
            numberToLetter 20 `shouldBe` "twenty"
            numberToLetter 21 `shouldBe` "twenty-one"
            numberToLetter 22 `shouldBe` "twenty-two"
            numberToLetter 23 `shouldBe` "twenty-three"
            numberToLetter 27 `shouldBe` "twenty-seven"
            numberToLetter 29 `shouldBe` "twenty-nine"
            numberToLetter 30 `shouldBe` "thirty"
            numberToLetter 42 `shouldBe` "forty-two"
            numberToLetter 51 `shouldBe` "fifty-one"
            numberToLetter 68 `shouldBe` "sixty-eight"
            numberToLetter 74 `shouldBe` "seventy-four"
            numberToLetter 84 `shouldBe` "eighty-four"
            numberToLetter 99 `shouldBe` "ninety-nine"
        it "numbers over one hundred have a space between words" $ do
            numberToLetter 100 `shouldBe` "one hundred"
            numberToLetter 101 `shouldBe` "one hundred one"
            numberToLetter 199 `shouldBe` "one hundred ninety-nine"
            numberToLetter 200 `shouldBe` "two hundred"
            numberToLetter 300 `shouldBe` "three hundred"
            numberToLetter 800 `shouldBe` "eight hundred"
            numberToLetter 999 `shouldBe` "nine hundred ninety-nine"
            numberToLetter 1000 `shouldBe` "one thousand"
            numberToLetter 2000 `shouldBe` "two thousand"
            numberToLetter 999999 `shouldBe` "nine hundred ninety-nine thousand nine hundred ninety-nine"
            numberToLetter 1000000 `shouldBe` "one million"
            numberToLetter 2000000 `shouldBe` "two million"
            numberToLetter 1000000000 `shouldBe` "one billion"
            numberToLetter 4000000000 `shouldBe` "four billion"
