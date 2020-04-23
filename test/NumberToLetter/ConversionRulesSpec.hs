module NumberToLetter.ConversionRulesSpec (spec) where

import NumberToLetter.ConversionRules
import Test.Hspec

spec = do
    describe "resultOf" $ do
        it "should apply a conversion rule and return its result" $ do
            let dummy _ = ("some result", 0)
            let ruleUnderTest = resultOf dummy

            ruleUnderTest 42 `shouldBe` "some result"


    describe "specialCase" $ do
        let theAnswer = "Answer to the Ultimate Question of Life, the Universe, and Everything"
        let ruleUnderTest = specialCase 42 theAnswer

        it ("should return an array containing " ++
            "the special case literal and 0 " ++
            "when given the exact number") $ do

            ruleUnderTest 42 `shouldBe` (theAnswer, 0)

        it ("should return an array containing " ++
            "the empty string and the number " ++
            "when given any other number") $ do

            ruleUnderTest 51 `shouldBe` ("", 51)


    describe "joinWith" $ do

        describe "first to last" $ do

            it ("should return an array " ++
                "containing an empty string and the number " ++
                "when there are no rules") $ do
                let inputNumber = 42
                let rules = []
                let ruleUnderTest = joinWith ";" rules FirstToLast

                ruleUnderTest inputNumber `shouldBe` ("", inputNumber)

            it ("should return the rule result " ++
                "when there is only one rule") $ do
                let inputNumber = 999
                let rules = [ \_ -> ("evil result", 666) ]
                let ruleUnderTest = joinWith "*" rules FirstToLast

                ruleUnderTest inputNumber `shouldBe` ("evil result", 666)

            it ("should return an array containing " ++
                "the rule results joined by the separator and the result of the last rule" ++
                "when there are several rules") $ do
                let inputNumber = 1956
                let rules = [ \_ -> ("a", 956)
                            , \_ -> ("b", 56)
                            , \_ -> ("c", 0) ]
                let ruleUnderTest = joinWith ";" rules FirstToLast

                ruleUnderTest inputNumber `shouldBe` ("a;b;c", 0)

            it ("should produce a sequence of no more than one separator " ++
                "when some rules return a result containing an empty string") $ do
                let inputNumber = 1993
                let rules = [ \_ -> ("a", 993)
                            , \_ -> ("", 93)
                            , \_ -> ("", 3)
                            , \_ -> ("d", 0) ]
                let ruleUnderTest = joinWith "." rules FirstToLast

                ruleUnderTest inputNumber `shouldBe` ("a.d", 0)

        describe "last to first" $ do

            it ("should return an array " ++
                "containing an empty string and its parameter " ++
                "when there are no rules") $ do
                let inputNumber = 42
                let rules = []
                let ruleUnderTest = joinWith ";" rules LastToFirst

                ruleUnderTest inputNumber `shouldBe` ("", inputNumber)

            it ("should return the rule result " ++
                "when there is only one rule") $ do
                let inputNumber = 999
                let rules = [ \_ -> ("evil result", 666) ]
                let ruleUnderTest = joinWith "*" rules LastToFirst

                ruleUnderTest inputNumber `shouldBe` ("evil result", 666)

            it ("should return an array containing " ++
                "the rule results joined by the separator and the result of the last rule" ++
                "when there are several rules") $ do
                let inputNumber = 1956
                let rules = [ \_ -> ("a", 0)
                            , \_ -> ("b", 56)
                            , \_ -> ("c", 956) ]
                let ruleUnderTest = joinWith ";" rules LastToFirst

                ruleUnderTest inputNumber `shouldBe` ("a;b;c", 0)

            it ("should produce a sequence of no more than one separator " ++
                "when some rules return a result containing an empty string") $ do
                let inputNumber = 1993
                let rules = [ \_ -> ("a", 0)
                            , \_ -> ("", 3)
                            , \_ -> ("", 93)
                            , \_ -> ("d", 993) ]
                let ruleUnderTest = joinWith "." rules LastToFirst

                ruleUnderTest inputNumber `shouldBe` ("a.d", 0)


    describe "literal" $ do
        describe "simple one-to-one mapping" $ do
            let a_to_f = ["A", "B", "C", "D", "E", "F"]
            let ruleUnderTest = literal a_to_f 10 15 1

            it ("should return an array containing " ++
                "the corresponding literal and 0 " ++
                "when given a number in its range of values") $ do
                ruleUnderTest 10 `shouldBe` ("A", 0)
                ruleUnderTest 11 `shouldBe` ("B", 0)
                ruleUnderTest 12 `shouldBe` ("C", 0)
                ruleUnderTest 13 `shouldBe` ("D", 0)
                ruleUnderTest 14 `shouldBe` ("E", 0)
                ruleUnderTest 15 `shouldBe` ("F", 0)

            it ("should return an array containing " ++
                "the empty string and the number " ++
                "when given a number outside of its range of values") $ do
                ruleUnderTest 8 `shouldBe` ("", 8)
                ruleUnderTest 9 `shouldBe` ("", 9)
                ruleUnderTest 16 `shouldBe` ("", 16)
                ruleUnderTest 17 `shouldBe` ("", 17)

        describe "one-to-one mapping with an interval" $ do
            let ten_to_ninety = ["X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]
            let ruleUnderTest = literal ten_to_ninety 10 99 10

            it ("should return an array containing " ++
                "the corresponding literal and the remainder " ++
                "when given a number in its range of values") $ do
                ruleUnderTest 10 `shouldBe` ("X", 0)
                ruleUnderTest 11 `shouldBe` ("X", 1)
                ruleUnderTest 19 `shouldBe` ("X", 9)
                ruleUnderTest 24 `shouldBe` ("XX", 4)
                ruleUnderTest 37 `shouldBe` ("XXX", 7)
                ruleUnderTest 99 `shouldBe` ("XC", 9)

            it ("should return an array containing " ++
                "the empty string and the number " ++
                "when given a number outside of its range of values") $ do
                ruleUnderTest 8 `shouldBe` ("", 8)
                ruleUnderTest 9 `shouldBe` ("", 9)
                ruleUnderTest 100 `shouldBe` ("", 100)
                ruleUnderTest 101 `shouldBe` ("", 101)

        describe "many-to-one mapping" $ do
            let ruleUnderTest = literal ["soixante"] 60 79 1

            it ("should return an array containing " ++
                "the literal and 0 " ++
                "when given a number in its range of values") $ do
                ruleUnderTest 60 `shouldBe` ("soixante", 0)
                ruleUnderTest 61 `shouldBe` ("soixante", 1)
                ruleUnderTest 69 `shouldBe` ("soixante", 9)
                ruleUnderTest 70 `shouldBe` ("soixante", 10)
                ruleUnderTest 74 `shouldBe` ("soixante", 14)
                ruleUnderTest 79 `shouldBe` ("soixante", 19)

            it ("should return an array containing " ++
                "the empty string and the number " ++
                "when given a number outside of its range of values") $ do
                ruleUnderTest 58 `shouldBe` ("", 58)
                ruleUnderTest 59 `shouldBe` ("", 59)
                ruleUnderTest 80 `shouldBe` ("", 80)
                ruleUnderTest 81 `shouldBe` ("", 81)


    describe "suffixPowerOfTen" $ do
        describe "with an invariant suffix" $ do
            let roman x = [error "undefined", "I", "II", "III", "IV"] !! x
            let ruleUnderTest = suffixPowerOfTen 2 roman "*C" "I*C"

            it ("should return an array containing " ++
                "the converted multiple with the suffix and " ++
                "the remainder in the division of the power of ten" ++
                "when given any multiple of the power of ten") $ do
                ruleUnderTest 104 `shouldBe` ("I*C", 4)
                ruleUnderTest 200 `shouldBe` ("II*C", 0)
                ruleUnderTest 207 `shouldBe` ("II*C", 7)
                ruleUnderTest 300 `shouldBe` ("III*C", 0)
                ruleUnderTest 402 `shouldBe` ("IV*C", 2)

            it ("should return an array containing " ++
                "the empty string and the number " ++
                "when given a number smaller than the power of ten") $ do
                ruleUnderTest 90 `shouldBe` ("", 90)
                ruleUnderTest 97 `shouldBe` ("", 97)

        describe "with a pluralized suffix" $ do
            let french x = [error "undefined", error "undefined", "deux", "trois", "quatre"] !! x
            let ruleUnderTest = suffixPowerOfTen 3 french "-mille" "mille"

            it ("should return an array containing " ++
                "the singular and " ++
                "the remainder in the division of the power of ten " ++
                "when given a multiple of the power of ten equal to 1") $ do
                ruleUnderTest 1001 `shouldBe` ("mille", 1)
                ruleUnderTest 1004 `shouldBe` ("mille", 4)

            it ("should return an array containing " ++
                "the converted multiple with the suffix and " ++
                "the remainder in the division of the power of ten " ++
                "when given a multiple of the power of ten greater than 1") $ do
                ruleUnderTest 2000 `shouldBe` ("deux-mille", 0)
                ruleUnderTest 2007 `shouldBe` ("deux-mille", 7)
                ruleUnderTest 3000 `shouldBe` ("trois-mille", 0)
                ruleUnderTest 4002 `shouldBe` ("quatre-mille", 2)

            it ("should return an array containing " ++
                "the empty string and the number " ++
                "when given a number smaller than the power of ten") $ do
                ruleUnderTest 900 `shouldBe` ("", 900)
                ruleUnderTest 907 `shouldBe` ("", 907)
