module NumberToLetter.ConversionRules
    ( Direction (FirstToLast, LastToFirst)
    , conditionalElement
    , joinWith
    , literal
    , replace
    , resultOf
    , specialCase
    , suffixPowerOfTen
    ) where

import Control.Monad (sequence)
import Control.Monad.State (state, runState)
import Data.Function (id, (&))
import Data.List (intercalate)
import Data.List.Split (splitOn)


type Rule = Int -> (String, Int)


resultOf :: Rule -> Int -> String
resultOf rule  = \number -> fst $ rule number


suffixPowerOfTen :: Int -> (Int -> String) -> String -> String -> Rule
suffixPowerOfTen power convert quantifier singular =
    \number ->
        let factor = 10 ^ power
            next = number `rem` factor
        in
        case number `quot` factor of
            0 -> ("", number)
            1 -> (singular, next)
            _ -> ((convert $ number `quot` factor) ++ quantifier, next)


literal :: [String] -> Int -> Int -> Int -> Rule
literal values from to step =
    \number ->
        let index = ((number - from) `quot` step) `rem` (length values)
            next = number - from - (index * step)
        in
        if number `elem` [from..to]
        then (values !! index, next)
        else ("", number)


specialCase :: Int -> String -> Rule
specialCase value immediatelyConvertsTo =
    \number ->
        if number == value
        then (immediatelyConvertsTo, 0)
        else ("", number)


type Predicate a = a -> Bool

conditionalElement :: String -> Predicate Int -> Rule
conditionalElement element predicate =
    \number ->
        if predicate number
        then (element, number)
        else ("", number)


data Direction = FirstToLast | LastToFirst

joinWith :: String -> [Rule] -> Direction -> Rule
joinWith separator rules direction =
    \number ->
        let (parts, next) = order rules &
                            map state &
                            sequence &
                            accept number
        in (order parts & filter (/="") & intercalate separator, next)
  where
    accept = flip runState
    order = case direction of
        FirstToLast -> id
        LastToFirst -> reverse


replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old
