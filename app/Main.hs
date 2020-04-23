module Main where

import NumberToLetter (numberToLetter)
import Data.Function ((&))
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= convert >>= putStrLn


convert :: [String] -> IO String
convert [language, number] = read number & numberToLetter language & return
convert _ = usage


usage :: IO String
usage = putStrLn "Usage: number-to-letter LANGUAGE NUMBER"
        >> (exitWith (ExitFailure 1))
