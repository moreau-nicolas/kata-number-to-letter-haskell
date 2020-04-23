module NumberToLetter (numberToLetter) where

import qualified NumberToLetter.Dutch as Dutch
import qualified NumberToLetter.English as English
import qualified NumberToLetter.Estonian as Estonian
import qualified NumberToLetter.French as French
import qualified NumberToLetter.German as German
import qualified NumberToLetter.Italian as Italian
import qualified NumberToLetter.Portuguese as Portuguese

numberToLetter :: String -> Int -> String
numberToLetter "Dutch" = Dutch.numberToLetter
numberToLetter "English" = English.numberToLetter
numberToLetter "Estonian" = Estonian.numberToLetter
numberToLetter "French" = French.numberToLetter
numberToLetter "German" = German.numberToLetter
numberToLetter "Italian" = Italian.numberToLetter
numberToLetter "Portuguese" = Portuguese.numberToLetter

numberToLetter _ = \_ -> "Sorry, I do not speak this language. :("
