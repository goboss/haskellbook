module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar n = map shift
  where shift c
          | isAlpha c = chr $ mod (ord c - minChar c + n) (maxChar c - minChar c + 1) + minChar c
          | otherwise  = c
        maxChar c = ord $ if (isUpper c) then 'Z' else 'z'
        minChar c = ord $ if (isUpper c) then 'A' else 'a'

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

