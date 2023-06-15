module Cipher where

import           Data.Char

cipher n = map ( chr
             . (\x -> if x >= 0 && x < n - 1 then x + 65 else x)
             . (\x -> mod x 91)
             . (\x -> if x /= 32 then x + n else x)
             . ord
             . toUpper
             )

unCipher n = map ( chr
                 . (\x -> if x /= 32 then x - n else x )
                 . ord )
