module Main

import Effects
import Effects.Random
import Effects.Exception
import Effect.StdIO

data MyErr = InvalidInput
data Finger = Paper | Scissors | Stone

parsing : String -> {[EXCEPTION MyErr]} Eff Int
parsing str = if all isDigit (unpack str)
              then let x = cast str in
                   if 



main : IO ()
main = run game



