module Main

import Effects
import Effect.State
import Effect.StdIO
import Data.Vect

-- readInt: Eff () [STATE (List Int), STDIO]
readInt : Eff Bool [STATE (Vect n Int), STDIO]
              (\ok => if ok then [STATE (Vect (S n) Int), STDIO]
                            else [STATE (Vect n Int), STDIO])
                --  [STATE (Vect (S n) Int), STDIO]
readInt = do let x = trim !getStr
             case all isDigit (unpack x) of
                False => pureM False
                True => do putM (cast x :: !get)
                           pureM True

            --  putM $ cast x :: !get





main : IO ()
main = do
  putStrLn ""
--  run readInt







