module Main

import Effects
import Effect.Random
import Effect.Exception
import Effect.StdIO

-- data MyErr = InvalidInput
-- data Finger = Paper | Scissors | Stone

-- parsing : String -> {[EXCEPTION MyErr]} Eff Int
-- parsing str = if all isDigit (unpack str)
--               then let x = cast str in
--                    if x>=1 && x<=3 then pure x
--                    else raise InvalidInput
--               else raise InvalidInput                  

-- shoot : {[STDIO]} Eff ()
-- shoot = do putStr "Paper(1), Scissors(2), Stone(3): "
--            deal <- 
--            case run {m=Maybe} (parsing $ trim !getStr) of
--               Nothing => 

-- {[RND, STDIO]} Eff ()
rands : Eff Integer [RND]
-- rands = do srand 21324321
--            fromInteger !(rndInt 0 100)
-- rands = rndInt 0 100
rands = do srand 21324321
           rndInt 0 100

main : IO ()
main = do
  putStrLn $ show $ [run {m=Maybe} rands | x<-[1..10]]
  -- run game



