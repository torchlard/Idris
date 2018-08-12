module Main
import Control.Monad.State

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

countFrom : Integer -> InfList Integer
countFrom n = n :: countFrom (n+1)

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbs [] = []
labelWith (lb1 :: lb1s) (val :: vals) = (lb1, val) :: labelWith lb1s vals

-- label : List a -> List (Integer, Char)
-- label = labelWith (iterate (+1) 0)

getPrefix : Nat -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (x :: xs) = x :: getPrefix k xs

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525*seed+1013904223 in
                  (seed' `shiftR` 2) :: randoms seed'

quiz : Stream Int -> (score: Nat) -> IO ()
quiz (num1 :: num2 :: nums) score   -- take num1,num2 from infinite source of Ints
  = do putStrLn ("Score so far: " ++ show score)
       putStr (show num1 ++ " * " ++ show num2 ++ "? ")
       answer <- getLine
       if cast answer == num1*num2
          then do putStrLn "Correct!"
                  quiz nums (score+1)
          else do putStrLn ("wrong, ans is " ++ show (num1*num2))
                  quiz nums score
       



main : IO ()
main = do 
  -- putStrLn (show (labelWith (cycle ["a","b","c"]) [1..5] ))
  -- quiz (iterate (*2) 1) 5
  quiz (randoms 12345) 5




