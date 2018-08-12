module Main

square_root_approx: (number: Double) -> (approx: Double) -> Stream Double
square_root_approx num approx = approx :: helper num approx where
  helper num approx =
    let next = (approx + (num/approx)) / 2.0 in
      next :: helper num next

square_root_bound : (max: Nat) -> (num: Double) -> (bound: Double) -> 
                    (approxs: Stream Double) -> Double
square_root_bound Z number bound (x :: xs) = x
square_root_bound (S k) number bound (x :: xs) =
       if (abs (x * x - number) < bound)
          then x
          else square_root_bound k number bound xs

square_root : (number: Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001 (square_root_approx number number)

main : IO ()
main = do
  -- putStrLn (show (take 3 (square_root_approx 10 10 )))
  -- putStrLn (show (take 3 (square_root_approx 100 25 )))
  putStrLn (show (square_root 6))







