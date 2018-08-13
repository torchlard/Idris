module Main

occurrences : Eq ty => (item: ty) -> (values: List ty) -> Nat
occurrences item [] = 0
occurrences item (value :: values) = case value == item of
                                      False => occurrences item values
                                      True => 1 + occurrences item values





main : IO ()
main = do
  putStrLn "------------"
  putStrLn (show (occurrences 'b' ['a','b','a','b','b','b','c']))






