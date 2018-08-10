module Main

import Average

add: Int -> Int -> Int
add x y = x+y

double: Num ty => ty -> ty
double x = x+x

twice : (a->a) -> a->a
twice f x = f (f x)

quadruple: Num a => a->a
quadruple = twice double

-- casting: Num a => a -> String
-- casting x = cast {to=String} x

longer: String->String->Nat
longer word1 word2
  = let len1 = length word1
        len2 = length word2 in
          if len1>len2 then len1 else len2

pythagoras: Double->Double->Double
pythagoras x y = sqrt (square x+square y)
  where
    square: Double -> Double
    square x = x*x

    
main: IO ()
main = do putStrLn (cast {to=String} (quadruple 5))
          putStrLn (cast {to=String} (twice (\x => x*x) 3) )
          putStrLn (the String (cast (pythagoras 3 4)) )
          putStrLn (cast {to=String} (longer "fdsafadsabcd" "efg") )
          putStrLn ("word average: " ++ cast {to=String} (average "how long area these words?") )




