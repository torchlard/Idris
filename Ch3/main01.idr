module Main
import Data.Vect

fourInts: Vect 4 Int
fourInts = [0,1,2,3]

sixInts : Vect 6 Int
sixInts = [4,5,6,7,8,9]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

insSort : Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                      insert x xsSorted
                      
insert : (x : elem) -> (xsSorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: ys) = if x<y then x :: y :: xs 
                            else y :: insert x xs
                      
                      


main : IO()
main = do putStrLn (show tenInts)






