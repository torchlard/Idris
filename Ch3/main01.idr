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

-- concrete comparison for two elements
insert : Ord elem => 
        (x : elem) -> (xsSorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = if x<y then x::y::xs 
                            else y :: insert x xs

-- overall operation for whole insert sort
insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                      insert x xsSorted

my_length : Ord x => Vect n x -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs
                      
-- 
my_swap : Ord elem => elem -> Vect k elem -> Vect (S k) elem
my_swap x [] = [x]
my_swap y xs = my_swap xs :: y

my_reverse : Ord x => Vect n elem -> Vect n elem
my_reverse [] = []
my_reverse (x :: xs) = my_swap x (my_reverse xs)

main : IO ()
main = do putStrLn (show tenInts)
          putStrLn (show (insSort [1,3,2,9,7,6,4,5,8] ) )
          putStrLn (show (my_length [1,3,5] ))
          putStrLn (show (my_reverse [1,3,5]))




