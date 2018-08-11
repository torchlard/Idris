module Main
import Data.Vect

-- addHelper : Num a => Vect n a -> Vect n a -> Vect n a
-- addHelper [] [] = []
-- addHelper (x::xs) (y::ys) = (x+y) :: addHelper xs ys

addMatrix : Num a => Vect rows (Vect cols a) -> 
                     Vect rows (Vect cols a) -> 
                     Vect rows (Vect cols a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMatrix xs ys


createEmpties : Vect n (Vect 0 a)
createEmpties {n = Z} = [] 
createEmpties {n = (S k)} = [] :: createEmpties

-- transposeHelper : Vect n elem -> Vect n (Vect k elem) -> Vect n (Vect (S k) elem)
-- transposeHelper [] [] = []
-- transposeHelper (x::xs) (y::ys) = (x::y) :: transposeHelper xs ys

-- transposeMat : Num elem => Vect m (Vect n elem) -> Vect n (Vect m elem)
-- transposeMat [] = createEmpties
-- transposeMat (x :: xs) = transposeHelper x (transposeMat xs)

transposeMat : Num elem => Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)




main : IO ()
main = do putStrLn (show (transposeMat [[1,2],[3,4],[5,6]]))
          putStrLn (show (addMatrix [[1,2], [3,4],[10,20]] [[5,6], [7,0], [1,9]]))
          putStrLn (show (1+1))
          putStrLn (show (createEmpties {a=Int} {n=4}))






