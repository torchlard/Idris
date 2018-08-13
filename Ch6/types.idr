module Main
import Data.Vect

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt: Bool) -> StringOrInt isInt
getStringOrInt False = "ninety four"
getStringOrInt True = 94

valToString : (isInt: Bool) -> StringOrInt isInt -> String
-- valToString : (isInt: Bool) -> (case isInt of
--                                   False => String
--                                   True => Int) -> String
valToString False y = trim y
valToString True y = cast y


-- adder 0 : Int -> Int
-- adder 1 : Int -> Int -> Int
-- adder 2 : Int -> Int -> Int -> Int

AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)


data Format = Number Format
            | Str Format
            | Chars Format
            | Floats Format 
            | Lit String Format
            | End

-- produce sequence of types for input according to formats detected in string
PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str: String) -> PrintfType fmt
PrintfType (Chars fmt) = (char: Char) -> PrintfType fmt
PrintfType (Floats fmt) = (float: Double) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String            

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Chars fmt) acc = \char => printfFmt fmt (acc ++ show char)
printfFmt (Floats fmt) acc = \float => printfFmt fmt (acc ++ show float)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

-- detect formats 
toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chars (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Floats (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)    -- other % chars
toFormat (c :: chars) = case toFormat chars of    -- concat characters
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

-- Q1
Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

testMatrix : Matrix 2 3
testMatrix = [[0,0,0], [0,0,0]]

-- Q3
TupleVect : Nat -> Type -> Type
TupleVect Z _ = ()
TupleVect (S n) ty = (ty, TupleVect n ty)

test : TupleVect 4 Nat
test = (1,2,3,4,())


main : IO ()
main = do 
  putStrLn "----------"
  -- putStrLn (valToString True 34)
  -- putStrLn (valToString False " fdsa ")
  -- putStrLn (show (adder 2 2 3 4))
  putStrLn (printf "%s %c yes! %d %f" "AMD" 'y' 12 15.5)


  
  
  

