module Main
import System

printLength : IO ()
-- >>= : IO a -> (a -> IO b) -> IO b ; >>= as infix operator
-- getLine: IO a ; \input: a ; putStrLn (...): IO b
printLength = getLine >>= \input => putStrLn ("Lenght: " ++ show (length input))


readNumber : IO (Maybe (Nat))
readNumber = do
  input1 <- getLine
  if all isDigit (unpack input1)
    then pure (Just (cast input1))
    else pure Nothing

-- readNums : IO (Maybe (Nat, Nat))    
-- readNums =
--   do num1 <- readNumber
--      case num1 of 
--        Nothing => pure Nothing
--        Just num1_ok =>
--          do num2 <- readNumber
--             case num2 of
--               Nothing => pure Nothing
--               Just num2_ok => pure (Just (num1_ok, num2_ok))

-- rewrite !!
readNums : IO (Maybe (Nat, Nat))            
readNums = 
  do Just num1_ok <- readNumber | Nothing => pure Nothing
     Just num2_ok <- readNumber | Nothing => pure Nothing
     pure (Just (num1_ok, num2_ok))

printNum : IO ()             
printNum = do res <- readNums
              case res of
                Nothing => putStrLn "nothing"
                Just (a,b) => putStrLn (show a ++ ", " ++ show b)
      
readPair : IO (String, String) 
readPair = do str1 <- getLine
              str2 <- getLine
              pure (str1, str2)

usePair : IO ()              
usePair = do (str1, str2) <- readPair
             putStrLn ("you entered " ++ str1 ++ " and " ++ str2)


countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown secs


main : IO ()
main = do
  -- putStrLn "Enter your nums: "
  -- x <- getLine
  -- putStrLn ("Hello " ++ x ++ "!")
  -- getLine >>= putStrLn
  -- printLength
  -- getLine >>= \x => putStrLn x
  -- usePair
  -- printNum
  countdown 5





