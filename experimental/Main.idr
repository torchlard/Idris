module Main

getMiddle : String -> String
getMiddle (x::xs) =  x :: (getMiddle xs)




main : IO ()    
main = do
  putStrLn ""
  -- putStrLn (show (Tree 5 Empty))
  -- putStrLn (showing ( Empty))
  -- putStrLn (test 2 3)

  