module Main

palindrome : Nat -> String -> String
palindrome n x = let cmp = toLower (reverse x) == toLower x in
        if cmp && (length x > n) then "True" else "False"

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

parse : (Nat,Nat) -> String
parse a = cast {to=String} (fst a) ++ " " ++ cast {to=String} (snd a)

top_ten : Ord a => List a -> List a
top_ten a = take 10 (reverse (sort a))

-- parse_list : Ord a => List a -> String
-- parse_list a = foldl (++) "" (map (\x => cast {from=Num} {to=String} x ++ ",") a)

average : String -> Double
average str = let numWords = wordCount str
                  totalLength = sum (allLengths (words str)) in
                    cast totalLength / cast numWords
                    where
                      wordCount : String -> Nat
                      wordCount str = length (words str)

                      allLengths: List String -> List Nat
                      allLengths strs = map length strs

showAvg : String -> String
showAvg str = "Average word length is " ++ show (average str) ++ "\n"

main: IO()
main = do putStrLn (palindrome 9 "rrrrarrrr")
          putStrLn (parse (counts "hello, idris world!"))
          repl "Enter a String: " showAvg
          -- putStrLn (parse_list [1,3,5])




