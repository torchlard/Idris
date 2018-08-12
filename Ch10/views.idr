module Main

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs: List a) -> (x: a) -> ListLast (xs ++ [x])
  
-- describeHelper : (input: List Int) -> (form: ListLast input) -> String
-- describeHelper [] Empty = "empty"
-- describeHelper (xs ++ [x]) (NonEmpty xs x) = "nonempty, initial portion = " ++ show xs

-- covering function of ListLast view
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                        Empty => NonEmpty [] x
                        NonEmpty ys y => NonEmpty (x :: ys) y

-- value of pattern (xs ++ [x]) forced by valid pattern  NonEmpty xs x
describeListEnd : List Int -> String
-- describeListEnd xs = describeHelper xs (listLast xs)                        
describeListEnd input with (listLast input)
  describeListEnd [] | Empty = "empty"
  describeListEnd (xs ++ [x]) | (NonEmpty xs x) = "nonempty, init = " ++ show xs


my_reverse : List a -> List a
my_reverse input with (listLast input)
  my_reverse [] | Empty = []  
  my_reverse (xs ++ [x]) | (NonEmpty xs x) = x :: my_reverse xs 


data SplitList : List a -> Type where
  SplitNil : SplitList []
  SplitOne : SplitList [x]
  SplitPair : (lefts: List a) -> (rights: List a) -> SplitList (lefts ++ rights)

splitList : (input: List a) -> SplitList input
splitList input = splitListHelp input input where
  splitListHelp _ [] = SplitNil
  splitListHelp _ [x] = SplitOne
  splitListHelp (_ :: _ :: counter) (item :: items)
    = case splitListHelp counter items of
           SplitNil => SplitOne
           SplitOne {x} => SplitPair [item] [x]
           SplitPair lefts rights => SplitPair (item :: lefts) rights
  splitListHelp _ items = SplitPair [] items


mergeSort : Ord a => List a -> List a  
mergeSort input with (splitList input)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (lefts ++ rights) | (SplitPair lefts rights) 
    = merge (mergeSort lefts) (mergeSort rights)    -- merge will sort list

  

  


main : IO ()
main = do putStrLn (describeListEnd [1,2,3])
          putStrLn (show (my_reverse [1,3,5,7,9]))
          putStrLn (show (mergeSort [5,1,4,3,2,6,8,7,9]))








