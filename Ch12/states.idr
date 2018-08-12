module Main

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

countFrom : Integer -> InfList Integer
countFrom n = n :: countFrom (n+1)

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbs [] = []
labelWith (lb1 :: lb1s) (val :: vals) = (lb1, val) :: labelWith lb1s vals

-- label : List a -> List (Integer, Char)
-- label = labelWith (iterate (+1) 0)

getPrefix : Nat -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (x :: xs) = x :: getPrefix k xs

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525*seed+1013904223 in
                  (seed' `shiftR` 2) :: randoms seed'

quiz : Stream Int -> (score: Nat) -> IO ()
quiz (num1 :: num2 :: nums) score   -- take num1,num2 from infinite source of Ints
  = do putStrLn ("Score so far: " ++ show score)
       putStr (show num1 ++ " * " ++ show num2 ++ "? ")
       answer <- getLine
       if cast answer == num1*num2
          then do putStrLn "Correct!"
                  quiz nums (score+1)
          else do putStrLn ("wrong, ans is " ++ show (num1*num2))
                  quiz nums score



data Tree a = Empty | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

flatten : Tree a -> List a
flatten Empty = []                
flatten (Node left val right) = flatten left ++ val :: flatten right

-- labelType provide source of labels from 1 to inf
-- Tree(label, node)
treeLabelWidth : Stream labelType -> Tree a -> (Stream labelType, Tree (labelType, a))
treeLabelWidth lbls Empty = (lbls, Empty) 
-- label generated follows tree navigation
treeLabelWidth lbls (Node left val right)
-- label left subtree, give you new subtree and stream; lblThis: label current node; left_labelled: labelled tree
        = let (lblThis :: lblsLeft, left_labelled) = treeLabelWidth lbls left
              -- labels right subtree, lblsLeft: stream returned after labelling left subtree
              (lblsRight, right_labelled) = treeLabelWidth lblsLeft right
                in
              (lblsRight, Node left_labelled (lblThis, val) right_labelled)

treeLabel : Tree a -> Tree (Integer, a)       
treeLabel tree = snd (treeLabelWidth [1..] tree)





main : IO ()
main = do
  putStrLn "------------"
  -- putStrLn (show (labelWith (cycle ["a","b","c"]) [1..5] ))
  -- quiz (iterate (*2) 1) 5
  -- quiz (randoms 12345) 5
  putStrLn (show (flatten (treeLabel testTree)))




