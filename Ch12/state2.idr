module Main
import Control.Monad.State

increase : Nat -> State Nat ()
increase inc = do current <- get
                  put (current + inc)


data Tree a = Empty | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))                  
                  
treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node left val right)
  = do left_labelled <- treeLabelWith left
      -- get current stream of label
       (this :: rest) <- get
       put rest   -- set new stream of labels to tail of current stream
       right_labelled <- treeLabelWith right
       pure (Node left_labelled (this, val) right_labelled)


flatten : Tree a -> List a
flatten Empty = []                
flatten (Node left val right) = flatten left ++ val :: flatten right

treeLabel : Tree a -> Tree (Integer, a)       
treeLabel tree = evalState (treeLabelWith tree) [1..]
       
       
       

main : IO ()
main = do 
  putStrLn "-----------"
  -- putStrLn (show (runState (increase 5) 89))
  putStrLn (show (flatten (treeLabel testTree)))





