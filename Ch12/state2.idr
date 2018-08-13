module Main
import Control.Monad.State

-- increase : Nat -> State Nat ()
-- increase inc = do current <- get
--                   put (current + inc)


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
       
       
--  Q2_1
update : (stateType -> stateType) -> State stateType ()
update fn = do current <- get
               put (fn current)

increase : Nat -> State Nat ()
increase x = update (+x)

-- Q2_2
countEmpty : Tree a -> State Nat ()
countEmpty Empty = update (+1)
countEmpty (Node left val right) = 
  do countEmpty left
     countEmpty right

crew : List String
crew = ["Lister","Rimmer","Kryten","Cat"]     


-- mutual                       
--   Functor (State stateType) where
--     map func x = do val <- x
--                     pure (func val)

--   Applicative (State stateType) where
--     pure = Pure
--     (<*>) f a = do f' <- f
--                    a' <- a
--                    pure (f' a')

--   Monad (State stateType) where
--     (>>=) = Bind                   
  

addIfPositive : Integer -> State Integer Bool
addIfPositive val = do when (val > 0) $
                            do current <- get
                                put (current + val)
                    -- return whether integer sussessfully added
                       pure (val > 0)   
     
addPositives : List Integer -> State Integer Nat
-- for every integer in vals, value correspond to whether int added to state
addPositives vals = do added <- traverse addIfPositive vals
                -- number of successfully added integers
                       pure (length (filter id added))

  

main : IO ()
main = do 
  putStrLn "-----------"
  -- putStrLn (show (runState (increase 5) 89))
  -- putStrLn (show (flatten (treeLabel testTree)))
  -- putStrLn (show (execState (countEmpty testTree) 0))
  -- putStrLn "display crew?"  
  -- x <- getLine
  -- when (x == "yes") $
  --   do traverse putStrLn crew
  --      pure ()
  -- putStrLn "done"
  -- putStrLn (show (runState (addPositives [-4,3,-8,9,8]) 0 ))




