module Main

import Effects
import Effect.State
import Effect.StdIO
import Effect.Random

hello : Eff () [STDIO]
hello = putStrLn "hello world"

data BTree a = Leaf
             | Node (BTree a) a (BTree a)

Show a => Show (BTree a) where
    show Leaf = "[]"
    show (Node l x r) = "[" ++ show l ++ " "
                            ++ show x ++ " "
                            ++ show r ++ "]"

testTree : BTree String
testTree = Node (Node Leaf "Jim" Leaf)                            
            "Fred"
            (Node (Node Leaf "Alice" Leaf)
              "Sheila"
              (Node Leaf "Bob" Leaf))

treeTagAux : BTree a -> Eff (BTree (Int, a)) [STATE Int]
treeTagAux Leaf = pure Leaf
treeTagAux (Node l x r) = do l' <- treeTagAux l
                             i <- get
                             put (i+1)
                             r' <- treeTagAux r
                             pure (Node l' (i,x) r')
              
treeTag : Int -> BTree a -> BTree (Int,a)
treeTag i x = runPure (do put i; treeTagAux x)

stateLength : Eff Nat [STATE String]
-- stateLength = do x <- get
--                  pure (length x)
stateLength = pure (length !get)


-- let y = 42 in f !(g !(print y) !x)

-- navStr : Int -> List Int
-- navStr a = [x*2 | x<-[a .. a+10]]
-- navStr : Int -> Eff (List Int) [STATE Int]
-- navStr a = pure ([x*2 | x<-[a .. a+10]])





main : IO ()
main = do --run hello
  -- printLn (treeTag 1 testTree)
  putStrLn ""



