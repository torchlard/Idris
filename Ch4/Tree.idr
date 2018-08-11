module Main

-- binary search tree
-- data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
data BSTree : Type -> Type where
      Empty : Ord elem => BSTree elem
      Node : Ord elem => (left : BSTree elem) -> (val: elem)
                          -> (right : BSTree elem) -> BSTree elem

-- data Ordering = LT | EQ | GT
-- compare : Ord a => a -> a -> Ordering

insert : Ord elem => elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) 
  = case compare x val of
      LT => Node (insert x left) val right
      EQ => orig
      GT => Node left val (insert x right)


listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x::xs) = insert x (listToTree xs)

-- Q2
treeToList : Ord elem => BSTree elem -> List elem
treeToList Empty = []
treeToList (Node a b c) = (treeToList a) ++ [b] ++ (treeToList c)

-- Q3
data Expr = Val Int
            | Add Expr Expr
            | Sub Expr Expr
            | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val a) = a
evaluate (Add a b) = (evaluate a) + (evaluate b)
evaluate (Sub a b) = (evaluate a) - (evaluate b)
evaluate (Mult a b) = (evaluate a) * (evaluate b)
            



main : IO ()
main = do putStrLn (show (treeToList (listToTree [4,1,8,7,2,3,9,5,6])))
          putStrLn (show (evaluate (Mult (Val 10) (Add (Val 6) (Val 3))) ))










