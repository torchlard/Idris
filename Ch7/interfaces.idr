module Main

occurrences : Eq ty => (item: ty) -> (values: List ty) -> Nat
occurrences item [] = 0
occurrences item (value :: values) = case value == item of
                                      False => occurrences item values
                                      True => 1 + occurrences item values

data Matter = Solid | Liquid | Gas
data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False
  (/=) x y = not (x == y)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node left e right) (Node left' e' right')
       = left == left' && e == e' && right == right'
  (==) _ _ = False    

-- Q1
data Shape = Triangle Double Double
            | Rectangle Double Double
            | Circle Double

Eq Shape where 
  (==) (Triangle a b) (Triangle a' b') = a == a' && b == b'
  (==) (Rectangle a b) (Rectangle a' b') = a==a' && b==b'
  (==) (Circle a) (Circle a') = a==a'
  (==) _ _ = False
  (/=) x y = not (x==y)

-- can apply functions across tree nodes
Functor Tree where
  map func Empty = Empty
  map func (Node left e right)  
    = Node (map func left)
           (func e)
           (map func right)

totalLen : List String -> Nat
-- len: accumulated length, str: string of item, xs: list of strings
totalLen xs = foldr (\str, len => length str + len) 0 xs           

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node left e right)
        = let leftfold = foldr func acc left
              rightfold = foldr func leftfold right in
              func e rightfold

test_tree : Tree Int
test_tree = Node (Node (Node Empty 1 Empty) 2 
                (Node Empty 3 Empty)) 4 
                (Node (Node Empty 5 Empty) 6 (Node Empty 7 Empty))





main : IO ()
main = do 
  putStrLn "-----------"
  -- putStrLn (show (occurrences 'b' ['a','b','a','b','b','b','c']))
  -- putStrLn (show (Solid == Solid))
  -- putStrLn (show (Solid == Liquid))
  -- putStrLn (show (Circle 4 == Circle 4))
  -- putStrLn (show (Circle 4 == Circle 5))
  -- putStrLn (show (Circle 4 == Triangle 3 2))
  -- putStrLn (show (totalLen ["one", "two", "three"]))
  putStrLn (show (foldr (+) 0 test_tree))
  putStrLn (show (the (Maybe _) (pure "driven snow")))  
  


