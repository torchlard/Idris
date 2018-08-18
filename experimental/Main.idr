module Main
data Tree : Type -> Type
  where
    Empty : Ord v => Tree v
    Node : Ord k => k -> Tree k -> Tree k

(Show a) => Show (Tree a) where
  show Empty = "Em"  
  show (Node a b) = show a ++","++ show b


dd : Int -> Int
dd x = case compare x 5 of
  LT => 1
  GT => 2
  EQ => 0

-- test : Int -> Int -> String
-- test a b with (dd b)
--   test 5 _ | 2 = "a"
--   test 5 _ | 1 = "b"
--   test 5 _ | 0 = "c"
--   test _ _ = "k"
    

-- test : Int -> Int -> String
-- test a b with (dd b)
--   test 5 _ | 2 = go 5
--     where go _ = "c"
--   test 5 _ | 1 = go 5
--   test 5 _ | 0 = "b"
--   test _ _ | _ = "k"
    
-- mutual
--   even : Nat -> Bool
--   even Z = True
--   even (S k) = odd k

--   odd : Nat -> Bool
--   odd Z = False
--   odd (S k) = even k

showing : Num a => Tree a -> String
showing a = case a of
  Empty => "E"
  Node c d => "a"


main : IO ()    
main = do
  putStrLn ""
  -- putStrLn (show (Tree 5 Empty))
  -- putStrLn (showing ( Empty))
  -- putStrLn (test 2 3)

  