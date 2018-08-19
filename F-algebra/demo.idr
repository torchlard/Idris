module Main

-- data myList e = Nil | Cons e (myList e)

data NatF a = ZeroF | SuccF a

fib_n : NatF (Int, Int) -> (Int,Int)
fib_n ZeroF = (1,1)
fib_n (SuccF (m,n)) = (n, m+n)


data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr

-- replace fractally recursive structure by repeatedly applying ExprF to itself
-- ExprF (ExprF (ExprF a))
data ExprF a = Const Int
             | Add a a
             | Mul a a

data Fix f = Fx (f (Fix f))


type Expr = Fix ExprF



main : IO ()
main = putStrLn ""










