module Main
import Data.Vect

appShow : Show a => ({b : _} -> Show b => b -> String) -> a -> String
appShow s x = s x

AppendType : Type
AppendType = {a, n, m : _} -> Vect n a -> Vect m a -> Vect (n + m) a

append : AppendType
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

tupleId : ({a : _} -> a -> a) -> (a, b) -> (a, b)
tupleId f (a, b) = (f a, f b)

Proxy  : Type -> Type -> Type -> Type -> (Type -> Type) -> Type -> Type

Producer' : Type -> (Type -> Type) -> Type -> Type
Producer' a m t = {x', x : _} -> Proxy x' x () a m t

yield : Monad m => a -> Producer' a m ()

applys : ([a] -> Int) -> [a] -> Int
applys f x = f x





main : IO ()
main = putStrLn (show (applys length "hello world"))
