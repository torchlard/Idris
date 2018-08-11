module Main
import Data.Vect

data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
        Bicycle : Vehicle Pedal
        Car : (fuel : Nat) -> Vehicle Petrol
        Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

my_zip : Vect n a -> Vect n b -> Vect n (a, b)
my_zip [] ys = []
my_zip (x::xs) (y::ys) = (x, y) :: my_zip xs ys


main : IO ()
main = do putStrLn (show (my_zip [1,2,3] [4,6,8]))







