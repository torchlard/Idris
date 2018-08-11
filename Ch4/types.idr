module Main

-- Enumeration
data Bool = False | True
data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

parse : Direction -> String
parse x = case x of
            North => "North"
            South => "South"
            East => "East"
            West => "West"
    
-- union type
data Shape = Triangle Double Double
            | Rectangle Double Double
            | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5*base*height
area (Rectangle length height) = length*height
area (Circle radius) = pi*radius*radius

-- recursive type
-- data Nat = Z | S Nat

data Picture = Primitive Shape
              | Combine Picture Picture
              | Rotate Double Picture
              | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)              

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic1 pic2) = pictureArea pic1 + pictureArea pic2
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic


testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
                (Combine (Translate 35 5 circle)
                  (Translate 15 25 triangle))

-- generic data type: parameterized over some other type

-- data Biggest = NoTriangle | Size Double
-- biggestTriangle : Picture -> Biggest
-- biggestTriangle (Primitive shape) = case shape of
--                                       Triangle => pictureArea shape
--                                       otherwise => NoTriangle


data DivResult = DivByZero | Result Double
-- data Maybe valType = Nothing | Just valType

-- safeDivide : Double -> Double -> DivResult
-- safeDivide x y = if y == 0 then DivByZero else Result (x/y)
safeDivide : Double -> Double -> Maybe Double
safeDivide x y = if y==0 then Nothing else Just (x/y)

-- data List elem = Nil | (::) elem (List elem)  
-- data Either a b = Left a | Right b




main : IO ()
main = do putStrLn (parse (turnClockwise East))
          putStrLn (show (area (Triangle 4 5)))
          putStrLn (show (pictureArea testPicture))
          putStrLn (show (safeDivide 1 2))








