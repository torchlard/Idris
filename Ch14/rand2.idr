module Main

import Effects
import Effect.Random
import Effect.System
import Control.Monad.Identity
import Data.Vect

data Action = Flower | Upwards | Branch

Show (Action) where
  show Flower = "Flower"
  show Upwards = "Ipwards"
  show Branch = "Branch"


RndNumGen : Type
RndNumGen = EffM Identity Action [RND] (const [RND])

getRnd : RndNumGen -> (Action, RndNumGen)
getRnd prev =
    let (Id action) = run prev in
        (action, next)
    where
        next : RndNumGen
        next = do
            prev
            rndSelect $ [ Flower, Branch, Upwards, Upwards, Upwards ]


main : IO ()
main = do
  putStrLn ""
  -- run getRnd >>= putStrLn . show

-- myRandom : Eff (List Integer) [RND, SYSTEM]
-- myRandom = do
--      srand !time
--      pure [ ! (rndInt 0 100), ! (rndInt 0 100), ! (rndInt 0 100)]

-- main : IO ()
-- main = run myRandom >>= putStrLn . show 



