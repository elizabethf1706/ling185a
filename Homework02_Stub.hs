module Homework02 where

-- Imports everything from the Recursion module. 
import Recursion

-- Imports just a few things that we have seen from the standard Prelude module. 
-- (If there is no explicit 'import Prelude' line, then the entire Prelude 
-- module is imported. I'm restricting things here to a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), (<=), (>=), Bool(..), Char, Int)

bigger :: Numb -> (Numb -> Numb)
bigger = \n -> (\m -> case n of
                      Z -> m
                      S n' -> case m of {Z -> n; S m' -> S ((bigger n') m')}
               )

numbToInt :: Numb -> Int
numbToInt = \n -> case n of
                  Z -> 0
                  S n' -> 1 + numbToInt n'

intToNumb :: Int -> Numb
intToNumb = \x -> case (x <= 0) of {True -> Z; False -> S (intToNumb (x-1))}

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
mult :: Numb -> (Numb -> Numb)
mult = \n -> (\m ->  case n of
              Z -> 0
              S n' -> case m of
                     z -> 0 
                     S m' -> add m (mult n' m)
              )
