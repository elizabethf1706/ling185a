{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Homework03 where

import Prelude(Show, Eq, (==), Int, (+), (-), (*), (<), (<=), undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude((++), map, elem)

import FiniteState

-- Another type we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | MB deriving (Show,Eq)

-- A list-like type that will be useful for computing forward values
data SnocList a = ESL | (SnocList a) ::: a deriving Show

-- The string ``CVCCV'' encoded as a snoc list
sl :: SnocList SegmentCV
sl = ((((ESL ::: C) ::: V) ::: C) ::: C) ::: V

-- Checks that all states and symbols mentioned in the transition 
-- table (i.e. delta) come from the provided lists of states and symbols.
fsaSanityCheck :: (Eq st, Eq sy) => Automaton st sy -> Bool
fsaSanityCheck m =
    let (states, syms, i, f, delta) = m in
    let validTransition (q1,x,q2) = elem q1 states && elem x syms && elem q2 states in
    and (map validTransition delta)

-- The `deriving Eq' here means that the obvious/natural 
-- definition of equality will apply when we use (==) 
-- on these numbers.
data Numb = Z | S Numb deriving (Show,Eq)

one, two, three, four :: Numb
one = S Z
two = S one
three = S two
four = S three

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-- A.
fsa_countTrues :: Automaton Numb Bool
fsa_countTrues = ([Z, S Z, S(S Z), S(S(S Z))], -- finite states 
                [True, False],  -- alphabet
                [Z], -- initial state 
                [S(S(S Z))], --end state
                [(Z, False, Z),(Z, True, S Z),(S Z, False, S Z),
                (S Z, True, S (S Z)),
                (S(S Z), False, S(S Z)),
                (S(S Z), True, Z), 
                (S(S Z), True, S(S(S Z))),
                (S(S(S Z)), False, S(S(S Z)))
                ] -- set of transitions
                )
            
-- B. returns a snoc list that is like q but has an additional 
-- occurance of x as its leftmost element.
addToFront :: a-> SnocList a -> SnocList a 
addToFront x lst = case lst of
    ESL -> ESL ::: x
    rest:::l -> (addToFront x rest) ::: l

-- C. returns/ produces the snoc list corresponding to the
-- given norm list
toSnoc :: [a] -> SnocList a 
toSnoc a = case a of 
    [] -> ESL
    head:tail -> addToFront head (toSnoc tail)
    -- when building , the last element in regular list is actually
    --
-- a list like [1,2,3,4]
-- is like 1 :: (2 :: (3 :: (4 :: [])))
--and we want it to be [((((ESL :::1):::2)::3)::4)]

--D. 
forward :: (Eq st, Eq sy) => Automaton st sy -> SnocList sy -> st -> Bool
forward m w q = let (states,syms, i, f, delta) = m in 
            case w of
                ESL -> elem q i
                (rest:::x) -> or (map (\q1 -> elem (q1,x,q) delta && forward m rest q1) states)
-- computes forward values. return true or False
-- depending on if there is a way to get from an initial state
-- of the automaton m to the state q tha tproduces 
-- the symbols of w. 

      --           (x:rest) -> or (map (\q1 -> elem (q,x,q1) delta && backward m rest q1) states)
generates2 :: (Eq st, Eq sy) => Automaton st sy -> [sy] -> Bool
generates2 m w = let (states, syms, i, f, delta) = m in
                or (map (\q1 -> elem q1 f && forward m (toSnoc w) q1) states)
-- code from regular generates:
--             or (map (\q0 -> elem q0 i && backward m w q0) states)
 ------------- FSAs

 -- F.
fsa_twoCs :: Automaton Int SegmentCV 
fsa_twoCs =([1,2,3], -- finite states 
                [C,V],  -- alphabet
                [1], -- initial state 
                [3], --end state
                [(1, V, 1),(2, V, 2), (3, V, 3),
                (1, C, 2),(2, C, 3)
                ] -- set of transitions
            )
-- note for f the recursive c's such as 1,c,1 might not be necessary or correct..

-- G.
fsa_thirdC :: Automaton Int SegmentCV 
fsa_thirdC =    (  [1,2,3,4], -- finite states 
                [C,V],  -- alphabet
                [1], -- initial state 
                [4], --end state
                [(1, C, 2),(1,V,2),(2,C,3),(2,V,3),(3,C,4),(4,C,4),(4,V,4)] -- set of transitions
             )
--H.
fsa_thirdlastC :: Automaton Int SegmentCV 
fsa_thirdlastC = (  [], -- finite states 
                [C,V],  -- alphabet
                [1], -- initial state 
                [4], --end state
                [(1,C,1),(1,V,1),(1,C,2),(2,C,3),(2,V,3),(3,C,4),
                (3,V,4)] -- set of transitions
            )  
-- K. 
fsa_IU :: Automaton Int SegmentPKIU 
fsa_IU = ([1,2], -- finite states 
            [P,K,I,U],  -- alphabet
            [1], -- initial state 
            [1,2], --end state
            [(1,P,1),(1,K,1),(1,I,2),(2,P,2),(2,I,2),(2,K,2),(2,U,2)
            ] -- set of transitions
        )  

-- J.
fsa_adjacentKU:: Automaton Int SegmentPKIU 
fsa_adjacentKU = ([1,2,3], -- finite states 
            [P,K,I,U],  -- alphabet
            [1], -- initial state 
            [1,2,3], --end state
            [
            (1,P,1),(1,I,1),
            (1,K,2),(2,U,3),(3,K,2),(2,P,1),(2,I,1)  
            ] 
        )  
-- K.

fsa_harmony :: Automaton Int SegmentPKIU 
fsa_harmony =([1,2,3,4], -- finite states
               [P,K,I,U,MB],  -- alphabet
               [1], -- initial state
               [1,2,3,4], -- accepting states
               [(1,P,1), (1,K,1), (1,MB,1), (1,I,2), (1,U,3),
                (2,P,2), (2,K,2), (2,MB,4), (2,I,2), 
                (3,P,3), (3,K,3), (3,MB,4), (3,U,3), 
                (4,P,1), (4,K,1), (4,MB,1), (4,I,2), (4,U,3)  
               ])
-- L.
fsa_oddEven :: Automaton Int SegmentCV
fsa_oddEven = ([1,2], -- finite states 
            [C,V],  -- alphabet
            [1], -- initial state 
            [1], --end state
            [(1,V,2),(2,V,1),(1,C,1),(2,C,2) 
            ] 
        )   
-- M.
numbsLessThan :: Numb -> [Numb]
numbsLessThan num = case num of 
            Z -> []
            S n' -> n' : (numbsLessThan n')

-- H. constructs an FSA that accepts all and only those 
-- strings that contain exactly the given number of Cs. 
requireCs :: Numb -> Automaton Numb SegmentCV
requireCs num = 
        let states = numbsLessThan (S num) in
        -- 0 to 2 in numbsLessthan is 0,1
        -- idea is that we are adding 1 to num so that it does 0-2 by addidng S Num
        let syms = [C,V] in
        let starts = [Z] in          
        let ends = [num] in
        let ctransitions = map (\i -> (i, C, S i)) (numbsLessThan num) in
        let vtransitions = map (\i -> (i, V, i)) states in
        (states, syms, starts, ends, ctransitions ++ vtransitions)
 