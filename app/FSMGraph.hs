module FSMGraph where

import qualified Data.Graph.Inductive as GI
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete

import Data.Map

-- FSM is a list of states and their transitions, a start state and a list of accept states
data FSM = FSM {
                states :: Map String Id
              , start :: Id
              , transitions :: [Trans]
              , accepts :: [Id]
              }

-- A transition is done with a symbol from a state to a state
type Trans = (Id, Id, Symbol)

-- Identifier for graphviz
type Id = Int

-- A symbol in the language ∑
type Symbol = String 

mkStates :: [String] -> Map String Id
mkStates =
  iterStates 1
  where
    iterStates n [] = empty
    iterStates n (x:xs)= insert x n (iterStates (n+1) xs)


mkTransitions :: [String] -> Map String Id -> Maybe [Trans]
mkTransitions [] _ = Just []
mkTransitions (x:xs) m =
  let w = words x
  in case length w of
    3 -> Just [(m ! head w, m ! (w !! 1), w !! 2)] <> mkTransitions xs m
    _ -> Nothing


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

flipMap :: (Ord a, Ord b) => Map a b -> Map b a
flipMap m = fromList [(val, key) | (key, val) <- toList m]

graphFSM :: FSM -> GI.Gr String String
graphFSM fsm = GI.mkGraph ((0, "") : toList (flipMap $ states fsm)) ((0, start fsm, "") : transitions fsm)

gParams :: [Id] -> GraphvizParams Int String String () String
gParams idaccepts = nonClusteredParams { globalAttributes = ga
                               , fmtNode          = fn idaccepts
                               , fmtEdge          = fe
                               }
  where fn idaccepts (n, l)
            | n == 0 = [shape PointShape]
            | n `elem` idaccepts = [toLabel l, shape DoubleCircle]
            | otherwise = [toLabel l]
        fe (_,_,l) = [toLabel l]


        ga = [ GraphAttrs [ RankDir   FromLeft
                          , BgColor   [toWColor White]
                          , Overlap ScaleOverlaps
                          ]
             , NodeAttrs  [ shape     Circle
                          ]
             ]