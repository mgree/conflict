module Ordering where

import System.Environment
import System.Exit
import System.IO

import System.Random
import Control.Monad.Random
import Control.Monad (replicateM)

import Data.Ord
import Data.Function (on)
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map ((!))

import Util

type Paper = String
type Email = String

type Conflicts = Map.Map Paper [Email]

type Schedule = [[String]]

with :: Conflicts -> Paper -> [Email]
with c p = sort $ Map.findWithDefault [] p c

overlap :: [String] -> [String] -> Int
overlap xs ys = length $ xs `intersect` ys

bestChoice :: Paper -> [Paper] -> Conflicts -> (Paper,[Paper])
bestChoice paper papers conflicts =
  let conflicted = conflicts `with` paper in
  let options = map (\p -> (p,Down $ conflicted `overlap` (conflicts `with` p))) papers in
  let (best:rest) = map fst $ sortBy (compare `on` snd) options in
  (best,rest)
  
greedySearch :: Paper -> [Paper] -> Conflicts -> Schedule
greedySearch paper [] conflicts = [paper : (conflicts `with` paper)]
greedySearch paper papers conflicts =
  let (next,papers') = bestChoice paper papers conflicts in
  (paper : (conflicts `with` paper)) : greedySearch next papers' conflicts

greedyOrdering :: RandomGen g => [Paper] -> Conflicts -> Rand g Schedule
greedyOrdering papers conflicts = do
  let n = length papers
  r <- getRandomR (0,n-1) 
  let pivot = papers !! r
  let papers' = take r papers ++ drop (r+1) papers
  return $ greedySearch pivot papers' conflicts

cost :: [[String]] -> Int
cost [] = 0
cost [(_:cs)] = length cs
cost ((_:cs1):p2@(_:cs2):papers) = length cs1 - (cs1 `overlap` cs2) + cost (p2:papers)

repeatedGreedyOrderings :: RandomGen g => [Paper] -> Conflicts -> Int -> Rand g Schedule
repeatedGreedyOrderings papers conflicts runs = do
  orderings <- replicateM runs (greedyOrdering papers conflicts)
  return $ minimumBy (compare `on` cost) orderings

optimalGreedyOrdering :: [Paper] -> Conflicts -> (Schedule,[Int])
optimalGreedyOrdering papers conflicts = (optimal,map snd costs)
  where optimal = fst $ minimumBy (compare `on` snd) costs
        costs = map (\s -> (s,cost s)) orderings 
        orderings = map (\(pivot,ps) -> greedySearch pivot ps conflicts) pivots
        pivots = [(papers !! i,take i papers ++ drop (i+1) papers) | i <- [0..length papers-1]]

main :: IO ()
main = do 
  args <- getArgs
  case args of
    [paperFile,conflictFile,pcFile] -> do
      -- read in the data
      paperCSV <- readCSV paperFile []
      let papers = concat paperCSV
      
      rawConflicts <- readCSV conflictFile conflictHeaders
      
      pc <- readCSV pcFile pcHeaders
      
      
      -- pc members
      let emails = Set.fromList $ map (\[_,_,email,_] -> email) pc
            
      -- filter out conflicts for non-papers and non-pc members
      let realConflicts = filter (\[paper,_,email,_] -> 
                                   paper `elem` papers &&
                                   email `Set.member` emails) 
                          rawConflicts
      let conflicts = Map.fromListWith (\cs1 cs2 -> sort $ cs1 ++ cs2) $ 
                      map (\[paper,_,email,_] -> (paper,[email])) realConflicts
      
      -- reorder the papers
      -- schedule <- evalRandIO $ repeatedGreedyOrderings papers conflicts 10
      let (schedule,costs) = optimalGreedyOrdering papers conflicts
          
      hPutStr stderr $ "Schedule has " ++ show (length schedule) ++ " papers under discussion.\n"
      -- hPutStr stderr $ "Chose schedule with minimal cost " ++ show (cost schedule) ++ " out of " ++ show costs
      
      putStr $ intercalate "\n" $ map (intercalate ",") schedule
      exitSuccess
    _ -> do 
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " [paper list] [conflict CSV] [pc list]"
      exitFailure