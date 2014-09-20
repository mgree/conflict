module Ordering where

import System.Environment
import System.Exit

import System.Random
import Control.Monad.Random
import Control.Monad (replicateM)

import Text.CSV
 
import Data.Ord
import Data.Function (on)
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map ((!))

diff_select :: RandomGen g => Int -> [a] -> Rand g [a]
diff_select 0 _  = return []
diff_select _ [] = error "too few elements to choose from"
diff_select n xs = do
  r <- getRandomR (0,n-1) 
  let remaining = take r xs ++ drop (r+1) xs
  rest <- diff_select (n-1) remaining
  return $ (xs !! r) : rest

permute :: RandomGen g => [a] -> Rand g [a]
permute xs = diff_select (length xs) xs

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
  
greedySearch :: Paper -> [Paper] -> Conflicts -> [Paper]
greedySearch paper [] _ = [paper]
greedySearch paper papers conflicts =
  let (next,papers') = bestChoice paper papers conflicts in
  paper : greedySearch next papers' conflicts

greedyOrdering :: RandomGen g => [Paper] -> Conflicts -> Rand g Schedule
greedyOrdering papers conflicts = do
  let n = length papers
  r <- getRandomR (0,n-1) 
  let pivot = papers !! r
  let papers' = take r papers ++ drop (r+1) papers
  let ordering = greedySearch pivot papers' conflicts
  return $ map (\paper -> paper : (conflicts `with` paper)) ordering

cost :: [[String]] -> Int
cost [] = 0
cost [(_:cs)] = length cs
cost ((_:cs1):p2@(_:cs2):papers) = length cs1 - (cs1 `overlap` cs2) + cost (p2:papers)

repeatedGreedyOrderings :: RandomGen g => [Paper] -> Conflicts -> Int -> Rand g Schedule
repeatedGreedyOrderings papers conflicts runs = do
  orderings <- replicateM runs (greedyOrdering papers conflicts)
  return $ minimumBy (compare `on` cost) orderings

format :: Show a => [a] -> String
format [] = ""
format (hd : tl) = show hd ++ " " ++ format tl

readCSV :: FilePath -> [String] -> IO CSV
readCSV f h = do
  p <- parseCSVFromFile f
  case p of
    Left e -> do
      putStrLn $ show e
      exitFailure
    Right (header:c) -> 
      if null h
      then return (header:c) -- no headers at all
      else if h == header
      then return $ filter (\row -> length row == length h) c -- correct headers
      else do
             putStrLn $ "Parse error in " ++ f ++ 
                        ": expected header row " ++ format h ++ ", got " ++ format header
             exitFailure

paperHeaders :: [String]
paperHeaders = []

conflictHeaders :: [String]
conflictHeaders = ["paper","title","PC email","conflict type"]
  
pcHeaders :: [String]                  
pcHeaders = ["first","last","email","affiliation"]

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
      schedule <- evalRandIO $ repeatedGreedyOrderings papers conflicts 10
      
      putStrLn $ intercalate "\n" $ map (intercalate ",") schedule
--      putStrLn $ "COST: " ++ show (cost schedule)
      exitSuccess
    _ -> do 
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " [paper list] [conflict CSV] [pc list]"
      exitFailure