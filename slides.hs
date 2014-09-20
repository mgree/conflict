module Slides where

import System.Environment
import System.Exit

import Util

import Data.Function (on)
import Data.List
import qualified Data.Map as Map

asIn names c = Map.findWithDefault "" c names

conflictList :: Map.Map String String -> [String] -> String
conflictList names rawConflicts =
  let conflicts = sortBy (compare `on` (asIn names)) rawConflicts in
  if null conflicts
  then "\\textbf{No conflicts}"
  else "  \\textbf{Conflicts}:\n" ++
       if length conflicts > 8
       then let (cs1,cs2) = splitAt (length conflicts `div` 2) conflicts in
         "  \\begin{columns}[T]\n" ++
         "  \\begin{column}[T]{5cm}\n" ++ (items cs1) ++ 
         "  \\end{column}\n\n" ++
         "  \\begin{column}[T]{5cm}\n" ++ (items cs2) ++ 
         "  \\end{column}\n" ++
         "  \\end{columns}\n"
       else items conflicts
  where items cs = "  \\begin{itemize}\n" ++ 
                   concatMap conflictEntry cs ++
                   "  \\end{itemize}\n"
        conflictEntry c = case Map.lookup c names of
            Just name -> "    \\item " ++ name ++ "\n"
            Nothing -> ""
  
upNext :: Map.Map String String -> [String] -> String
upNext names (paper:rawConflicts) =
  "\\begin{block}{Up next: Paper " ++ paper ++ "}\n" ++
  conflictList names rawConflicts ++
  "\\end{block}\n"

makeSlides :: Map.Map String String -> [[String]] -> String
makeSlides _ [] = ""
makeSlides names ((paper:rawConflicts):rest) =
  "\\begin{frame}[shrink]\n" ++
  "  \\frametitle{Paper " ++ paper ++ "}\n\n" ++
  conflictList names rawConflicts ++
  (if not (null rest)
   then "\\begin{columns}[T]\n\\begin{column}{.6\\linewidth}~\\end{column}\n" ++
        "\\begin{column}{.38\\linewidth}\n" ++
        (case rest of
          (next1:next2:_) -> "\\tiny\n" ++ upNext names next1 ++ upNext names next2
          [next] -> "\\tiny\n" ++ upNext names next) ++
        "\\end{column}\n\\end{columns}\n"
   else "") ++
  "\\end{frame}\n\n" ++
  makeSlides names rest

slideDeck :: String -> String
slideDeck slides = 
  "\\documentclass{beamer}\n\n" ++
  "\\begin{document}\n" ++
  slides ++
  "\\end{document}\n"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [scheduleFile,pcFile] -> do
      -- read in the data
      schedule <- readCSV scheduleFile []
      
      pc <- readCSV pcFile pcHeaders
      
      -- pc members
      let names = Map.fromList $ 
                  map (\[first,last,email,_] -> (email,first ++ " " ++ last)) pc
                   
      putStr $ slideDeck $ makeSlides names schedule
      
      exitSuccess
    _ -> do 
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " [schedule] [pc list]"
      exitFailure