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
  then "  \\textbf{No conflicts}"
  else "  \\textbf{Conflicts}:\n" ++ items conflicts
       {- if length conflicts > 8
       then let (cs1,cs2) = splitAt (length conflicts `div` 2) conflicts in
         "  \\begin{columns}[T]\n" ++
         "  \\begin{column}[T]{5cm}\n" ++ (items cs1) ++ 
         "  \\end{column}\n\n" ++
         "  \\begin{column}[T]{5cm}\n" ++ (items cs2) ++ 
         "  \\end{column}\n" ++
         "  \\end{columns}\n"
       else -}
  where items cs = "  \\begin{itemize}\n" ++ 
                   concatMap conflictEntry cs ++
                   "  \\end{itemize}\n"
        conflictEntry c = case Map.lookup c names of
            Just name -> "    \\item " ++ name ++ "\n"
            Nothing -> ""
  
upNext :: String -> Map.Map String String -> [String] -> String
upNext phrase names (paper:rawConflicts) =
  "\\begin{block}{" ++ phrase ++ ": Paper \\#" ++ paper ++ "}\n" ++
  conflictList names rawConflicts ++
  "\\end{block}\n"

makeSlides :: Map.Map String String -> [[String]] -> String
makeSlides _ [] = ""
makeSlides names ((paper:rawConflicts):rest) =
  "\\begin{frame}[shrink]\n" ++
  "  \\frametitle{Paper \\#" ++ paper ++ "}\n\n" ++
  "\\begin{columns}[T]\n\\begin{column}{.6\\linewidth}\n\\large" ++
  conflictList names rawConflicts ++
  "\\end{column}\n" ++
  (if not (null rest)
   then "\\begin{column}{.38\\linewidth}\n" ++
        (case rest of
          (next1:next2:_) -> "\\tiny\n" ++ upNext "Up next" names next1 ++ upNext "And then" names next2
          [next] -> "\\tiny\n" ++ upNext "Up next (last paper)" names next) ++
        "\\end{column}\n"
   else "") ++
  "\\end{columns}\n\\end{frame}\n\n" ++
  makeSlides names rest

slideDeck :: String -> String
slideDeck slides = 
  "\\documentclass{beamer}\n\n" ++
  "\\mode<presentation>\n" ++
  "{\n" ++
  "\\usetheme{Philly}\n\\setbeamercovered{invisible}\n\\setbeamertemplate{navigation symbols}{}\n" ++
  "}\n\n"++
  "\\begin{document}\n" ++ slides ++ "\\end{document}\n"

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