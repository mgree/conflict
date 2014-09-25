module Slides where

import System.Environment
import System.Exit

import Util

import Data.Function (on)
import Data.List
import qualified Data.Map as Map

asIn names c = Map.findWithDefault "" c names

handoutSlide :: [(String,[String])] -> String
handoutSlide schedule =
  "\\begin{frame}[shrink]\n  \\fontsize{3pt}{3.6}\\selectfont\n" ++
  "  {\\small \\textbf{Schedule}}\n" ++
  "  \\begin{columns}[T]\n" ++
  -- this manual flowing is terrible :(
  let (schedule1,rest) = splitAt ((length schedule `div` 4) - 4) schedule in
  let (schedule2,schedule3) = splitAt ((length rest `div` 2) - 4) rest in
  makeColumn schedule1 ++
  makeColumn schedule2 ++
  makeColumn schedule3 ++
  "  \\end{columns}\n" ++
  "\\end{frame}"
  where makeColumn s = "    \\begin{column}{.31\\linewidth}\n" ++ 
                       concatMap makeEntry s ++
                       "    \\end{column}\n"
        makeEntry (paper,conflicts) = "      \\textbf{Paper \\#" ++ paper ++ ":} " ++ 
                                      intercalate ", " conflicts ++ "\\\\\n"

conflictList :: [String] -> String
conflictList conflicts =
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
        conflictEntry c = "    \\item " ++ c ++ "\n"
  
upNext :: String -> (String,[String]) -> String
upNext phrase (paper,conflicts) =
  "\\begin{block}{" ++ phrase ++ ": Paper \\#" ++ paper ++ "}\n" ++
  conflictList conflicts ++
  "\\end{block}\n"

makeSlides :: [(String,[String])] -> String
makeSlides [] = ""
makeSlides ((paper,conflicts):rest) =
  "\\begin{frame}[shrink]\n" ++
  "  \\frametitle{Paper \\#" ++ paper ++ "}\n\n" ++
  "\\begin{columns}[T]\n\\begin{column}{.6\\linewidth}\n\\large" ++
  conflictList conflicts ++
  "\\end{column}\n" ++
  (if not (null rest)
   then "\\begin{column}{.38\\linewidth}\n" ++
        (case rest of
          (next1:next2:_) -> "\\tiny\n" ++ upNext "Up next" next1 ++ upNext "And then" next2
          [next] -> "\\tiny\n" ++ upNext "Up next (last paper)" next) ++
        "\\end{column}\n"
   else "") ++
  "\\end{columns}\n\\end{frame}\n\n" ++
  makeSlides rest

slideDeck :: String -> String
slideDeck slides = 
  "\\documentclass{beamer}\n\n" ++
  "\\usepackage{pgfpages}\n" ++
  "\\pgfpagesuselayout{resize to}[a4paper, border shrink=5mm, landscape]\n\n" ++
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
      rawSchedule <- readCSV scheduleFile []
      
      pc <- readCSV pcFile pcHeaders
      
      -- pc members
      let shortNames = Map.fromList $ 
                  map (\[first,last,email,_] -> (email,[head first] ++ ". " ++ last)) pc
                  
      let names = Map.fromList $ 
                  map (\[first,last,email,_] -> (email,first ++ " " ++ last)) pc

      let scheduleWith n = map (\(paper:rawConflicts) -> 
                           (paper,sort $ map (asIn n) rawConflicts))
                         rawSchedule

      putStr $ slideDeck $ 
        handoutSlide (scheduleWith shortNames) ++ 
        makeSlides (scheduleWith names)
      
      exitSuccess
    _ -> do 
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " [schedule] [pc list]"
      exitFailure
