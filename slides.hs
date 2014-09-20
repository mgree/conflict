module Slides where

import System.Environment
import System.Exit

import Util

import Data.Function (on)
import Data.List
import qualified Data.Map as Map

asIn names c = Map.findWithDefault "" c names

makeSlide :: Map.Map String String -> [String] -> String
makeSlide names (paper:rawConflicts) =
  let conflicts = sortBy (compare `on` (asIn names)) rawConflicts in
  "\\begin{frame}" ++ (if length conflicts > 20 then "[shrink]" else "") ++ "\n" ++
  "  \\frametitle{Paper " ++ paper ++ "}\n\n" ++
  (if null conflicts
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
        else items conflicts) ++
  "\\end{frame}"
  where items cs = "  \\begin{itemize}\n" ++ 
                   concatMap conflictEntry cs ++
                   "  \\end{itemize}\n"
        conflictEntry c = case Map.lookup c names of
            Just name -> "    \\item " ++ name ++ "\n"
            Nothing -> ""

slideDeck :: [String] -> String
slideDeck slides = 
  "\\documentclass{beamer}\n\n" ++
  "\\begin{document}\n" ++
  intercalate "\n\n" slides ++ "\n" ++
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
                   
      putStr $ slideDeck $ map (makeSlide names) schedule
      
      exitSuccess
    _ -> do 
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " [schedule] [pc list]"
      exitFailure