module Util where

import System.Exit
import Text.CSV
 

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
