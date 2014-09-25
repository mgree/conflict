module Util where

import System.Exit
import System.IO

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
      then return $ filter ([""] /=) (header:c) -- no headers at all
      else if h == header
      then return $ filter (\row -> row /= [""] && 
                                    length row == length h) c -- correct headers
      else do
             hPutStrLn stderr $ "Parse error in " ++ f ++ ": expected header row " ++ format h ++ ", got " ++ format header
             hPutStrLn stderr $ "Keeping calm and carrying on..."
             return $ filter ([""] /=) (header:c)

paperHeaders :: [String]
paperHeaders = ["paper"]

conflictHeaders :: [String]
conflictHeaders = ["paper","title","PC email","conflict type"]
  
pcHeaders :: [String]                  
pcHeaders = ["first","last","email","affiliation"]
