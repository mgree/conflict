module Ordering where

import System.Environment
import System.Exit

import System.Random
import Control.Monad.Random

import Text.CSV
 
import Data.List

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
      then return c -- correct headers
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
          
      conflicts <- readCSV conflictFile conflictHeaders
      pc <- readCSV pcFile pcHeaders
      putStrLn "parsed successfully"
      
      -- reorder the papers
      ordering <- evalRandIO $ permute papers
      
      putStrLn $ intercalate "\n" $ ordering
      exitSuccess
    _ -> do 
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " [paper list] [conflict CSV] [pc list]"
      exitFailure
  putStrLn "gimme a sec"
