module Main where

import Data.List.Split
import System.IO


findInString str ch = [ y | (x,y) <- zip str [0..], x == ch]
isDigit x = (length (findInString "0123456789" x)) > 0
extractDigit y = [ x | x <- y, isDigit x] 

spelled = ["one","two","three","four","five","six","seven","eight","nine"]
spelledIdx = ['1','2','3','4','5','6','7','8','9']

xahead n [] = ""
xahead 0 (_) = ""
xahead n (x:xs) = [x] ++ (xahead (n-1) xs)

foundInSpelled s ziptwolists =
       let filteredTuple = filter (\w -> (xahead (length (fst w)) s) == (fst w)) ziptwolists in
       case filteredTuple of
         [] -> Nothing
         (x:xs) -> Just (snd x)

-- find first digit or "named digit" in the string, given the dictionary of name with index
findFirst :: String -> [(String, Char)] -> Maybe Char
findFirst [] (_) = Nothing
findFirst all@(x:xs) ziptwolists =
      if isDigit x then
         Just x
      else
         let r = foundInSpelled all ziptwolists  in
            case r of
                Just(e) -> Just e
                Nothing -> findFirst xs ziptwolists

-- handle the error of not finding the digit in the file
flatMaybe :: Maybe Char -> String
flatMaybe x = case x of
    Just (e) -> [e]
    Nothing -> ""

main :: IO()
main = do
         content <- readFile "input_day1.txt" 
         let lines = splitOn "\n" content 
         let cleanLined = filter (\l -> length l > 0) lines
         let firsts = map (\l -> findFirst l (zip spelled spelledIdx)) cleanLined
         let lasts = map (\l -> findFirst l (zip (map reverse spelled) spelledIdx)) (map reverse cleanLined)
         let onlyDigits = map (\l -> (fst l) ++ (snd l)) (zip (map flatMaybe firsts) (map flatMaybe lasts))
         let result = map (\l -> read l ::Integer) onlyDigits
         print result
         print (sum result)
    
