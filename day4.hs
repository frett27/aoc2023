import Data.List.Split
import Data.Maybe

import Data.Set (fromList, toList)

removeDuplicates :: (Eq a, Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

s = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

-- cell, number with a parent id (for constructing numbers)
data C = Number Char Int |  Separator | Symbol Char deriving (Show,Ord,Eq)
data PosCell = PosCell C Int Int deriving (Show, Ord, Eq)

-- construct cells from chars
constructCell :: String -> Int -> [C]
constructCell [] parent = []
constructCell (c:xs) parent = 
     let elem = any . (==) in
     let cell =  if elem c ['0'..'9'] then 
               [Number c parent] 
             else 
               if c == '.' then [Separator] else [Symbol c] in 
             cell ++ constructCell xs (parent+1)

applyWithPrevious f p [] = []
applyWithPrevious f p (x:xs) = let newcell = (f p x) in 
                [newcell] ++ applyWithPrevious f (Just newcell) xs

-- reparent the word partition
reparent prev cell = case cell of
         Number c parent -> case prev of
             Just (Number prevc prevparent) -> Number c prevparent
             Just Separator -> Number c parent
             Just (Symbol chs) -> Number c parent
             Nothing -> Number c parent
         otherwise -> cell

-- transform cells to positionned cells         
posCell :: [C] -> Int -> Int -> [PosCell]
posCell [] px py = []
posCell (x:xs) px py = case x of
        Number ch parent -> [PosCell x px py] ++ posCell xs (px+1) py
        Symbol '\n' -> posCell xs 0 (py + 1)
        Symbol a -> [PosCell x px py] ++ posCell xs (px+1) py
        Separator -> [PosCell x px py] ++ posCell xs (px+1) py

around referenceCell testedCell = let PosCell r p1 p2 = referenceCell in
                                     let PosCell t t1 t2 = testedCell in
                                          ((abs (p1 - t1)) <=1) && ((abs (p2 - t2)) <=1)
isSymbol cellPos = let PosCell p x y = cellPos in
   case p of
     Symbol s -> True
     otherwise -> False    

partition cellPos = let PosCell p x y = cellPos in
   case p of
     Number ch part -> Just part
     otherwise -> Nothing

charsOfPartition p [] = []
charsOfPartition p (poscell:xs) = let PosCell thecell x y = poscell in
   case thecell of
     Number ch part -> if part == p then [ch] else []
     otherwise -> []
   ++ (charsOfPartition p xs)
           

run:: IO()
run = do 
        r <- readFile "input_day3.txt"
        print r
        let allposCell = (posCell ( applyWithPrevious reparent Nothing (constructCell r 0)) 0 0) 
        print allposCell
        let appPartitions = removeDuplicates (catMaybes (map partition allposCell))
        print appPartitions
        let allSymbols = filter isSymbol allposCell 
        print allSymbols
--         let allCoveredPartitions = removeDuplicates (foldl (\acc poscell -> acc ++ (catMaybes (map partition (filter (around poscell) allposCell)))) [] allSymbols)
--         print allCoveredPartitions
--         let elem x = any (== x)
--         let unCovered = filter (\e -> not (e `elem` allCoveredPartitions)) appPartitions 
--         print unCovered
--         let allcoveredNumbers = map (\l -> charsOfPartition l allposCell) allCoveredPartitions
--         print allcoveredNumbers
--         let result = map (\x -> read x::Int) allcoveredNumbers
--         print (sum result)
        -- part 2
        let allGearsSymbols = filter (\s -> let PosCell c x y = s in case c of
                                             Symbol '*' -> True
                                             otherwise -> False) allSymbols
        let allAssociatedCoveredPartitions = foldl (\acc poscell -> acc ++ [removeDuplicates (catMaybes (map partition (filter (around poscell) allposCell)))]) [] allGearsSymbols
        print allAssociatedCoveredPartitions
        -- retain all symbols associated to exactly 2 parts
        let exactly2associations = filter (\l -> (length l) == 2 ) allAssociatedCoveredPartitions
        print exactly2associations
        let allcoveredNumbers = map (\t -> map (\l -> (read (charsOfPartition l allposCell)::Int)) t) exactly2associations
        print allcoveredNumbers
        let gearsratios = map (\g -> product g) allcoveredNumbers
        print gearsratios
        print (sum gearsratios)
        
        



