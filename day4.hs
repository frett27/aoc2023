import Data.List.Split
import Data.Maybe
import Data.Set (fromList, toList)

removeDuplicates :: (Eq a, Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

s = "Card   1: 99 46 62 92 60 37 52 56 41 31 | 83 40 31 33 46  3 10 39 82  8 64 35  5 63 60 72 48 87 11 81 95 34 97 37 99"

data Card = Card Int [Int] [Int] deriving (Show, Ord, Eq)

filterEmpty mylist = filter (\l -> (length l) > 0) mylist

convertToNumberList s = map (\n -> read n ::Int) (filterEmpty (splitOn " " s))

parse s = let elements = splitOn ":" s in
            let cardelements = splitOn " " (head elements) in
              let no = read (head (reverse cardelements))::Int in
                 let numberelement = splitOn "|" (head (reverse elements)) in
                     let r = map convertToNumberList numberelement in
                        Card no (head r) (head (tail r)) 

elementInList = any . (==) 


holdedwinningNumber :: [Int] -> [Int] -> [Int]
holdedwinningNumber [] winnersnumber = []
holdedwinningNumber (x:xs) winnersnumber
        | x `elementInList` winnersnumber = [x] ++ holdedwinningNumber xs winnersnumber
        | otherwise = holdedwinningNumber xs winnersnumber

computePoints l = foldl (\acc e -> if acc == 0 then 1 else acc *2 ) 0 l

gains l = map (\e -> let Card no winnings my = e in
                                let winning = holdedwinningNumber my winnings in
                                    computePoints winning) l

count_cards l = map (\e -> let Card no winnings my = e in
                                let winning = holdedwinningNumber my winnings in
                                    length winning) l

v_zero :: Int -> [Int]
v_zero 0 = []
v_zero n = v_zero (n-1) ++ [0]

v_ident 1 = [1]
v_ident n = [0] ++ (v_ident (n-1))

v_add :: [Int] -> [Int] -> [Int]
v_add a b = let m = max (length a) (length b) in
               let la = length a in
                   let lb = length b in 
                      let (aa,ab) = if la < lb  then (a ++ v_zero (lb-la),b) else (a, b ++ v_zero (la-lb)) in
                          zipWith (+) aa ab
v_tovec :: [Int] -> [Int]
v_tovec a = foldl (\acc l -> v_add  acc (v_ident l)) [] a 

fois c vect = map (*c) vect

v_count a f n nend = if n == nend then a else
               let c = a !! n  in
                 let vect = f !! n  in 
                    let newa = v_add a (c `fois` vect) in
                         v_count newa f (n+1) nend


run :: IO()
run = do
        r <- readFile "input_day4.txt"
        
        let parsed = map parse (filterEmpty (splitOn "\n" r))
        let points = gains parsed
        print (sum points)
        print points
        let max_no = length parsed

        let card_gains = zip parsed (count_cards parsed)
        print (card_gains)
        let all_associated_copies = (map (\t -> let Card no winnings my = (fst t) in
                     let copy = snd t in
                        [(no + 1)..( min (no+copy) max_no ) ] ) card_gains)
        print all_associated_copies
        print (head (all_associated_copies))
        let f = map v_tovec all_associated_copies
        print f
        let cards_count = replicate max_no 1 -- one card of each
        print max_no
        print $ sum (v_count cards_count f 0 max_no)
--        print (head f)
--        print (f!! 2)
--        print (head all_associated_copies)
--


