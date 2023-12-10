import Data.List
import Data.List.Split
import Data.Char (isSpace)

data Color = Green | Blue | Red deriving (Eq, Ord, Show, Read, Bounded, Enum)

data BallNumber = BallNumber Int Color deriving (Show)
data Round = Round [BallNumber] deriving (Show)

data Game = Game Int [Round] deriving (Show)


s = "Game 1: 4 blue; 1 green, 2 red; 4 blue, 1 green, 6 red"

parseBall s = let number_color = splitOn " " s in
             case number_color of
                 (x:["blue"])  -> BallNumber ((read x)::Int) Blue
                 (x:["green"]) -> BallNumber ((read x)::Int) Green
                 (x:["red"]) -> BallNumber ((read x)::Int) Red

trim = dropWhileEnd isSpace . dropWhile isSpace              

parseRound s = let part = splitOn ";" s in
             let games = map (\l -> Round $ map (parseBall . trim) (splitOn "," l)) part in
                games             

parseGame :: String -> Game
parseGame s = let twoparts = splitOn ":" s in
                let (intro:final) = splitOn " " (head twoparts) in
                Game ((read (head final)):: Int) (parseRound (head ( tail twoparts)))


possibleRoundBall :: BallNumber -> Bool
possibleRoundBall b = case b of
       BallNumber n Red | n <=12 -> True
       BallNumber n Green | n <=13 -> True
       BallNumber n Blue | n <=14 -> True
       otherwise -> False


checkGame g = let (Game n rounds) = g in
          map (\l -> let (Round listballnumber) = l in 
                         map possibleRoundBall listballnumber
          ) rounds 

reduceBools :: [Bool] -> Bool
reduceBools b = case b of   
      (x:xs) -> x && (reduceBools xs)
      [] -> True

minColorsForRound :: [BallNumber] -> (Int,Int,Int) -> (Int,Int,Int)
minColorsForRound [] (r,g,b) = (r,g,b)
minColorsForRound (x:xs) (r,g,b) = case x of
       BallNumber n Red -> minColorsForRound xs ((max r n), g, b)
       BallNumber n Green -> minColorsForRound xs (r,(max n g), b)
       BallNumber n Blue -> minColorsForRound xs (r, g, (max n b))

power :: (Int,Int,Int) -> Int
power (r,g,b) = r*g*b

max' (a,b,c) (d,e,f) = ((max a d), (max b e), max c f)

run :: IO()
run = do
         content <- readFile "input_day2.txt" 
         let lines = splitOn "\n" content 
         let cleanLined = filter (\l -> length l > 0) lines
         let games = map parseGame cleanLined
         let possible_round = (map checkGame games)
         print games
         let result = map (\x -> reduceBools (map reduceBools x)) possible_round
         print result
         let retained_games = map (\p -> if (fst p) then (snd p) else 0)  (zip result [1..])
         print retained_games
         print (sum retained_games)
         let mincolorforgames = map (\p -> 
                let Game n vround = p in
                       foldl (\acc elem -> max' acc (
                               let Round ballenumbers = elem in
                                      minColorsForRound ballenumbers (0,0,0))) (0,0,0) vround) games
         print mincolorforgames
         let result_power = foldl (\acc t -> let p = power t in sum ([p] ++ [acc])) 0 mincolorforgames 
         print result_power



