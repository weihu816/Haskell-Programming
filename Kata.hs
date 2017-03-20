import Data.List as List
import Data.Char as Char
import Test.HUnit

  
{- Questions to discuss and answer with your partner:

1. Which of these three answers is the most readable to you right now? Why?
   
2. Do you think your answer to question #1 will change during the semester? 

3. The general structure of the problem is to read in data from a test file and 
   then calculate with that data. In which of these three versions is that 
   structure the most apparent?

4. There is a design decision even in this small example: Should the program
   specify the lines of interest (and how?), or should the line parser just
   ignore lines that don't make sense?  Which is more robust? 

5. How could these examples be improved? (pick one and improve it.)

-}
       

        
-----------------------------------
-- SAMPLE A --        
        
fusionProcess :: [String] -> String -> Int -> Int -> Int -> Int -> String
fusionProcess lns minName minVal indName ind1 ind2 =
  case lns of
    x : xs -> let col = words x in
              if length col > maximum [indName,ind1,ind2]
                then let val1 = readInt (take 2 (col !! ind1)) in
                     let val2 = readInt (take 2 (col !! ind2)) in
                     let diff = abs (val1 - val2) in
                       if diff < minVal
                         then fusionProcess xs (col !! indName) diff indName
                             ind1 ind2
                         else fusionProcess xs minName minVal indName ind1 ind2
                else fusionProcess xs minName minVal indName ind1 ind2
    _      -> minName

fusionDropLines :: Int -> String -> [String]
fusionDropLines i str =
  drop i (lines str)

weather2_A :: String -> String
weather2_A str =
  let lns = take 30 (fusionDropLines 2 str) in
    fusionProcess lns "" 100 0 1 2

soccer2_A :: String -> String
soccer2_A str =
  let lns = fusionDropLines 1 str in
    fusionProcess lns "" 100 1 6 8

-----------------------------------
-- SAMPLE B --

-- NOTE, this version behaves like the provided tests,
-- not the provided description   

relevantRowsSoccer :: [String] -> [String]
relevantRowsSoccer (_ : tl) = tl
relevantRowsSoccer _ = error "rows: not used properly"

relevantRowsWeather :: [String] -> [String]
relevantRowsWeather (_ : _ : tl) = tl
relevantRowsWeather _  = error "rows: not used properly"
                   
allLines :: String -> [String]
allLines = splitBy (== '\n')
                    
splitAllBy :: (Char -> Bool) -> [String] -> [[String]]
splitAllBy _ [] = []
splitAllBy pred (h : t) = splitBy pred h : splitAllBy pred t

clearEmptyStrings :: [String] -> [String]
clearEmptyStrings [] = []
clearEmptyStrings ([] : tl) = clearEmptyStrings tl
clearEmptyStrings (hd : tl) = hd : clearEmptyStrings tl

maxDiff :: [[String]] -> (String, String)
maxDiff [[h1], [h2], [h3]] = (h1, show (readInt h2 - readInt h3))
maxDiff [h1 : t1, h2 : t2, h3 : t3] =
  let (title, diff) = maxDiff [t1, t2, t3] in
  let thisDiff = (readInt h2) - (readInt h3) in
  if thisDiff > readInt diff then (h1, show thisDiff) else (title, diff)
maxDiff _ = error "wrong file being read"
                  
format :: (Char -> Bool) -> ([String] -> [String]) -> String -> [[String]]
format pred getRows str =
  let lines = allLines str in
  let rows = getRows lines in
  let split = splitAllBy pred rows in
  let cleared = map clearEmptyStrings split in
  transpose cleared

weather2_B :: String -> String
weather2_B str = 
  case format (\x -> x == ' ' || x == '*') relevantRowsWeather str of
    days : maxs : mins : _ ->
      fst $ maxDiff $ List.map List.init [days, maxs, mins]
    _ -> error "weather2: used on wrong file"
  
soccer2_B :: String -> String
soccer2_B str =
  case format (\x -> x == ' ' || x == '-') relevantRowsSoccer str of
    _ : teams : _ : _ : _ : _ : f : a : _ -> fst $ maxDiff [teams, f, a]
    _ -> error "soccer2: used on wrong file"

-------------------------------------------------
-- SAMPLE C --

headIsDigit :: [String] -> Bool
headIsDigit ((x:_):_) = Char.isDigit x
headIsDigit _         = False
   
getMinimumByExtraction :: ([String] -> (String, Int)) -> String -> String
getMinimumByExtraction extract str =
  fst .
  List.minimumBy (\x -> compare (snd x) . snd) .
  map extract .
  filter headIsDigit .
  map (splitBy isSpace) $
  splitBy (== '\n') str

extractTemps :: [String] -> (String, Int)
extractTemps (day:hi:lo:_) =
  let high = filter Char.isDigit hi in
  let low  = filter Char.isDigit lo in
  (day, readInt high - readInt low)
extractTemps _ = ("", maxBound)

extractScore :: [String] -> (String, Int)
extractScore (_:team:_:_:_:_:f:_:a:_) =
  (team, abs $ readInt f - readInt a)
extractScore _ = ("", maxBound)
             
 
weather2_C :: String -> String
weather2_C = getMinimumByExtraction extractTemps

soccer2_C :: String -> String
soccer2_C = getMinimumByExtraction extractScore

-------------------------------------------

readInt :: String -> Int
readInt = read

-- function from earlier HW problem
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p l =
  splitByAux p l []

splitByAux :: (a -> Bool) -> [a] -> [a] -> [[a]]
splitByAux _ [] []  = []
splitByAux _ [] acc = [reverse acc]
splitByAux p (x:xs) acc
    | p x && not(null acc) = reverse acc : splitByAux p xs []
    | p x                  = splitByAux p xs []
    | otherwise            = splitByAux p xs (x:acc)

-------------------------------------------    
        
testWeather :: (String -> String) -> Test
testWeather weather = "weather" ~: do
  str <- readFile "weather.dat"
  weather str @?= "14"

        
testSoccer :: (String -> String) -> Test
testSoccer soccer = "soccer" ~: do
  str <- readFile "football.dat"
  soccer str @?= "Aston_Villa"

main = do
  _ <- runTestTT $ TestList [
       "A" ~: testWeather weather2_A,
       "B" ~: testWeather weather2_B,   
       "C" ~: testWeather weather2_C,
       "A" ~: testSoccer soccer2_A,
       "B" ~: testSoccer soccer2_B,     
       "C" ~: testSoccer soccer2_C ]
  return ()

-------------------------------------------  
