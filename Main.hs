-- Homework 1
-- Name: Wei Hu
-- Date: Jan 2017

{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs #-}

{-# OPTIONS -fdefer-type-errors  #-}

module Main where
import Prelude hiding (takeWhile, all, zip, reverse, concat)
import Test.HUnit
import qualified Data.List as List
import qualified Data.Char as Char

main :: IO ()
main = do
   _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testWeather,
                               testSoccer ]
   return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzap ]


-- A specific boolean evaluation function
abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || (x && z))
 
 
tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True,
                          abc True False False ~?= False,
                          abc False True True ~?= False]


-- A specific integer valuation function
arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) = (b*f - c*e, c*d - a*f, a*e - b*d)

 
tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3),
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]


-- reverse a list
reverse :: [a] -> [a]
reverse l = case l of
  [] -> []
  (x:xs) -> reverse xs ++ [x]

-- alternative way
-- reverse l = reverse_aux l [] 
--   where
--     reverse_aux [] acc = acc
--     reverse_aux (x:xs) acc = reverse_aux xs (x:acc)

treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]

-- zap a list
zap :: [a -> b] -> [a] -> [b]
zap funs args = 
  case (funs, args) of
    ([], _) -> []
    (_, []) -> []
    (f:fs, a:as) -> f a : zap fs as

tzap :: Test
tzap = "zap" ~:
  TestList [ zap [ (+1), \n -> n - 1, (+1) ]
                   ([3, 4, 5] :: [Int]) ~?= [4,3,6],
             zap [ null, not . null ] [ [], "a" ] ~?= [True, True],
             zap [] "a" ~?=  "",
             zap [not] [] ~?= []]

--------------------------------------------------------------------------------

testLists :: Test
testLists = "testLists" ~: TestList
  [tintersperse, tinvert, ttakeWhile, tfind, tall, tmap2, tzip,
   tmapMaybe, ttranspose, tconcat, tcountSub, tsplitBy]

-- The intersperse function takes an element and a list
-- and intersperses that element between the elements of the list.
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"
--
-- intersperse is defined in Data.List, and you can test your solution against
-- that one.

intersperse :: a -> [a] -> [a]
intersperse sep l =
  case l of
    [] -> []
    [x] -> [x]
    (x:xs) -> x : sep : intersperse sep xs

tintersperse :: Test
tintersperse = "intersperse" ~: TestList [ 
  intersperse ',' "abcde" ~?= "a,b,c,d,e",
  intersperse 0 [1, 2, 3] ~?= [1, 0, 2, 0, 3] ]
 

-- invert lst returns a list with each pair reversed.
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")]
--   invert ([] :: [(Int,Char)])  returns []
 
--   note, you need to add a type annotation to test invert with []
--

invert :: [(a, b)] -> [(b, a)]
invert l =
  case l of
    [] -> []
    (a, b):xs -> (b, a) : invert xs 

tinvert :: Test
tinvert = "invert" ~: TestList [
  invert [("a",1),("a",2)] ~?= [(1,"a"),(2,"a")],
  invert ([] :: [(Int,Char)]) ~?= [],
  invert [(1,2)] ~?= [(2,1)] ]
 

-- takeWhile, applied to a predicate p and a list xs,
-- returns the longest prefix (possibly empty) of xs of elements
-- that satisfy p:
-- For example,
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []
--
-- takeWhile is a Prelude function

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: TestList [
  takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1,2],
  takeWhile (< 9) [1,2,3] ~?= [1,2,3],
  takeWhile (< 0) [1,2,3] ~?= [] ]
 

-- find pred lst returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a "Maybe".
-- for example:
--     find odd [0,2,3,4] returns Just 3
--
-- find is defined in Data.List

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs)
  | p x = Just x
  | otherwise = find p xs

tfind :: Test
tfind = "find" ~: TestList [
  find odd [0,2,3,4] ~?= Just 3,
  find (<0) [0,2,3,4] ~?= Nothing ]
 

-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False
--
-- all is a prelude function

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs)
  | not (f x) = False
  | otherwise = all f xs

tall :: Test
tall = "all" ~: TestList [
  all odd [1,2,3] ~?= False,
  all even [2,4] ~?= True,
  all odd [ ] ~?= True ]
 

-- map2 f xs ys returns the list obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the Prelude


map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x:xs) = f x : map f xs

tmap2 :: Test
tmap2 = "map2" ~: TestList [
  map2 (+1) [0,1,2] ~?= [1,2,3],
  map2 (*2) [1,2,3] ~?= [2,4,6] ]


-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:
--    zip [1,2] [True] returns [(1,True)]
--
-- zip is defined in the prelude


zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

tzip :: Test
tzip = "zip" ~: TestList [
  zip [1,2] [True] ~?= [(1,True)],
  zip [1,2] [3,4] ~?= [(1,3), (2,4)] ]


-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is not the same behavior as the library version
-- of transpose.
 
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]
--
-- transpose is defined in Data.List


transpose :: [[a]] -> [[a]]
transpose x
  | transposeAux x = []
  | otherwise = case x of
    ((x:xs) : xss) -> (x : [h | (h:_) <- xss]) :
        transpose (xs : [ t | (_:t) <- xss])
    _ -> []

-- whether or not the list contains an emply list
transposeAux :: [[a]] -> Bool
transposeAux [] = True
transposeAux ([]:_) = True
transposeAux [_] = False
transposeAux (x:xs) = transposeAux xs

ttranspose :: Test
ttranspose = "transpose" ~: TestList [
    transpose [[1,2,3],[4,5,6]] ~?= [[1,4],[2,5],[3,6]],
    transpose [[1,2],[3,4,5]] ~?= [[1,3],[2,4]],
    transpose [[], [1]] ~?= [],
    transpose [[] :: [Int]] ~?= [],
    transpose [[1,2],[3]] ~?= [[1,3]],
    transpose [[1],[2,3]] ~?= [[1,2]],
    transpose [[1],[2,3],[4]] ~?= [[1,2,4]],
    transpose [[1,2],[3],[4,5]] ~?= [[1,3,4]],
    transpose [[2],[],[5]] ~?= [],
    transpose [[2,2],[],[5,5]] ~?= [],
    transpose [[],[2],[]] ~?= [],
    transpose [[],[2,2],[]] ~?= [],
    transpose [[1,1],[2,2,2,2],[3,3]] ~?= [[1,2,3],[1,2,3]]
  ]

-- concat
 
-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
--
-- NOTE: remember you cannot use any functions from the Prelude or Data.List for
-- this problem, even for use as a helper function.
 

concat :: [[a]] -> [a]
concat [] = []
concat ([]:xs) = concat xs
concat ((x:xs) : xss) = x : concat (xs:xss)

tconcat :: Test
tconcat = "concat" ~: TestList [
    concat [[1,2,3],[4,5,6],[7,8,9]] ~?= [1,2,3,4,5,6,7,8,9],
    concat [[1,2,3],[],[7,8]] ~?= [1,2,3,7,8]
  ]


-- mapMaybe
 
-- Map a partial function over all the elements of the list
-- for example:
--    mapMaybe root [0.0, -1.0, 4.0] == [0.0,2.0]

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d
 
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) = case f x of
  Just a -> a : mapMaybe f xs
  Nothing -> mapMaybe f xs

tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~: TestList [
    mapMaybe root [0.0, -1.0, 4.0] ~?= [0.0,2.0],
    mapMaybe root [-1.0, -2.0] ~?= ([] :: [Double]),
    mapMaybe root [] ~?= []
  ]

-- countSub sub str
 
-- Return the number of (potentially overlapping) occurrences of substring sub
-- found in the string str.
-- for example:
--      countSub "aa" "aaa" returns 2

countSub :: String -> String -> Int
countSub [] _ = 0
countSub _ [] = 0
countSub s (x:xs)
  | strSlice (x:xs) (length s) == s = countSub s xs + 1
  | otherwise = countSub s xs

strSlice :: String -> Int -> String
strSlice [] _ = []
strSlice (x : xs) i = if i <= 0 then [] else x : strSlice xs (i - 1)

tcountSub :: Test
tcountSub = "countSub" ~: TestList [
    countSub "aa" "aaa" ~?= 2,
    countSub "" "aaa" ~?= 0,
    countSub "aaa" "aaa" ~?= 1
  ]

-- splitBy pred lst
--
-- Divide the list into sections delimited by the given predicate, which do not
-- appear in the output below.
--    for example,
--      splitBy isSpace "four score and seven years" returns
--            ["four","score","and","seven","years"]
--      splitBy isSpace "" returns []

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace  _  = False

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy pred lst =
  case splitByAux pred lst of
    ([], []) -> []
    (fst, rest) -> fst : splitBy pred rest
  where
    splitByAux _ [] = ([], [])
    splitByAux pred (x:xs)
        | pred x = ([], xs) 
        | otherwise = (x:res1, res2) where (res1, res2) = splitByAux pred xs

tsplitBy :: Test
tsplitBy = "splitBy" ~: TestList [
    splitBy isSpace "four score and seven years" ~?= 
      ["four","score","and","seven","years"],
    splitBy isSpace "four  score  and  seven  years" ~?= 
      ["four", "", "score", "", "and", "", "seven", "", "years"],
    splitBy isSpace "" ~?= []
  ]

--------------------------------------------------------------------------------

-- Part One: Weather Data

weather :: String -> String
weather str =
  fst (
    getMin (
      map (\x -> ((x !! 0), (getInt (x !! 1)) - (getInt (x !! 2)))) (
        map (\x -> (filter (\t ->length t>0) (splitBy isSpace x)))
        (slice 2 (lines str))
      )
    )
  )
    

weatherProgram :: IO ()
weatherProgram = do
  str <- readFile "weather.dat"
  putStrLn (weather str)

readInt :: String -> Int
readInt = read

testWeather :: Test
testWeather = "weather" ~: do str <- readFile "weather.dat"
                              weather str @?= "14"


-- Part Two: Soccer League Table

soccer :: String -> String
soccer str = fst (
    getMin (
      map (\x -> ((x !! 1), abs((getInt (x !! 6)) - (getInt (x !! 8))))) (
        map (\x -> (filter (\t ->length t>0) (splitBy isSpace x)))
        ((mySlice 1 17 strs) ++ (mySlice 19 (length strs - 1) strs))
      )
    )
  )
  where strs = (lines str)
 

soccerProgram :: IO ()
soccerProgram = do
  str <- readFile "football.dat"
  putStrLn (soccer str)

testSoccer :: Test
testSoccer = "soccer" ~: do
  str <- readFile "football.dat"
  soccer str @?= "Aston_Villa"


-- Part Three: DRY Fusion

weather2 :: String -> String
weather2 str = getMinKey (slice 2 (lines str)) 0 1 2
 
soccer2 :: String -> String
soccer2 str = getMinKey ((mySlice 1 17 strs) ++ (mySlice 19 (length strs - 1) strs))
  1 6 8 where strs = (lines str)


-- Common retrieve method for above two functions
getMinKey :: [String] -> Int -> Int -> Int -> String
getMinKey strs a b c = fst (
    getMin (
      map (\x -> ((x !! a), abs((getInt (x !! b)) - (getInt (x !! c)))))
      (map (\x -> (filter (\t ->length t>0) (splitBy isSpace x))) strs)
    )
  )
-- slice an array given from index except the last line
slice :: Int -> [a] -> [a]
slice from x = take ((length x) - 1 - from) (drop from x)
-- parse the integer from string
getInt :: String -> Int
getInt x = read (filter (\t -> (Char.isNumber(t) || t == '.')) x) :: Int
-- custome min temperature range function
getMin :: [(String, Int)] -> (String, Int)
getMin l = case l of
  [] -> ("", -1)
  x:[] -> x
  x:xs -> if ((snd x) < (snd m)) then x else m
    where m = (getMin xs)
-- slice an array given from index to index
mySlice :: Int -> Int -> [a] -> [a]
mySlice from to x = take (to - from + 1) (drop from x)
-- Kata Questions


-- To what extent did the design decisions you made when writing the original
-- programs make it easier or harder to factor out common code?
 
shortAnswer1 :: String
shortAnswer1 = "The design decisions I made when writing the original\
\ programs make it easy to factor out common code because I already write\
\ lots of methods that are common and can be shared across these two\
\ functions and as a result, I only need to change a few lines of code"

-- Was the way you wrote the second program influenced by writing the first?
 
shortAnswer2 :: String
shortAnswer2 = "Yes, largely. I was influenced the way to think about how to\
\ write the second program in the way I write the first program in order not\
\ to reinvent wheels."

-- Is factoring out as much common code as possible always a good thing? Did the
-- readability of the programs suffer because of this requirement? How about the
-- maintainability?
 
shortAnswer3 :: String
shortAnswer3 = "I would say that it depends, but it usually should always be\
\ a good design. Usually, this can increase readability of the program because\
\ now there are multiple methods and modules with its own responsibility. For\
\ maintainability, factor out common code is good for maintainability\
\ because when requirement changes you don't need to change code multiple\
\ times, but it does not necessarily need to factoring out as much as\
\ possible. You need to make each common code as clear as possible. \
\ Too much common code sometimes can make the code looks weird and hurt\
\ readability."



