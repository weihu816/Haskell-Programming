{-# LANGUAGE NoImplicitPrelude, KindSignatures #-}
module Monads where
import Prelude hiding ((>>))
import Data.Char (toUpper)
import Control.Monad (guard)

data Tree a = Leaf a | Branch (Tree a) (Tree a)
   deriving (Eq, Show)

-- | zip two trees together
zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree = undefined

testZip0 :: Bool
testZip0 =
  zipTree (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
          (Branch (Leaf 0  ) (Branch (Leaf 1  ) (Leaf 2  )))
  ==
  (Branch (Leaf ("a",0)) (Branch (Leaf ("b",1)) (Leaf ("c",2))))

testZip zt =
  zt (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
     (Branch (Leaf 0  ) (Branch (Leaf 1  ) (Leaf 2  )))
      ==
      Just (Branch (Leaf ("a",0)) (Branch (Leaf ("b",1)) (Leaf ("c",2))))
  &&
  zt (Branch (Leaf "a") (Leaf "b")) (Leaf 0) == Nothing

zipTree1 :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree1 (Leaf a)     (Leaf b)       = undefined
zipTree1 (Branch l r) (Branch l' r') = undefined
zipTree1 _ _ = Nothing

zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree2 (Leaf a)     (Leaf b) = undefined
zipTree2 (Branch l r) (Branch l' r') = undefined
zipTree2 _ _ = Nothing

zipTree3 :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree3 (Leaf a)     (Leaf b)       = undefined
zipTree3 (Branch l r) (Branch l' r') = undefined
zipTree3 _ _ = Nothing

main :: IO ()
main = do
   putStrLn "This is the Classes lecture. What is your name?"
   inpStr <- getLine
   putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
   return ()

(>>)  :: Monad m => m a -> m b -> m b
m1 >> m2 = undefined

whatDoesThisDo :: IO ()
whatDoesThisDo = foldr (>>) (putStrLn " Batman!")
           (replicate 10 (putStr (show (0.0/0.0))))

zipTree4 :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree4 (Leaf a)     (Leaf b)       = undefined
zipTree4 (Branch l r) (Branch l' r') = undefined
zipTree4 _ _ = Nothing

monad_fmap :: (Monad m) => (a -> b) -> m a -> m b
monad_fmap = undefined

monad_pure :: (Monad m) => a -> m a
monad_pure = undefined

monad_zap  :: (Monad m) => m (a -> b) -> m a -> m b
monad_zap  = undefined

pairs0 :: [Int] -> [Int] -> [(Int,Int)]
pairs0 xs ys = undefined

testPairs ps = ps [1,2,3,4] [5,6,7,8] ==
  [(1,5),(1,6),(1,7),(1,8),(2,5),(2,6),(2,7),(2,8),
   (3,5),(3,6),(3,7),(3,8),(4,5),(4,6),(4,7),(4,8)]

pairs1 :: [Int] -> [Int] -> [(Int,Int)]
pairs1 xs ys = undefined

pairs2 :: [Int] -> [Int] -> [(Int,Int)]
pairs2 xs ys = undefined

testPairs1 = testPairs pairs1
testPairs2 = testPairs pairs2

pairs3 xs ys = [ (x,y) | x <- xs, y <- ys ]

data Color = Red | Green | Blue | Yellow | Orange | Violet deriving (Show, Enum, Eq)

stateColors :: [Color] -> [(Color, Color, Color, Color, Color)]
stateColors colors =
  [(tennessee, mississippi, alabama, georgia, florida) |
   tennessee   <- colors,
   mississippi <- colors,
   alabama     <- colors,
   georgia     <- colors,
   florida     <- colors,
   tennessee   /= mississippi,
   tennessee   /= alabama,
   tennessee   /= georgia,
   mississippi /= alabama,
   alabama     /= georgia,
   florida     /= alabama,
   florida     /= georgia]

colorsNeeded = head (filter (not . null . stateColors) cs) where
    cs = zipWith take [1..] (replicate 6 [Red ..])

map' f xs = undefined

firstLess xs ys = [ (x,y) | x <- xs, y <- ys, x < y]

testFirstLess fl = fl [1,2,3,4] [1,2,3,4] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

firstLess1 xs ys = undefined

firstLess2 xs ys = undefined

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = undefined

quicksort [] = []
quicksort (x:xs) = undefined

pairs4 xs ys = undefined

map'' f xs   = pure f <*> xs

 
genpairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)
 

myval = genpairs getChar getChar

liftM :: Monad m => (a -> b) -> m a -> m b
liftM = undefined

join    :: Monad m => m (m a) -> m a
join mmx = undefined

sequence  :: Monad m => [m a] -> m [a]
sequence  = undefined

