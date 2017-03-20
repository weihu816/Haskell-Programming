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
zipTree1 (Leaf a)     (Leaf b)       = Just (Leaf (a,b))
zipTree1 (Branch l r) (Branch l' r') =
   case zipTree1 l l' of
     Nothing -> Nothing
     Just x  -> case zipTree1 r r' of
                  Nothing -> Nothing
                  Just y  -> Just (Branch x y)
zipTree1 _            _              = Nothing

helper :: Maybe a -> (a -> Maybe b) -> Maybe b
helper Nothing  f = Nothing
helper (Just x) f = (f x)

helper2 :: a -> Maybe a
helper2 = Just

zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree2 (Leaf a)     (Leaf b) = return (Leaf (a,b))
zipTree2 (Branch l r) (Branch l' r') =
    (zipTree2 l l') >>= (\x ->
      (zipTree1 r r') >>= (\y -> return (Branch x y)))

zipTree2 _ _ = Nothing

zipTree3 :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree3 (Leaf a)     (Leaf b)       = return (Leaf (a,b))
zipTree3 (Branch l r) (Branch l' r') = do
    x <- zipTree3 l l'
    y <- zipTree2 r r'
    return (Branch x y)
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
zipTree4 (Leaf a)     (Leaf b)       = pure (Leaf (a,b))
zipTree4 (Branch l r) (Branch l' r') =
     Branch <$> zipTree4 l l' <*> zipTree4 r r'
zipTree4 _ _ = Nothing

monad_fmap :: (Monad m) => (a -> b) -> m a -> m b
monad_fmap = undefined

monad_pure :: (Monad m) => a -> m a
monad_pure = undefined

monad_zap  :: (Monad m) => m (a -> b) -> m a -> m b
monad_zap  = undefined

pairs0 :: [Int] -> [Int] -> [(Int,Int)]
pairs0 xs ys = concatMap (\x -> concatMap (\y -> [(x,y)]) ys) xs

testPairs ps = ps [1,2,3,4] [5,6,7,8] ==
  [(1,5),(1,6),(1,7),(1,8),(2,5),(2,6),(2,7),(2,8),
   (3,5),(3,6),(3,7),(3,8),(4,5),(4,6),(4,7),(4,8)]

pairs1 :: [Int] -> [Int] -> [(Int,Int)]
pairs1 xs ys = xs >>= (\x -> (flip concatMap) ys (\y -> [(x,y)] ))

testPairs1 = testPairs pairs1
testPairs2 = testPairs pairs2

pairs3 xs ys = [ (x,y) | x <- xs, y <- ys ]

pairs2 :: [Int] -> [Int] -> [(Int,Int)]
pairs2 xs ys = do
  x <- xs
  y <- ys
  return (x,y)

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

map' f xs = [ f x | x <- xs ]
map2 f xs = xs >>= (\x -> return (f x))

firstLess xs ys zs = [ (x,y,z) | y <- ys,  x <- xs, x < y, z <- zs]

testFirstLess fl = fl [1,2,3,4] [1,2,3,4] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

firstLess1 xs ys = do
   y <- ys
   x <- xs
   if (x < y) then
     return (x,y)
     else []

firstLess2 xs ys = do
   x <- xs
   y <- ys
   guard (x < y)  --- (>>)
   return (x,y)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [ x | x <- xs, f x ]

quicksort [] = []
quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt where
   lt = [ y | y <- xs, y < x]
   gt = [ y | y <- xs, y > x]

pairs4 xs ys = (,) <$> xs <*> ys

map'' f xs   = pure f <*> xs

 
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)
 

myval = pairs getChar getChar

liftM :: Monad m => (a -> b) -> m a -> m b
liftM = undefined

join    :: Monad m => m (m a) -> m a
join mmx = undefined

sequence  :: Monad m => [m a] -> m [a]
sequence  = undefined

