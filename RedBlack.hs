{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module RedBlack where

import Persistent

import qualified Data.Maybe as Maybe
import qualified Data.List  as List
import Control.Monad (liftM)

import Test.QuickCheck hiding (elements)

data Color = R | B deriving (Eq, Show)
data RBT a = E | N Color (RBT a) a (RBT a)
  deriving (Eq, Show)

color :: RBT a -> Color
color (N c _ _ _) = c
color E = B

good1 :: RBT Int
good1 = N B (N B E 1 E) 2 (N B E 3 E)

bad1 :: RBT Int
bad1  = N R (N B E 1 E) 2 (N B E 3 E)

bad2 :: RBT Int
bad2  = N B (N R E 1 E) 2 (N B E 3 E)

bad3  :: RBT Int
bad3  = N B (N R (N R E 1 E) 2 (N R E 3 E)) 4 E

bad4 :: RBT Int
bad4  = N B (N B E 1 E) 3 (N B E 2 E)

trees :: [RBT Int]
trees = [good1, bad1, bad2, bad3, bad4]

prop_Rb1 :: Bool
prop_Rb1 = color E == B

prop_Rb2 :: RBT Int -> Bool
prop_Rb2 t = color t == B

prop_Rb3 :: RBT Int -> Bool
prop_Rb3 (N c a x b) = prop_Rb3 a && prop_Rb3 b && bh a == bh b where
   bh E = 1
   bh (N c a x b) = (bh a)  + (if c == B then 1 else 0)
prop_Rb3 E = True

prop_Rb4 :: RBT Int  -> Bool
--prop_Rb4 (N c a x b) = (if c == R then color a == B && color b == B else True) && prop_Rb4 a && prop_Rb4 b
prop_Rb4 (N R (N R _ _ _) _ _) = False
prop_Rb4 (N R _ _ (N R _ _ _)) = False
prop_Rb4 (N _ a _ b) = prop_Rb4 a && prop_Rb4 b
prop_Rb4 E = True  

prop_BST :: RBT Int -> Bool
prop_BST t = check Nothing Nothing t where
   check :: Maybe Int -> Maybe Int -> RBT Int -> Bool  
   check min (Just max) (N _ a x b) = min < Just x && x < max && check min (Just x) a && check (Just x) (Just max) b
   check min Nothing    (N _ a x b) = min < Just x && check min (Just x) a && check (Just x) Nothing b     
   check min max E           = True     

instance (Ord a, Arbitrary a) => Arbitrary (RBT a)  where
--   arbitrary = (arbitrary :: Gen [a]) >>= (\l -> return $ foldr insert empty l)

   arbitrary = (foldr insert empty) <$> (arbitrary :: Gen [a])

rbt :: Proxy RBT
rbt = Proxy

main :: IO ()
main = do

  quickCheck $ prop_empty  rbt
  quickCheck $ prop_insert rbt
  --quickCheck $ prop_insert2 rbt

  putStrLn "BST property"
  quickCheck prop_BST
  putStrLn "Leaves are black"
  quickCheck prop_Rb1
  putStrLn "Root is black"
  quickCheck prop_Rb2
  putStrLn "Black height the same"
  quickCheck prop_Rb3
  putStrLn "Red nodes have black children"
  quickCheck prop_Rb4

instance Set RBT where

  empty :: RBT a
  empty = E

  member :: Ord a => a -> RBT a -> Bool
  member x E = False
  member x (N _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  elements :: Ord a => RBT a -> [a]
  elements = undefined

  insert :: Ord a => a -> RBT a -> RBT a
  insert x t = blacken (ins x t)

ins :: Ord a => a -> RBT a -> RBT a

ins x E = N R E x E

ins x s@(N c a y b)
  | x < y     = balance (N c (ins x a) y b)
  | x > y     = balance (N c a y (ins x b))
  | otherwise = s

blacken :: RBT a -> RBT a
blacken E = E
blacken (N _ l v r) = N B l v r

balance :: RBT a -> RBT a 
balance (N B (N R (N R a x b) y c) z d) = N R (N B a x b) y (N B c z d)
balance (N B (N R a x (N R b y c)) z d) = N R (N B a x b) y (N B c z d)
balance (N B a x (N R (N R b y c) z d)) = N R (N B a x b) y (N B c z d)
-- balance (N B a x (N R b y (N R c z d))) = N R (N B a x b) y (N B c z d)
balance t = t  

delete :: Ord a => a -> RBT a -> RBT a
delete x t = blacken (del t) where
      del E = E
      del (N _ a y b)
          | x < y     = delLeft  a y b
          | x > y     = delRight a y b
          | otherwise = merge a b

      delLeft a@(N B _ _ _) y b = balLeft (del a) y b
      delLeft a             y b = N R (del a) y b

      balLeft :: RBT a -> a -> RBT a -> RBT a
      balLeft (N R a x b) y c            = N R (N B a x b) y c
      balLeft bl x (N B a y b)           = balance (N B bl x (N R a y b))
      balLeft bl x (N R (N B a y b) z c) = N R (N B bl x a) y (balance (N B b z (sub1 c)))

      sub1 :: RBT a -> RBT a
      sub1 (N B a x b) = N R a x b
      sub1 _ = error "invariance violation"

      delRight a y b@(N B _ _ _) = balRight a y (del b)
      delRight a y b             = N R a y (del b) 

      balRight :: RBT a -> a -> RBT a -> RBT a
      balRight a x (N R b y c)            = N R a x (N B b y c)
      balRight (N B a x b) y bl           = balance (N B (N R a x b) y bl)
      balRight (N R a x (N B b y c)) z bl = N R (balance (N B (sub1 a) x b)) y (N B c z bl)

      merge :: RBT a -> RBT a -> RBT a
      merge E x = x
      merge x E = x
      merge (N R a x b) (N R c y d) =
        case merge b c of
          N R b' z c' -> N R (N R a x b') z (N R c' y d)
          bc -> N R a x (N R bc y d)
      merge (N B a x b) (N B c y d) = 
        case merge b c of
          N R b' z c' -> N R  (N B a x b') z (N B c' y d)
          bc -> balLeft a x (N B bc y d)
      merge a (N R b x c)           = N R (merge a b) x c
      merge (N R a x b) c           = N R a x (merge b c)

prop_delete_spec1 :: RBT Int -> Bool
prop_delete_spec1 t = all (\x -> not (member x (delete x t))) (elements t)

prop_delete_spec2 :: RBT Int -> Bool
prop_delete_spec2 t = all (\(x,y) -> x == y || (member y (delete x t))) allpairs where
  allpairs = [ (x,y) | x <- elements t, y <- elements t ]

prop_delete_spec3 :: RBT Int -> Int -> Property
prop_delete_spec3 t x = not (x `elem` elements t) ==> (delete x t == t)

prop_delete_bst :: RBT Int -> Bool
prop_delete_bst t = all (\x -> prop_BST (delete x t)) (elements t)

prop_delete2 :: RBT Int -> Bool
prop_delete2 t = all (\x -> prop_Rb2 (delete x t)) (elements t)

prop_delete3 :: RBT Int -> Bool
prop_delete3 t = all (\x -> prop_Rb3 (delete x t)) (elements t)

prop_delete4 :: RBT Int -> Bool
prop_delete4 t = all (\x -> prop_Rb4 (delete x t)) (elements t)

check_delete = do
  quickCheckWith (stdArgs {maxSuccess=1000}) prop_delete_spec1
  quickCheckWith (stdArgs {maxSuccess=1000}) prop_delete_spec2
  quickCheckWith (stdArgs {maxSuccess=1000}) prop_delete_spec3
  quickCheckWith (stdArgs {maxSuccess=1000}) prop_delete2
  quickCheckWith (stdArgs {maxSuccess=1000}) prop_delete3
  quickCheckWith (stdArgs {maxSuccess=1000}) prop_delete4
  quickCheckWith (stdArgs {maxSuccess=1000}) prop_delete_bst

