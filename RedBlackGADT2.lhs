Red-Black Trees with GADTs
==========================

This version of RedBlack trees demonstrates the use of GADTs to
statically verify all four RedBlack tree invariants.

* In this version, the type system enforces that the root is black
  by using a "singleton" type.

* In this version, the type system enforces that red nodes have
  black children
  
> {-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns #-}

> {-# LANGUAGE InstanceSigs, GADTs, DataKinds, KindSignatures, 
>     MultiParamTypeClasses, FlexibleInstances, TypeFamilies,
>     InstanceSigs, ScopedTypeVariables #-}
>      
> module RedBlackGADT2 where

> import qualified Data.Maybe as Maybe
> import qualified Data.List  as List
> import Control.Monad (liftM)
> import Test.QuickCheck hiding (elements)


> data Color = Red | Black
    
> data SColor c where
>    R :: SColor Red
>    B :: SColor Black

We need a generalized form of equality. We don't want to force the two types to be the same.
     
> (%==) :: SColor c1 -> SColor c2 -> Bool
> R %== R = True
> B %== B = True
> _ %== _ = False

> instance Show (SColor c) where
>   show R = "R"
>   show B = "B"    

> data RBT a where
>    Root :: CT Black a -> RBT a 
    
> data CT c a where
>    E :: CT Black a
>    N :: Valid c c1 c2 => SColor c -> (CT c1 a) -> a -> (CT c2 a) -> CT c a

> class Valid (c1 :: Color) (c2 :: Color) (c3 :: Color)
> instance Valid Red Black Black
> instance Valid Black c1 c2  
     
    
> instance Show a => Show (CT c a) where
>   show E = "E"
>   show (N c a x b) = "(N " ++ show c ++ " " ++ show a ++ " " ++ show x ++ " " ++ show b ++ ")"      

> instance Show a => Show (RBT a) where
>   show (Root t) = "(Root " ++ show t ++ ")"  
    
> color :: CT c a -> SColor c
> color (N c _ _ _) = c
> color E = B

Furthermore, Red Black trees must satisfy the following 
invariants.

  1. The empty nodes at the leaves are black

  2. The root is always black

  3. From each node, every path to a leaf 
     has the same number of black nodes

  4. Red nodes have black children
  
Sample Trees
------------

> good1 :: RBT Int
> good1 = Root (N B (N B E 1 E) 2 (N B E 3 E))

Root is red

> -- bad1 :: RBT Int
> -- bad1  = Root (N R (N B E 1 E) 2 (N B E 3 E))

Inequal black height

> bad2 :: RBT Int
> bad2  = Root (N B (N R E 1 E) 2 (N B E 3 E))

Red/Red combo

> -- bad3  :: RBT Int
> -- bad3  = Root (N B (N R (N R E 1 E) 2 (N R E 3 E)) 4 E)

Not a BST

> bad4 :: RBT Int
> bad4  = Root (N B (N B E 1 E) 3 (N B E 2 E))

All sample trees

> trees :: [RBT Int]
> trees = [good1, bad2, bad4]


Checking the RBT invariants
---------------------------

We can write quickcheck properties for each of the invariants.

1. The empty nodes at the leaves are black. 

> prop_Rb1 :: Bool
> prop_Rb1 = color E %== B

2. The root of the tree is Black.

> prop_Rb2 :: RBT Int -> Bool
> prop_Rb2 (Root t) = aux t where
>   aux :: CT c Int -> Bool  
>   aux t = color t %== B

3.  For all nodes in the tree, all downward paths from the
node to a leaf contain the same number of Black nodes. 

> prop_Rb3 :: RBT Int -> Bool
> prop_Rb3 (Root t) = aux t where
>   aux :: CT c Int -> Bool  
>   aux (N c a x b) = aux a && aux b && bh a == bh b where
>    bh :: CT c Int -> Int    
>    bh E = 1
>    bh (N c a x b) = (bh a) + (if c %== B then 1 else 0)
>   aux E = True

4. All children of red nodes are black.

> prop_Rb4 :: RBT Int  -> Bool
> prop_Rb4 (Root t) = aux t where
>  aux :: CT c Int -> Bool  
>  aux (N R (N R _ _ _) _ _) = False
>  aux (N R _ _ (N R _ _ _)) = False
>  aux (N _ a _ b) = aux a && aux b
>  aux E = True  

And satisfies the binary search tree condition.

> prop_BST :: RBT Int -> Bool
> prop_BST t = l == List.sort (List.nub l) where
>     l = elements t 


To use quickcheck, we need an arbitrary instance. We'll use one 
based on `insert` and `empty`. 

> instance (Ord a, Arbitrary a) => Arbitrary (RBT a)  where
> --   arbitrary = (arbitrary :: Gen [a]) >>= (\l -> return $ foldr insert empty l)

>    arbitrary = (foldr insert empty) <$> (arbitrary :: Gen [a])

> prop_empty :: Bool
> prop_empty = null (elements (empty :: RBT Int))

> prop_insert :: Int -> RBT Int -> Bool
> prop_insert x y = member x (insert x y) 
  
    
> main :: IO ()
> main = do

Make sure the RBT is a set  
     
>   quickCheck $ prop_empty  
>   quickCheck $ prop_insert 

Implementation specific properties.

>   putStrLn "BST property"
>   quickCheck prop_BST
>   putStrLn "Leaves are black"
>   quickCheck prop_Rb1
>   putStrLn "Root is black"
>   quickCheck prop_Rb2
>   putStrLn "Black height the same"
>   quickCheck prop_Rb3
>   putStrLn "Red nodes have black children"
>   quickCheck prop_Rb4


Implementation
--------------

> empty :: RBT a
> empty = Root E

> member :: Ord a => a -> RBT a -> Bool
> member x (Root t) = aux x t where
>     aux :: Ord a => a -> CT c a -> Bool
>     aux _ E = False
>     aux x (N _ a y b)
>       | x < y     = aux x a
>       | x > y     = aux x b
>       | otherwise = True

> elements :: Ord a => RBT a -> [a]
> elements (Root t) = aux t [] where
>      aux :: Ord a => CT c a -> [a] -> [a]
>      aux E acc = acc
>      aux (N _ a x b) acc = aux a (x : aux b acc)


> insert :: Ord a => a -> RBT a -> RBT a
> insert x (Root t) = blacken (ins x t)

We'll define it with the help of an auxiliary function. 

> data HT a where
>   HN :: SColor c1 -> CT c2 a -> a -> CT c3 a -> HT a
      
> ins :: Ord a => a -> CT c a -> HT a
> ins x E = HN R E x E
> ins x s@(N c a y b)
>   | x < y     = balanceL c (ins x a) y b
>   | x > y     = balanceR c a y (ins x b)
>   | otherwise = (HN c a y b)

Note that this definition breaks the RBT invariants in two ways --- it could
create a tree with a red root, or create a red node with a red child.

Blackening
----------

Note that `ins` creates a tree with a red root when we insert into an empty
tree.  Our first fix to insert is to blacken the top node of the tree to make
sure that invariant (2) is always satisfied.

> blacken :: HT a -> RBT a
> -- blacken E = Root E
> blacken (HN _ l v r) = Root (N B l v r)


Balancing
---------
        
We make one tweak when balancing compared to the last class. We know which
*could* be unbalanced (the one that had the insert) so we only look at that
side to rebalance.

> 
> balanceL :: SColor c1 -> HT a -> a -> CT c2 a -> HT a   
> balanceL B (HN R (N R a x b) y c) z d = HN R (N B a x b) y (N B c z d)
> balanceL B (HN R a x (N R b y c)) z d = HN R (N B a x b) y (N B c z d)
  
> balanceL c (HN B a x b) z d           = HN c (N B a x b) z d
> balanceL c (HN R a@E x b@E) z d       = HN c (N R a x b) z d
> balanceL c (HN R a@(N B _ _ _) x b@(N B _ _ _)) z d
>                                       = HN c (N R a x b) z d  
> balanceL c (HN R a x b) z d           = error ("no case for " ++ show (color a) ++ " " ++ show (color b))
>  
> balanceR :: SColor c1 -> CT c2 a -> a -> HT a -> HT a  
> balanceR B a x (HN R (N R b y c) z d) = HN R (N B a x b) y (N B c z d)
> balanceR B a x (HN R b y (N R c z d)) = HN R (N B a x b) y (N B c z d)
  
> balanceR c a x (HN B b y d)           = HN c a x (N B b y d)
> balanceR c a x (HN R b@E y d@E)       = HN c a x (N R b y d)
> balanceR c a x (HN R b@(N B _ _ _) y d@(N B _ _ _ ))
>                                       = HN c a x (N R b y d)  
> balanceR c a x (HN R b y d)           = error ("no case for " ++ show (color b) ++ " " ++ show (color d))




