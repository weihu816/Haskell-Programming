{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}

module Persistent where

import Control.Monad 
import Test.QuickCheck hiding (elements)
import Data.Maybe as Maybe
import Data.List (sort,nub)

class Set s where
   empty    :: s a
   member   :: Ord a => a -> s a -> Bool
   insert   :: Ord a => a -> s a -> s a
   elements :: Ord a => s a -> [a]

data Proxy (s :: * -> *) = Proxy

list :: Proxy []
list = Proxy

prop_empty :: forall s. (Set s) => Proxy s -> Bool
prop_empty _ = null (elements (empty :: s Int))

prop_insert :: (Set s) => Proxy s -> Int -> s Int -> Bool
prop_insert _ x y = member x (insert x y)

prop_insert_inequal :: (Set s) => Proxy s -> Int -> Int -> s Int -> Property
prop_insert_inequal _ x y s =
    x /= y ==> member y s == member y (insert x s)  

prop_elements :: (Set s) => Proxy s -> s Int -> Bool      
prop_elements _ s = length (nub l) == length l where
                        l = elements s

instance Set [] where
   empty    = []
   member   = elem
   insert   = (:)
   elements = nub

