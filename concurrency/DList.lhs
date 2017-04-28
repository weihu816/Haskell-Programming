> {-# OPTIONS -Wall -fno-warn-orphans -fwarn-tabs -fno-warn-type-defaults #-}

Difference Lists
================

In this problem, you will use the idea of "continuations" to implement a
version of lists, called `DList`s.

> module DList (DList, empty, singleton, append, toList, fromDList, micro1, micro2) where


Motivation
----------

Difference Lists support O(1) append operations on lists, making them very
useful for append-heavy uses, such as logging and pretty printing.

If you'd like to see the difference between using (++) with regular lists and
`append` using DLists, in GHCi you can type

    *Main> :set +s

That will cause GHCi to give you timing information for each evaluation that
you do. Then, after you complete this file, you can test out these two logging
micro-benchmarks:

> micro1 :: Char
> micro1 = last (t 10000 "") where
>   t 0 l = l
>   t n l = t (n-1) (l ++ "s")

    *Main> micro1
    's'
    (2.80 secs, 4,300,584,976 bytes)        

> micro2 :: Char
> micro2 = last (fromDList (t 10000 empty) "") where 
>    t 0 l = l
>    t n l = t (n-1) (l `append` singleton 's')

     *Main> micro2
     's'
     (0.02 secs, 10,359,248 bytes)

Implementation
--------------

The key idea is that we will represent a list a function that, when given
another list, returns the contents of the difference list prepended to the
given list. They are related to continuations in that we have "factored out"
what to do at the end of the list. If the end of the list is "", then we are
done. However, it could be any other continuation of the list.

> data DList a = DList { fromDList :: [a] -> [a] }

These are the "constructors" of the data structure; the three functions that
we can use to create difference lists.

> empty :: DList a
> empty = DList $ \t -> t

> singleton :: a -> DList a 
> singleton x = DList $ \t -> x:t

> append :: DList a -> DList a -> DList a
> append (DList x) (DList y) = DList (x . y)

However, once we have constructed a `DList` the *only* way to observe it is to
convert it to a list. This data structure does not support any other form of
pattern matching.

> toList :: DList a -> [a]
> toList (DList x) = reverse $ x []

For example, we use `toList` to compare `DList`s for equality and to show them.

> instance Eq a => Eq (DList a) where
>    x == y = toList x == toList y 

> instance Show a => Show (DList a) where
>    show x = show (toList x)

And that's it.  You're on your own for testing here. You should ensure that
'DList's behave like normal lists.  Add your tests and properties here. 