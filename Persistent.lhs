Persistent Data Structures
==========================

> {-# LANGUAGE KindSignatures, ScopedTypeVariables #-}

> module Persistent where

> import Control.Monad 
> import Test.QuickCheck hiding (elements)
> import Data.Maybe as Maybe
> import Data.List (sort,nub)

A Persistent Set Interface
==========================

A persistent data structure is one where all of the operations are pure
functions. 

For example, let's look at the interface of a simple *persistent* set.  We can
tell that the implementation is persistent just by looking at the *types* of
the operations.

> class Set s where
>    empty    :: s a
>    member   :: Ord a => a -> s a -> Bool
>    insert   :: Ord a => a -> s a -> s a
>    elements :: Ord a => s a -> [a]

When we define an abstract data structure like `Set` above, using a type
class, we should also specify properties that *all* implementations should
satisfy.

Class answers included

   1. no duplicates
   2. isEqual should ignore order
   3. empty set is a subset of every set
   4. after inserting an item, that item should be a member
   5. if something was an element, and is inserted, size does not change
   6. Nothing is in an empty set.
   7. Operations should be idempotent
   8. empty should be false after insert has happened
   9. each element in elements should be a member of set
   10. insert shouldn't reduce the size of the set
   11. No members in empty set.
   12. x <> y -> member y s == member y (insert x s)
   13. A U B = B U A


For each of these properties, we will use a `Proxy` argument to tell
QuickCheck exactly which implementation it should be testing. We could
use a type annotation instead (except for `prop_empty`), but the
`Proxy`-argument trick is a bit easier to use.

> data Proxy (s :: * -> *) = Proxy

For example, we can define a proxy for the list type.

> list :: Proxy []
> list = Proxy
  
Here's an example property: The empty set has no elements.

> prop_empty :: forall s. (Set s) => Proxy s -> Bool
> prop_empty _ = null (elements (empty :: s Int))

> prop_insert :: (Set s) => Proxy s -> Int -> s Int -> Bool
> prop_insert _ x y = member x (insert x y)

> prop_insert_inequal :: (Set s) => Proxy s -> Int -> Int -> s Int -> Property
> prop_insert_inequal _ x y s =
>     x /= y ==> member y s == member y (insert x s)  

> prop_elements :: (Set s) => Proxy s -> s Int -> Bool      
> prop_elements _ s = length (nub l) == length l where
>                         l = elements s
      
More? 

For example, one trivial implementation of sets uses lists:

> instance Set [] where
>    empty    = []
>    member   = elem
>    insert   = (:)
>    elements = nub

Let's make sure our implementation satisfies properties of sets.
     
    *Persistent> quickCheck $ prop_empty list
    *Persistent> quickCheck $ prop_elements list
    *Persistent> quickCheck $ prop_insert list
    *Persistent> quickCheck $ prop_insert_inequal list

Persistent vs. Ephemeral
------------------------

* An *ephemeral* data structure is one for which only one version is
available at a time: after an update operation, the structure as it
existed before the update is lost.

For example, conventional arrays are ephemeral.  After a location in an array
is updated, its old contents are no longer available.

* A *persistent* structure is one where multiple version are
simultaneously accessible: after an update, both old and new versions
are available. 

For example, a binary tree can be implemented persistently, so that after
insertion, the old value of the tree is still available.

Persistent data structures can sometimes be more expensive than their
ephemeral counterparts (in terms of constant factors and sometimes
also asymptotic complexity), but that cost is often insignificant
compared to their benefits:

   - better integration with concurrent programming (naturally lock-free)
   - simpler, more declarative implementations
   - better semantics for equality, hashing, etc.
   - access to *all* old versions (git for everything)

Next, we'll look at another persistent version of a common data structure:
Red-Black trees. These lectures demonstrate that functional programming is
adept at implementing sophisticated data structures. In particular, datatypes
and pattern matching make the implementation of persistent tree-like data
structures remarkably straightforward. These examples are drawn from Chris
Okasaki's excellent book [Purely Functional Data
Structures](http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504).

However, we'll only scratch the surface. There are many
industrial-strength persistent data structures out there.

  * Finger trees/Ropes, see  [Data.Sequence](http://www.haskell.org/ghc/docs/7.6.3/html/libraries/containers-0.5.0.0/Data-Sequence.html)
  * Size balanced trees, see [Data.Map](http://www.haskell.org/ghc/docs/7.6.3/html/libraries/containers-0.5.0.0/Data-Map.html)
  * Big-endian Patricia trees, see [Data.IntMap](http://www.haskell.org/ghc/docs/7.6.3/html/libraries/containers-0.5.0.0/Data-IntMap.html)
  * Hash array mapped tries, used in the [Clojure](http://en.wikipedia.org/wiki/Hash_array_mapped_trie) language
  * and [many more](http://cstheory.stackexchange.com/questions/1539/whats-new-in-purely-functional-data-structures-since-okasaki)
  

