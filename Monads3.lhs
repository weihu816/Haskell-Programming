Monads III
==========

> {-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
> {-# OPTIONS_GHC -fdefer-type-errors #-}

> module Monads3 where

> import State
> import Control.Monad (liftM, ap,forM_,replicateM_)
> import System.Random (randomRIO)

> import Data.Map (Map)
> import qualified Data.Map as Map
> import qualified Data.Vector.Mutable as V

> import Control.Monad.ST


State Monad Exercise
====================
See [RandomGen](RandomGen.html)

Parsing Exercise
================
See [Xml](Xml.html)


The Functor/Applicative/Monad Laws
==================================

Recall that type classes often come with laws or properties that we expect all
instances of the type class to satisfy.

For example, we expect instances of the `Eq` type class to implement an
equivalence relation. The equality function must be reflexive, symmetric and
transitive.

Similarly, instances of the `Monoid` type class need to also satisfy
properties of identity and associativity.

~~~~~{.haskell}

       class Monoid a where
           mempty  :: a
           (<>)    :: a -> a -> a

~~~~~

An example Monoid is String --- `mempty` is the empty string and `<>` is
string concatenation. (This is also true more generally for any list.)

Technically, the Monoid laws require that the following equalities hold
for any instance of this class.

~~~~~{.haskell}

       mempty <> x == x                        -- left identity
 
       x <> mempty == x                        -- right identity

       x1 <> (x2 <> x3) == (x1 <> x2) <> x3    -- associativity

~~~~~~

Monad Laws
----------

Like `Eq` and `Monoid`, the notion of a monad requires that the `return` and
`>>=` operators satisfy some simple properties.  In fact, the properties
for monads are very similar to that for monoids.

The first two properties concern the link between `return` and `>>=`:

~~~~~{.haskell}

   (return x) >>= f  ==  f x    --   (1)

   mx >>= return     ==  mx     --   (2)

~~~~~

Intuitively, equation (1) states that if we return a value `x` and
then feed this value into a function `f`, this should give the same
result as simply applying `f` to `x`.  Conversely, equation (2) states
that if we feed the results of a computation `mx` into the function
return, this should give the same result as simply performing `mx`.
Together, these equations express -- modulo the fact that the second
argument to `>>=` involves a binding operation -- that `return` is a
left and right identity for `>>=`.

The third property expresses (again modulo binding) that
`>>=` is associative:

~~~~~{.haskell}

   (mx >>= f) >>= g  ==  mx >>= (\x -> (f x >>= g)) 	-- (3)

~~~~~

Note that we cannot simply write `mx >>= (f >>= g)` on the right-hand
side of this equation, as this would not be type correct.
 
It is instructive to rewrite these laws in terms of the `do` notation...


~~~~~{.haskell}

   (return x) >>= (\y -> f y) ==  f x    --   (1)

      do y <- return x     ==    f x
         f y

   mx >>= (\x -> return x)     ==  mx     --   (2)

      do x <- mx         ==    mx
         return x

   (mx >>= f) >>= (\y -> g y)  ==  mx >>= (\x -> (f x >>= (\y -> g y))) 	-- (3)

      do y <- do z <- mx       ==  do x <- mx
                 f z                  y <- fx
         g y                          g y

~~~~~

Discussion
----------

* What goes wrong if these laws are violated by a Monad instance?

* Where do these laws come from?

* How can knowing about these laws make us better Haskell programmers?

* How could you verify these laws for specific Monads (such as Maybe)?

Applicative Laws
----------------

Like the Monad laws, the Applicative laws also describe how the members of this type
class interact with each other.

1. Identity

~~~~~{.haskell}

     pure id <*> v == v

~~~~~

2. Composition

~~~~~{.haskell}
     pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
~~~~~
3. Homomorphism

~~~~~{.haskell}
     pure f <*> pure x == pure (f x)
~~~~~

4. Interchange

~~~~~{.haskell}
     u <*> pure y == pure ($ y) <*> u
~~~~~

Functor Laws
------------

For any functor, we expect that the following identities hold:

~~~~~{.haskell}
     fmap id  ==  id
~~~~~

~~~~~{.haskell}
     fmap (f . g)  ==  fmap f . fmap g
~~~~~

Combinations
------------

Finally, because all Monads are Applicatives, and all Applicatives are
Functors, the definitions must agree:

* Functor/Applicative

~~~~~{.haskell}

     fmap f m  == pure x <*> m

~~~~~

* Applicative/Monad

~~~~~{.haskell}

     pure      == return
 
     m1 <*> m2 == m1 >>= \x1 -> m2 >>= \x2 -> return (x1 x2)

~~~~~


The ST and IO Monads
====================

Recall that interactive programs in Haskell are written using the
type `IO a` of "actions" that return a result of type `a`, but may
also perform some input/output.  A number of primitives are
provided for building values of this type, including:

~~~~~{.haskell}
return  :: a -> IO a
(>>=)   :: IO a -> (a -> IO b) -> IO b
~~~~~

The presence of `return` and `>>=` means that we can treat `IO` as a
monad, and hence (as we've seen) that the `do` notation can be used to
write imperative programs.  For example, we can use mutable arrays
in Haskell in the IO monad:

>
> getFullLine :: IO String
> getFullLine = getChar >>= (\c ->
>           if c == '\n' then return []
>           else getLine >>= (\cs -> return (c:cs)))
>


One way to understand the `IO` monad is as a special case of the State monad,
in which the internal state is a suitable representation of the "state of the
world":

~~~~~{.haskell}
   type RealWorld = ...

   type IO a  = RealWorld -> (a,RealWorld)
~~~~~

That is, an IO action can be viewed as a function that takes the current state
of the world as its argument, and produces a value and a modified world as its
result. In other words, we have this correspondence:

In reality, Haskell systems such as GHC implement actions in a more
efficient manner, but for the purposes of understanding the behavior of
actions, the above interpretation can be useful.

Mutable Arrays in Haskell
-------------------------

The two most important actions associated with the State Monad are accessing
and modifying the `Store`.

~~~~~~{.haskell}
   get :: State Store Store
   put :: a -> Store -> State Store ()
~~~~~~

What does this mean for the interpretation of the IO monad as the State monad,
using the 'RealWorld' as the state?

~~~~~{.haskell}
   type IO a = State RealWorld a
~~~~~

In this case, `get` and `put` are not realistic. We cannot expect our program
to give us access to a reference to the entire `RealWorld`? What would we do
with it?  Furthermore, we should be able to change out the entire `ReadWorld`
with a new version (assuming we could construct one in the first place).

Instead, the IO monad gives us access to *parts* of the real world. For
example, the IO monad can give us access to a part of the computer's memory,
such as a mutable reference or mutable array.

The `Data.Vector.Mutable` library provides support for working with mutable
arrays in Haskell using an interface that is like the following:

~~~~~~~~~~{.haskell}
    type MVector e   -- type of arrays containing elements of type e
 
    read       :: MVector e -> Int -> IO e
    write      :: MVector e -> Int -> e -> IO ()

    replicate  :: Int -> e -> IO (MVector e)
    replicateM :: Int -> IO e -> IO (MVector e)
    length     :: MVector e -> Int
~~~~~~~~~~

You can think of a `MVector` as a description of the part of the real world
that we want to look at.  In that case, `read` and `write` look a lot like
`get` and `put`.  The only difference is we need to say what part of the store
we want to use with get and put; we aren't working with the entire store.

The `replicate` operation allows us to construct vectors (i.e. define a part
of the RealWorld that we want access to). The `Int` is the length of the
vector, and the `e` argument is used to provide an initial value to all of the
arguments of the vector.

For example, the code below constructs a new vector of length 100, prints out
its length, write to a particular location in that vector, and then access the
data from that same location.

> example = do
>    v <- V.replicate 100 Nothing      -- Char[] v = new Char[100];
>    putStrLn (show (V.length v))      -- System.out.println(v.length);
>    V.write v 10 (Just 'a')           -- v[10] = 'a';
>    x <- V.read v 10                  -- Int x = v[10];
>    putStrLn (show x)                 -- System.out.println(v[10]);

We can also create multidimensional arrays as vectors of vectors.

> make_matrix :: Int -> Int -> IO (MVector (MVector Int))
> make_matrix n m =
>   V.replicateM n (V.replicate m 0)

> read_matrix vec i j = do
>   m0 <- V.read vec i
>   V.read m0 j

> write_matrix vec i j x = do
>   m0 <- V.read vec i
>   V.write m0 j x

As a larger example, we can construct 2-dimensional arrays and use them to
solve the classic dynamic programming problem of [making
change](https://en.wikipedia.org/wiki/Change-making_problem).

> make_changeM :: [Int] -> Int -> IO Int
> make_changeM coins n = do
>   let num_coins = length coins
>   m <- make_matrix (num_coins + 1) (n + 1)
>   forM_ [ 0 .. n ] $ \i -> do
>      write_matrix m 0 i i
>   forM_ (zip coins [ 1 .. num_coins ]) $ \(c,ci) ->
>     forM_ [ 1 .. n ] $ \r -> do
>        if (c == r) then
>           -- Use that coin
>           write_matrix m ci r 1
>        else if (c > r) then do
>           -- We use the previous solution for making r,
>           -- excluding coin c
>           v <- read_matrix m (ci-1) r
>           write_matrix m ci r v
>        else do
>         -- We can use c
>         -- We need to decide which one of the following solutions is the best:
>         -- 1. Using the previous solution for making r
>         --     (without using c)
>         -- 2. Using the previous solution for making r - c
>         --     (without using c) plus this 1 extra coin.
>           v1 <- read_matrix m (ci - 1) r
>           v2 <- read_matrix m ci (r - c)
>           write_matrix m ci r (min v1 (1 + v2))
>   read_matrix m num_coins n

Local State and the ST monad
----------------------------

One thing to notice about `makeChangeM` above is that all of the state that is
use by this function is *local* to the function. It needs to allocate the 2D
array, but once the answer is calculated, this array does not persist past the
execution of this function.

In this case, it is somewhat annoying that the type of `makeChangeM` has `IO`
in it. We should be able to treat it like a pure functional program.

It turns out that Haskell's type system is powerful enough to allow us to do
just that.  The library that we have been using is actually more general than
I've been showing you. The `MVector` type in this file is really an
abbreviation for a more general type that mentions the "store" that the vector
is defined within.

> type MVector e = V.MVector RealWorld e

The `runST` function allows us to run a stateful computation as long as we
know that all of the state is local.

When we use vectors from the IO monad, all we know about them is that they
come from the real world.

However, we can also use mutable vectors in the ST monad, which tracks the
part of the state that they reference. For example, we could also give
`make_matrix` above the following type:

> -- make_matrix :: Int -> Int -> ST s (V.MVector s (V.MVector s Int))

Furthermore, the `make_changeM` matrix has this intriguing type~

> -- make_changeM :: [Int] -> Int -> ST s Int

Types like this are where the `ST` monad really shines. This type tells us
that the function only uses *local state* and that the state doesn't escape
elsewhere into the computation. How do we know that looking at the type?

Because we know that this state can be isolated, we can run the computation to
produce a pure value.

        runST :: (forall s. ST s a) -> a

For example,

> make_change :: [Int] -> Int -> Int
> make_change coins n = undefined -- runST (make_changeM coins n)

Note that we have to isolate the state. If we allow it to escape through the
result type of the ST computation

> -- bad = runST (make_matrix 10 10)

Or through some variable in the context that the computation refers to

> {-
> bad = do
>   m <- make_matrix 10 10
>   let y = runST (read_matrix m 5 5)
>   return y
> -}

we will get an error.
