
The Concurrency Monad Transformer
=================================

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

> module TransC where

> import Control.Monad.Trans
> import Control.Monad.Writer(Writer, runWriter, tell)
> import Control.Monad (ap, liftM)

> import Test.HUnit

The Concurrency monad that we presented in class was specialized to atomic
actions in the IO monad. In this problem, we generalize the atom constructor
so that it applies to computation in *any* monad.

> data Action m = 
>        Atom (m (Action m))           -- an atomic computation, returning a new action
>      | Fork (Action m) (Action m)    -- create a new thread
>      | Stop                          -- terminate this thread

We will add this monad type as an additional argument to `C`.

> newtype C m a = C { runC :: (a -> Action m) -> Action m }

First, make this new type a monad:

> -- a)

> instance Monad m => Monad (C m) where
>    
>    return x = error "return: unimplemented"
>    
>    m >>= f  = error "bind: unimplemented"

> instance Monad m => Applicative (C m) where
>     pure  = return
>     (<*>) = ap

> instance Monad m => Functor (C m) where
>     fmap = liftM

> -------------------------------------------------

Next, to make sure you follow how these generalizations work, add the type
signatures for our library of concurrency operations. Of course, you can ask
GHCi for these types, but try to figure them out yourself first.

> -- b)

> 
> atom m = C $ \k -> Atom (liftM k m)

>  
> action m = runC m $ const Stop

> 
> run m = sched [action m]

> 
> fork m = C $ \k -> Fork (action m) (k ())

> 
> stop = C $ const Stop

> 
> sched [] = return ()
> sched (Atom m : cs) = m >>= \ a -> sched (cs ++ [a])
> sched (Fork a1 a2 : cs) = sched (cs ++ [a1,a2])  
> sched (Stop : cs) = sched cs

> -------------------------------------------------
> -- return/bind test case

At this point, you should probably test your implementation of return and
bind. Using the examples from the lecture as implementation, define a
concurrent program that does something interesting with the IO monad. (Because
all of the action is in IO, you won't be able to turn this into a unit test,
but you can describe the expected behavior when you `run` it in comments after
the code.)

> concurrentProg :: C IO ()
> concurrentProg = error "concurrentProg: undefined"


> -------------------------------------------------

The next step is to make the type `C` an instance of the monad transformer
class. That means, you need to somehow figure out how to lift operations from
the underlying monad to `C`.

> -- c)

> instance MonadTrans C where
>     lift = error "lift: unimplemented"

For example, one place to use `lift` is in class instances, such as `Output`. 

> class Monad m => Output m where  
>    write :: String -> m ()

Here we can lift any monad that supports some kind of output method to the
concurrency monad.

> instance (Output m) => Output (C m) where
>    write str = lift (write str)

This includes the `IO` monad, as in our example in class:

> instance Output IO where
>    write = putStr

As well as the `Writer` monad:

> instance Output (Writer String) where
>    write = tell 

For example, given an output function:

> example :: Output m => C m ()
> example = do fork (write "Hello " >> write "552")
>              write "CIS"

We can run it in the IO monad

          Main>  run (example :: C IO ())
          Hello CIS552
  
Or run it in the writer monad

> runConcurrentWriter :: C (Writer String) () -> String
> runConcurrentWriter x = snd $ runWriter $ run x

> testWrite :: Test
> testWrite = runConcurrentWriter example ~?= "Hello CIS552"


> ---------------------------------------------------

> -- d)

Now, consider this simple puzzle: How can we merge an infinite list of
infinite lists?

For example, here are some infinite lists:

> ones,twos,threes :: String
> ones   = '1' : ones
> twos   = '2' : twos
> threes = '3' : threes 

*Main> take 10 ones

*Main> take 20 twos

And here is an infinite list of infinite lists.

> allnums :: [String]
> allnums = map nums [ 1::Integer .. ] where
>    nums i = show i ++ "  " ++ nums i

*Main> take 10 (map (take 10) allnums)


How could we merge `allnums` to create a single list that contains *all*
of the elements of all of the infinite lists?  (Hint: `concat`
typechecks, but it won't work!)

The trick is to `write` all of the strings concurrently, letter by
letter.

First, create a computation that writes each string letter by letter, in
parallel.

> writeLetterByLetter :: (Output m) => String -> m ()
> writeLetterByLetter = error "writeLetterByLetter: unimplemented"

> testLBL :: Test
> testLBL = "testLBL" ~: (runConcurrentWriter catdog ~?= "cdaotg") where
>    catdog = do fork (writeLetterByLetter "cat")
>                writeLetterByLetter "dog"

Next, create a computation that writes all strings in a list in parallel:

> parallelWrite :: Output m => [String] -> C m ()
> parallelWrite = error "parallelWrite: unimplemented"                            
  
Finally, we can merge these infinite strings by running the parallelWrite
computation.

> merge :: [String] -> String
> merge ss = runConcurrentWriter (parallelWrite ss) 

*Main> take 20 (merge allnums)

> testMerge :: Test
> testMerge = merge ["cat","dog"] ~?= "cdaotg"
