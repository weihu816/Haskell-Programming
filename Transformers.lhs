Monad Transformers
==================

> {-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
>              FlexibleInstances, KindSignatures, InstanceSigs,
>              MultiParamTypeClasses, ScopedTypeVariables #-}
>
> module Transformers where
>
> import Control.Monad (liftM,ap)
>
> import State (State)              -- State monad developed in lecture
> import qualified State as S

Announcements
=============
* Sit with your project partner --- tables on Piazza

* Should have met with your project mentor. If not, do so today
* Meet with your mentor *each* Monday

* Download this file as well as the in-class exercise from the schedule page.

What monads have we seen so far?
================================

????

How do we use *multiple* monads at once?
========================================

Today, we will see how monads can be used to write (and compose)
*evaluators* for programming languages.

Let's look at a simple language of division expressions.

> data Expr = Val Int
>           | Div Expr Expr
>           deriving (Show)

Our first evaluator is *unsafe*.

> eval            ::  Expr -> Int
> eval (Val n)   =  n
> eval (Div x y) =  eval x `div` eval y

Here are two terms that we will use as running examples.

> ok  = (Val 1972 `Div` Val 2)
>       `Div` Val 23
> err = Val 2 `Div`
>       (Val 1 `Div`
>        (Val 2 `Div` Val 3))

The first evaluates properly and returns a valid answer, while the
second fails with a divide-by-zero exception.

~~~~~{.haskell}
    *Main> eval ok
    *Main> eval err
~~~~~

We don't like this `eval` because it can blow up with a divide-by-zero
error and stop the whole evaluator.

Of course, one way to fix the problem is to detect the error and then
continue with a default value (such as 0).

> evalDefault            ::  Expr -> Int
> evalDefault (Val n)   =  n
> evalDefault (Div x y) =
>   let m = evalDefault y in
>   if m == 0 then 0 else evalDefault x `div` m

But, no one likes this solution. It leads to buggy code.

Alternatively, we can use the `Maybe` type to treat the failure case more
gently: a `Nothing` result means that an error happened somewhere, while a
`Just n` result meant that evaluation succeeded yielding `n`.

> evalMaybe ::  Expr -> Maybe Int
> evalMaybe (Val n)   = return n
> evalMaybe (Div x y) = do
>    vx <- evalMaybe x
>    vy <- evalMaybe y
>    if vy == 0 then Nothing else return (vx `div` vy)

~~~~~{.haskell}
    *Main> evalMaybe ok
    *Main> evalMaybe err
~~~~~


Error Handling Via Exception Monads
-----------------------------------

The trouble with the above is that it doesn't let us know *where* the
divide by zero occurred. It would be nice to have a real exception
mechanism where we could say `Throw x` (for some descriptive value
`x`, such as a string describing the error), which would percolate up
to the top and tell us what the problem was.

Instead of using `Maybe` we can use the `Either` datatype for our
result.

~~~~~~~~{.haskell}

     data Either s a = Left s | Right a

~~~~~~~~
 
In this example, we can use `Right` like `Just` to indicate a successful result.
However, although `Left` is like `Nothing`, it carries a value of type `s`
denoting what the problem was. In the examples below, we will use type `String`
for `s`. This string will be our error message.

The type `Either s` is a monad, with a very similar definition to that of
`Maybe`.  Note that we are partially applying this type constructor!

~~~~~~~~~{.haskell}

   instance Monad (Either s) where
 
        (>>=)  :: Either s a -> (a -> Either s b) -> Either s b
        Right x >>= f = f x
        Left s  >>= _ = Left s

        return :: a -> Either s a
        return   = Right

~~~~~~~~~
 
Now YOU can use the `Either` monad (with do notation) to write a better exception-
throwing evaluator,

> --evalEither ::  Expr -> Either String Int
> evalEither (Val n)   = return n
> evalEither (Div x y) = do
>    vx <- evalEither x
>    vy <- evalEither y
>    if vy == 0 then throwError (errorS x y) else return (vx `div` vy)


where the helper function `errorS` generates the error string.

> errorS y m = "Error dividing " ++ show y ++ " by " ++ show m

~~~~~{.haskell}
    *Main> evalEither ok
    *Main> evalEither err
~~~~~


Counting Operations Using the State Monad
-----------------------------------------

Next, let's stop being so paranoid about errors and instead try to do
some *profiling*. Lets imagine that the `div` operator is very
expensive, and that we would like to *count* the number of divisions
that are performed while evaluating a particular expression.

As you might imagine, our old friend the state monad is going to be
just what we need here! The type of store that we'd like to use is
just the count of number of division operations, and we can store that
in an `Int`.

> type Prof = State Int
 
We'll need a way of incrementing the counter:

> tickProf :: Prof ()
> tickProf = do
>   x <- S.get      -- use get and put from the state monad
>   S.put (x + 1)

Now we can write a *profiling* evaluator,

> -- evalProf           :: Expr -> Prof Int
> evalProf (Val n)   = return n
> evalProf (Div x y) = do
>   m <- evalProf x
>   n <- evalProf y
>   tickStateInt
>   return (m `div` n)

and observe it at work

> goProf :: Expr -> IO ()
> goProf e = putStrLn $ "value: " ++ show x ++ ", count: " ++ show s
>            where (x,s) = S.runState (evalProf e) 0 :: (Int, Int)

~~~~~{.haskell}
    *Main> goProf ok
~~~~~

But... alas!  To get the profiling, we threw out the nifty error
handling that we had put in earlier!!

~~~~~{.haskell}
    *Main> goProf err
~~~~~

Transformers: Making Monads Multitask
=====================================

So, at the moment, it seems that Monads can do many things, but only
*one thing at a time* -- you can either use a monad to do the error-
management plumbing *or* to do the state-manipulation plumbing, but
not at the same time.  Is it too much ask for both? I guess we could
write a *mega-state-and-exception* monad that supports the operations
of both, but that doesn't sound like any fun at all!  Especially
since, if we later decide to add yet another feature, then we would
have to make up yet another mega-monad.

So we will take a different approach, where we will keep *wrapping* --
or "decorating" -- monads with extra features, so that we can take a
simple monad, and then add the Exception monad's features to it, and
then add the State monad's features and so on.

The key to doing this is to define exception handling, state passing,
etc., not as monads, but rather as *type-level functions from monads
to monads.*

This will require a little more work up-front (most of which is done
already in well-designed libraries), but after that we can add new
features in a modular manner.  For example, to get a mega
state-and-exception monad, we will start with a dummy `Identity`
monad, supply it to the `StateT` monad transformer (which yields
state-passing monad) and pass the result to the `ExceptT` monad
transformer, which yields the desired mega monad.

(Incidentally, if you are a Python programmer, the above may remind
some of you of the [Decorator Design Pattern][2] and other [Python
Decorators][3].)



Step 1: Describing Monads With Special Features
-----------------------------------------------

The first step to being able to compose monads is to define
typeclasses that describe monads with particular features. For
example, the notion of an *exception monad* is captured by the
typeclass

> class Monad m => MonadError e m where
>   throwError :: e -> m a

that describes monads that are also equipped with an appropriate
`throwError` function. This function takes an error value of
type `e`  (e.g. `String` for error messages). The result type is `m a` where
`a` is polymorphic --- in otherwords, we can throw an error in any
(monadic) context.

We can make `Either s` an instance of the above class
like this:

> instance MonadError s (Either s) where
>   throwError :: s -> Either s a
>   throwError = Left

(See what happens if you change `Left` to `throwError` in the
evaluator `evalEither` above. What is the new type of the evaluator?)

Similarly, we can bottle the notion of a *state monad* in a
typeclass...

> class Monad m => MonadState s m where
>   get      :: m s           -- State s s
>   put      :: s -> m ()

which describes monads equipped with extraction, and modification functions of
appropriate types.  We can then redefine the ticking operation to work for any
state monad (watch for ambiguity though!):

> tickStateInt :: MonadState Int m => m ()
> tickStateInt = do
>     (x :: Int) <- get
>     put (x + 1)

Naturally, we can make our `State` monad an instance of the above:

> instance MonadState s (State s) where
>   get        = S.get
>   put        = S.put

(Now go back and see what happens when you replace `tickProf` with
`tickStateInt` in `evalProf` above.)



Step 2: Using Monads With Special Features
------------------------------------------

Armed with these two typeclasses, we can write our exception-throwing,
step-counting evaluator quite easily:

> -- evalMega :: (MonadState Int m, MonadError String m) => Expr -> m Int
> -- evalMega :: Expr -> Mega Int
> evalMega (Val n)   = return n
> evalMega (Div x y) = do
>   n <- evalMega x
>   m <- evalMega y
>   if (m == 0)
>     then throwError $ errorS n m
>     else do
>       tickStateInt
>       return (n `div` m)

Note that it is simply the combination of the two evaluators from before -- we
use the error handling from `evalEither` and the profiling from `evalProf`.

Meditate for a moment on the type of above evaluator; note that it
works with *any monad* that is *both* a exception- and a state- monad!

***Exercise***: Add exceptions to the simple imperative programming assignment
from your last homework.


Interlude: Creating MegaMonads
-----------------------------

But where do we *get* monads that are both state-manipulating and
exception-handling?

One answer is that we can just define one!

***Exercise***: define one! Finish the instances for `Monad`, `MonadError
String`, and `MonadState Int`. Make sure that `evalMega` works with your
monad.

> data Mega a = Mega { runMega :: Int -> Either String (a, Int) }
>
> instance Monad Mega where
>   return :: a -> Mega a
>   return x   = Mega $ \s -> Right (x,s)
>   (>>=) :: Mega a -> (a -> Mega b) -> Mega b
>   ma >>= fmb = Mega $ \s -> case runMega ma s of
>                                Left str -> Left str
>                                Right (a, s') -> runMega (fmb a) s'
>
> instance Applicative Mega where
>   pure = return
>   (<*>) = ap
> instance Functor Mega where
>   fmap = liftM
>
> instance MonadError String Mega where
>   throwError :: String -> Mega a
>   throwError str = Mega $ \s -> Left str
> instance MonadState Int Mega where
>   get   = Mega $ \s -> Right (s,s)
>   put x = Mega $ \_ -> Right ((), x)


In the end, making your own mega-monad is a bit disappointing, since we've
already defined the state- and exception-handling functionality separately.

A better answer is to build them piece by piece.  We'll do this by defining
some type level functions that will *add* state manipulation or exception
handling to *any* pre-existing monad.



Step 3: Adding Features to Existing Monads
------------------------------------------

To add new features to existing monads, we use *monad transformers* --
type operators `t` that map each monad `m` to a monad `t m`.


**A Transformer For Exceptions**

Consider the following datatype declaration:

> data ExceptT e m a = MkExc { runExceptT :: m (Either e a) }
 
> instance Monad m => Monad (ExceptT e m) where
>
>   return :: forall a. a -> ExceptT e m a
>   return x = MkExc (return (Right x) :: m (Either e a))
>
>   (>>=) :: ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
>   p >>= f  = MkExc $ runExceptT p >>= (\ x -> case x of
>                                           Left e  -> return (Left e)
>                                           Right a -> runExceptT (f a))

> instance Monad m => Applicative (ExceptT e m) where
>    pure = return
>    (<*>) = ap
> instance Monad m => Functor (ExceptT e m) where
>    fmap = liftM

And next we ensure that the transformer is an exception monad by
equipping it with `throwError`

> instance Monad m => MonadError e (ExceptT e m) where
>   throwError :: e -> ExceptT e m a
>   throwError msg = MkExc (return (Left msg))



**A Transformer For State**

Next, we will build a transformer for the state monad, following more
or less the same recipe as for exceptions. Here is the type for the
transformer:

> newtype StateT s m a =  MkStateT { runStateT :: s -> m (a, s) }

Thus, in effect, the enhanced monad is a variant of the ordinary state
monad where a starting store is mapped to an _action_ in the monad `m`
that returns both a result of type `a` and a new store.

(Note that the monad transformer is *not* this:

    newtype StateT s m a = MkStateT (m (s -> (a, s)))

That is, it is not an `m` action yielding a store transformation.
Why is this not what we want?)

Next, we declare that the transformer's output is a monad:

> instance Monad m => Monad (StateT s m) where
>   return :: a -> StateT s m a
>   return x = MkStateT $ \s -> return (x,s)
>
>   (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
>   p >>= f = MkStateT $ \s -> do (r,s') <- runStateT p s
>                                 runStateT (f r) s'

> instance Monad m => Applicative (StateT s m) where
>   pure  = return
>   (<*>) = ap

> instance Monad m => Functor (StateT s m) where
>   fmap  = liftM

And finally we declare that the transformer is a state monad by
equipping it with the operations from `MonadState Int`:

> instance Monad m => MonadState s (StateT s m) where

>   get :: StateT s m s
>   get = MkStateT getIt
>     where getIt :: s -> m (s, s)
>           getIt s = return (s, s)

>   put :: s -> StateT s m ()
>   put s = MkStateT putIt
>     where putIt :: s -> m ((), s)
>           putIt _ = return ((), s)

Where are we now?

* If m is a monad, then StateT s m  is a state monad
* If m is a monad, then ExceptT e m is an error monad

But, what about `StateT s (ExceptT e m)`?  We know it is a state monad by the
above.  But, we'd *also* like it to be an error monad.

In other words, we need the following "pass through" properties to hold:

* If m is a *state* monad, then `ExceptT e` m is still a state monad
* If m is an *error* monad, then `StateT Int` m is still an error monad

We can do this in a generic way.



Step 4: Preserving Old Features of Monads
------------------------------------------

Of course, we must make sure that the original features of the monads
are not lost in the transformed monads.  The key ingredient of a
transformer is that it must have a function `lift` that takes an `m`
action and turns it into a `t m` action.  This will allow us to
transfer operations from the old monad into the transformed monad: any
operation on the input monad `m` can be directly lifted into an action
on the transformed monad, and so the transformation *preserves* all
the operations on the original monad.


> class MonadTrans (t :: (* -> *) -> * -> *) where   -- from Control.Monad.Trans (among other places)
>   lift :: Monad m => m a -> t m a


It is easy to formally state that `ExceptT e` is a bona-fide transformer by
making it an instance of the `MonadTrans` class:


> instance MonadTrans (ExceptT e) where
>   lift :: Monad m => m a -> ExceptT e m a
>   -- MkExc :: m (Either e a) -> ExceptT e m a
>   lift = MkExc . lift_ where
>     lift_  :: (Monad m) => m a -> m (Either e a)
>     lift_ mt = do x <- mt
>                   return (Right x)

Similarly, for the state monad transformer:

> instance MonadTrans (StateT s) where
>   lift :: Monad m => m a -> StateT s m a
>   -- MkStateT  :: (s -> m (a,s)) -> StateT s m a
>   lift ma = MkStateT $ \s -> do r <- ma
>                                 return (r,s)


Using `lift`, we can ensure that, if a monad was already an
"error" monad, then the result of the state transformer is too:

> instance MonadError e m => MonadError e (StateT s m) where
>   throwError :: e -> StateT s m a
>   throwError = lift . throwError

Similarly, if a monad was already a state-manipulating monad, then the
result of the exception-transformer is *also* a state-manipulating
monad:

> instance MonadState s m => MonadState s (ExceptT e m) where
>
>   get :: ExceptT e m s
>   get = lift get
>
>   put :: s -> ExceptT e m ()
>   put = lift . put
>

Step 5: Whew! Put It Together and Run
-------------------------------------

Finally, we can put all the pieces together and run the transformers.
We can also *order* the transformations differently (which can have
different consequences on the output, as we will see).

> evalM :: Expr -> Mega Int
> evalM = evalMega

-- StateT s (Either String) a === "  s -> Either String (a, s) "
-- ExceptT String (State s) a === " State s (Either String a) " === "s -> (Either String a, s)"

> evalExSt :: Expr -> StateT Int (Either String) Int
> evalExSt = evalMega
>
> evalStEx :: Expr -> ExceptT String Prof Int
> evalStEx = evalMega

We can run this as follows:

> goExSt :: Expr -> IO ()
> goExSt e = putStr $ pr (evalExSt e) where
>     pr :: StateT Int (Either String) Int -> String
>     pr f = case runStateT f 0 of
>                      Left s         -> "Raise: " ++ s ++ "\n"
>                      Right (v, cnt) -> "Count: " ++ show cnt ++ "\n" ++
>                                         "Result: " ++ show v ++ "\n"

> goStEx :: Expr -> IO ()
> goStEx e = putStr $ pr (evalStEx e) where
>    pr :: ExceptT String Prof Int -> String
>    pr f = "Count: " ++ show cnt ++ "\n" ++ show r ++ "\n"
>      where (r, cnt) = S.runState (runExceptT f) 0

~~~~~{.haskell}
    *Main> goExSt ok
    *Main> goExSt err
    *Main> goStEx ok
    *Main> goStEx err
~~~~~

Step 6: Getting the original monads back
-----------------------------------------

It seems a little silly that the monad definitions for `State` and for
`StateT` share so much code. What if we want a monad that is *only* a state
monad, and not layered on top of something else?

As alluded to above, we can define an `Identity` monad to use under
any other.

> data Id a = MkId a deriving Show

> instance Monad Id where
>   return x = MkId x
>   (MkId p) >>= f  = (f p)

> instance Applicative Id where
>   pure  = return
>   (<*>) = ap
> instance Functor Id where
>   fmap  = liftM


> type State2 s  = StateT  s Id   -- isomorphic to State s
> type Either2 s = ExceptT s Id   -- isomorphic to Either s
