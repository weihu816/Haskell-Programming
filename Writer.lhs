Monad Transformers
==================

> {-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
>              FlexibleInstances, KindSignatures #-}
>
> module Writer where
>
> import Control.Monad.Except
> import Control.Monad.State
> import Control.Monad.Writer

Recall our simple example of a language with division operations.

> data Expr = Val Int
>           | Div Expr Expr
>           deriving (Show)

> ok  = (Val 1972 `Div` Val 2)
>       `Div` Val 23
> err = Val 2 `Div`
>       (Val 1 `Div`
>        (Val 2 `Div` Val 3))


> -- | Generate an error string.
> errorS y m = "Error dividing " ++ show y ++ " by " ++ show m

> -- | Increment the counter
> tick :: (MonadState Int m) => m ()
> tick = get >>= put . (+ 1)
>

Tracing Operations Via Logger Monads
====================================

As a last example of monad transformers, we will spice up our computations to
also *log* messages about what is happening (a pure variant of the usual
method where we just print the messages to the screen).  This can be done with
the standard Writer monad, which supports a `tell` action that logs the string
you want (and allows you to later view the entire log of the computation).

To accomodate logging, we juice up our evaluator directly as follows:

> eval2 :: (MonadError String m, MonadState Int m,
>           MonadWriter String m) =>
>           Expr -> m Int
> eval2 v =
>   case v of
>     Val n   -> do tell $ msg v n
>                   return n
>     Div x y -> do n <- eval2 x
>                   m <- eval2 y
>                   if m == 0
>                     then throwError $ errorS y m
>                     else do tick
>                             tell $ msg (Div (Val n) (Val m)) (n `div` m)   -- new
>                             return  $ n `div` m

The `msg` function is simply:

> msg :: (Show a, Show b) => a -> b -> String
> msg t r = "term: " ++ show t ++ ", yields " ++ show r ++ "\n"

Note that the only addition to the previous evaluator is the `tell`
operations! We can run the above using

> evalWSE :: Expr -> WSE Int
> evalWSE = eval2

where `WSE` is a type abbreviation:

> type WSE a = WriterT String (StateT Int (Either String)) a

That is, we simply use the `WriterT` transformer to decorate the underlying
monad carrying the state and exception information.

~~~~~{.haskell}
    *Main> runStateT (runWriterT (evalWSE ok)) 0
    *Main> runStateT (runWriterT (evalWSE err)) 0
~~~~~

The results are a bit unreadable, but we can write our own pretty-printer...

> ppr :: Show a => WSE a -> IO ()
> ppr m = putStrLn $
>         case runStateT (runWriterT m) 0 of
>            Left s            -> "Error: " ++ s
>            Right ((v, w), s) -> "Log:\n"  ++ w       ++ "\n" ++
>                                 "Count: " ++ show s  ++ "\n" ++
>                                 "Value: " ++ show v  ++ "\n"

after which we get:

~~~~~{.haskell}
    *Main> ppr $ evalWSE ok
    *Main> ppr $ evalWSE err
~~~~~



















*How come we didn't get any log in the error case?*

Once again, the answer lies in the *order* in which we compose the
transformers; since the error wraps the log, if the computation fails,
the log gets thrown away. Instead, we can just wrap the other way
around...

> type ESW a = ExceptT String (WriterT String (State Int)) a
>
> evalESW :: Expr -> ESW Int
> evalESW = eval2

after which, everything works just fine!

> ppr2 :: Show a => ESW a -> IO ()
> ppr2 m = putStrLn $
>            "Log:\n"  ++ log ++ "\n" ++
>            "Count: " ++ show cnt ++ "\n" ++
>            result
>     where ((res, log), cnt) = runState (runWriterT (runExceptT m)) 0
>           result   = case res of
>                        Left s -> "Error: " ++ s
>                        Right v -> "Value: " ++ show v

~~~~~{.haskell}
    *Main> ppr2 $ evalESW err
~~~~~
