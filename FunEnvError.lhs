FunEnvError
===========

Let's revise the environment based interpreter
[FunEnv](http://www.seas.upenn.edu/~cis552/lectures/FunEnv.html) so that it
can return error messages when things go wrong instead of producing junk
values.

If you like, you may edit the [hs](FunEnvError.hs) version of this file.


> {-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches
>     -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

> {-# LANGUAGE RecursiveDo, FlexibleContexts, FlexibleInstances #-}

> module FunEnvError where

> import Control.Monad.Fix
> import Control.Monad.Except

> import Data.Map (Map)
> import qualified Data.Map as Map

> import Parsers
> import FunSyntax

> import Test.HUnit

> type Environment = Map Variable Value

The big change is that our evaluation function may sometimes fail, for example
if we try to evaluate "2 + true" or "X + 1".

So we will give it this type, where if there is a runtime error, we can return
a string describing that error.

     eval :: Expression -> Environment -> Either String Value

To make this change we need one small update to the definition of values: we
need to let function values also produce errors when they are evaluated.

> data Value =
>    IntVal  Int
>  | BoolVal Bool
>  -- note! function values can go wrong when they are run
>  | FunVal (Value -> Either String Value)

> instance Show Value where
>     show (IntVal i)  = show i
>     show (BoolVal b) = show b
>     show (FunVal _)  = "<function>"   -- can't show functions


Now let's look at the function for evaluating binary operators.  Most of the
cases just return the appropriate value. However, the last case uses the
`throwError` function to report the problem.

> evalB :: Bop -> Value -> Value -> Either String Value
> evalB Plus   (IntVal i1) (IntVal i2) = return $ IntVal  (i1 + i2)
> evalB Minus  (IntVal i1) (IntVal i2) = return $ IntVal  (i1 - i2)
> evalB Times  (IntVal i1) (IntVal i2) = return $ IntVal  (i1 * i2)
> evalB Gt     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 > i2)
> evalB Ge     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 >= i2)
> evalB Lt     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 < i2)
> evalB Le     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 <= i2)
> evalB _ _ _ = throwError $ "Invalid argument to binary operator"


We can use `throwError` because `Either String` is an instance of the class
`MonadError String`, i.e. the class of monads that can report errors using
strings.

Furthermore, we can throw an error if there is no value for a variable
available in the environment.

> tLookup :: Variable -> Map Variable Value -> Either String Value
> tLookup x env =
>   case Map.lookup x env of
>     Just ty -> return ty
>     Nothing -> throwError $ "Unbound variable " ++ x

Now it is your turn: complete the following cases of the environment based
interpreter.  You can use the repl or the test cases below for testing.  The
first five cases are just like the evaluator for WHILE with error, so if you
get stuck take a look at what you did on the prior [in-class
exercise](http://www.seas.upenn.edu/~cis552/current/hw/trans/).

> eval :: Expression -> Environment -> Either String Value
> eval (Var x)       s = tLookup x s
> eval (IntExp i)    s = return $ IntVal i
> eval (BoolExp i)   s = return $ BoolVal i
> eval (Op o e1 e2)  s = do v1 <- eval e1 s
>                           v2 <- eval e2 s
>                           evalB o v1 v2
> eval (If e1 e2 e3) s = do v1 <- eval e1 s
>                           case v1 of
>                             BoolVal b -> if b then eval e2 s else eval e3 s
>                             _ -> throwError "if requires a boolean"
> eval (Fun x e)     s = return $ FunVal (\v -> eval e (Map.insert x v s))
> eval (App fun arg) s = do v1 <- eval fun s
>                           v2 <- eval arg s
>                           case v1 of
>                              FunVal g -> g v2
>                              _ -> throwError "app requires a boolean"

Because of the recursive structure of let, the last case is quite tricky. We
need to use a Haskell extension (recursive do) to enable the recursion to work
through the monadic code. See what happens when you change the `mdo` below to
just be `do`.  (Recursive do is available for monads that are instances of the
[MonadFix](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad-Fix.html)
class).

> eval (Let x e1 e2) s = mdo
>   v  <- eval e1 s'
>   let s' = Map.insert x v s
>   eval e2 s'

> -- Testing code

> isErr :: Either String Value -> Test
> isErr (Left _)  = TestCase $ assert True
> isErr (Right _) = TestCase $ assert False

> isIntVal :: Int -> Either String Value -> Test
> isIntVal y (Left _)  = TestCase $ assert False
> isIntVal y (Right (IntVal x)) = TestCase $ assert (x == y)


> tests = TestList [
>    "1 + true" ~: isErr $ eval (Op Plus (IntExp 1) (BoolExp True)) Map.empty,
>    "1 1"      ~: isErr $ eval (App (IntExp 1) (IntExp 1)) Map.empty,
>    "if 1 .."  ~: isErr $ eval (If (IntExp 1) (IntExp 2) (IntExp 3)) Map.empty,
>    "X"        ~: isErr $ eval (Var "X") Map.empty,
>    "FACT 6"   ~: isIntVal 120 $ eval factExp Map.empty ]

> -- repl

> replE :: IO ()
> replE = do
>    putStr "%> "
>    line <- getLine
>    case parse line of
>      Just exp ->
>          case eval exp Map.empty of
>            Left str  -> putStrLn str >> replE
>            Right val -> putStrLn (show val) >> replE
>      Nothing -> putStrLn "what?!?" >> replE
