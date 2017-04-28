Monad Transformers
==================

This file contains an inclass exercise involving monad transformers. Make sure that
you download the [zipfile](trans.zip) associated with it.
 
> {-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
>     FlexibleInstances #-}
> {-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}
> module Main where

For simplicity, we define the syntax of the language in a [separate file](WhileExn.html).
 
> import WhileExn

> import Data.Map (Map)
> import qualified Data.Map as Map
 
> import Control.Applicative
> import Control.Monad (liftM, liftM2)

> import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
> import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)

> import Test.HUnit hiding (State)
 
> main :: IO ()
> main = return ()

Expression Evaluator

> ------------------------

1. First, make sure that you understand how the expression evaluator works.
   Then, look at the inferred type of `evalE` in ghci.
   (Note: evalOp below was called evalB in your hw.)
 
> type Store = Map Variable Value
 
> evalE (Var x)      = do
>   m <- get
>   case (Map.lookup x m) of
>     Just v ->  return v
>     Nothing -> return (IntVal 0)
> evalE (Val v)      = return v
> evalE (Op e1 o e2) = evalOp o <$> evalE e1 <*> evalE e2


2. Next, modify evalOp and evalE so that it uses `throwError` for runtime errors
   - in the case of divide by zero, use `IntVal 1` as the error code
   - in the case of invalid args to the operator, use `IntVal 2`
  - use code (IntVal 0) for undefined variables.
 
> evalOp :: Bop -> Value -> Value -> Value
> evalOp Plus   (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
> evalOp Minus  (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
> evalOp Times  (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
> evalOp Divide (IntVal _ ) (IntVal 0)  = IntVal 0
> evalOp Divide (IntVal i1) (IntVal i2) = IntVal (i1 `div` i2)
> evalOp Gt     (IntVal i1) (IntVal i2) = BoolVal (i1 > i2)
> evalOp Ge     (IntVal i1) (IntVal i2) = BoolVal (i1 >= i2)
> evalOp Lt     (IntVal i1) (IntVal i2) = BoolVal (i1 < i2)
> evalOp Le     (IntVal i1) (IntVal i2) = BoolVal (i1 <= i2)
> evalOp _ _ _ = IntVal 0
 

Tests for expression evaluator

>  ------------------------------

To test the expression evaluator we have to pick a specific
monad to use; one that satisfies both MonadState and MonadError
constraints.

We can construct this monad easily by layering the exception monad on top of
the usual State monad.

> type M = ExceptT Value (State Store)
 
> executeE :: Expression -> Store -> (Either Value Value, Store)
> executeE b st = runState (runExceptT comp) st where
>     comp :: M Value
>     comp = evalE b

> raisesE :: Expression -> Value -> Test
> s `raisesE` v = case (executeE s Map.empty) of
>    (Left v',_) -> v ~?= v'
>    _            -> TestCase $ assertFailure "Error in raises"


 
> test_undefined :: Test
> test_undefined = "undefined variable" ~:
>   ((Var "Y") `raisesE` IntVal 0)

> test_divByZero :: Test
> test_divByZero = "divide by zero" ~:
>   ((Op  (Val (IntVal 1)) Divide (Val (IntVal 0))) `raisesE` IntVal 1)

> test_badPlus :: Test
> test_badPlus = "bad arg to plus" ~:
>   (Op (Val (IntVal 1)) Plus (Val (BoolVal True))) `raisesE` IntVal 2

 
> test_expErrors :: Test
> test_expErrors = "undefined variable & division by zero" ~:
>   TestList [ test_undefined, test_divByZero, test_badPlus ]
 
Statement Evaluator

>  -------------------

3. Now modify the statement evaluator so that it throws errors
   Use code (IntVal 2) for integer conditions in `While` and `If` statements.
 
> evalS :: (MonadError Value m, MonadState Store m) => Statement -> m ()
> evalS w@(While e (Block ss))    = do
>   v <- evalE e
>   case v of
>     BoolVal True  -> evalB (Block (ss ++ [w]))
>     BoolVal False -> return ()
>     IntVal  _     -> return ()
> evalS (Assign x e)     = do
>     v <- evalE e
>     m <- get
>     put (Map.insert x v m)
> evalS (If e b1 b2)      = do
>     v <- evalE e
>     case v of
>       BoolVal True  -> evalB b1
>       BoolVal False -> evalB b2
>       IntVal _ -> return ()
> evalS (Try _ _ _)   = error "evalS: unimplemented"
> evalS (Throw _)     = error "evalS: unimplemented"


> 
> evalB (Block ss) = mapM_ evalS ss
 
4. Add user-level exceptions.

- `Throw e` should evaluate the expression `e` and an exception
  carrying the value of `e`

- `Try s x h` should execute the statement `s` and if, in the course
  of execution, an exception is thrown, then the exception value should
  be assigned to the variable `x` after which the *handler* statement
  `h` is executed.

Note: the `catchError` function in
[Control.Monad.Except](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html)
will be necessary for `Try` statements.
 
For example, this [code](try.imp)
 
> test1 = do
>   mb  <- parse "try.imp"
>   case mb of
>     Right b -> run b
>     Left _  -> putStrLn "parse error"

Should print
 
       Result: Right ()
       Output Store: fromList [("a",IntVal 100),("e",IntVal 1),("x",IntVal 0),("y",IntVal 1),("z",IntVal 101)]

 
> execute :: Block -> Store -> (Either Value (), Store)
> execute b st = runState (runExceptT (evalB b)) st

> run :: Block -> IO ()
> run block = do let (r, s) = execute block Map.empty
>                putStr "Result: "
>                putStrLn (show r)
>                putStr "Output Store: "
>                putStrLn (show s)


> raises :: Block -> Value -> Test
> s `raises` v = case (execute s Map.empty) of
>    (Left v',_) -> v ~?= v'
>    _            -> TestCase $ assertFailure "Error in raises"

 
> -------------------------------------------------------------------------
