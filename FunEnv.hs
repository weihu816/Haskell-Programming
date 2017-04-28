{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module FunEnv where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

import FunSyntax

data Value =
   IntVal  Int
 | BoolVal Bool
 -- new! function values
 | FunVal (Value -> Value)

instance Show Value where
    show (IntVal i)  = show i
    show (BoolVal b) = show b
    show (FunVal _)  = "<function>"   -- can't show functions

evalB :: Bop -> Value -> Value -> Value
evalB Plus   (IntVal i1) (IntVal i2) = IntVal  (i1 + i2)
evalB Minus  (IntVal i1) (IntVal i2) = IntVal  (i1 - i2)
evalB Times  (IntVal i1) (IntVal i2) = IntVal  (i1 * i2)
evalB Gt     (IntVal i1) (IntVal i2) = BoolVal (i1 > i2)
evalB Ge     (IntVal i1) (IntVal i2) = BoolVal (i1 >= i2)
evalB Lt     (IntVal i1) (IntVal i2) = BoolVal (i1 < i2)
evalB Le     (IntVal i1) (IntVal i2) = BoolVal (i1 <= i2)
evalB _ _ _ = IntVal 0

type Environment = Map Variable Value

slookup :: Variable -> Environment -> Value
slookup x m = case (Map.lookup x m) of
    Just v ->  v
    Nothing -> (IntVal 0)

eval :: Expression -> Environment -> Value
eval (Var x)       s = slookup x s
eval (IntExp i)    s = IntVal i
eval (BoolExp i)   s = BoolVal i
eval (Op o e1 e2)  s = (evalB o) (eval e1 s) (eval e2 s)
eval (If e1 e2 e3) s = case eval e1 s of
                         BoolVal b -> if b then eval e2 s else eval e3 s
                         _  -> IntVal 0

eval (Fun x e)     s    = FunVal g where
                            g :: Value -> Value
                            g v = eval e (Map.insert x v s)

eval (App fun arg) s    = case eval fun s of
                            IntVal i  -> IntVal 0
                            BoolVal b -> IntVal 0
                            FunVal g  -> g (eval arg s)

eval (Let x e1 e2) s = let v  = eval e1 s'
                           s' = Map.insert x v s
                       in eval e2 s'

parseAndEval s = liftM (\x -> eval x Map.empty) (parse s)

p0 = parseAndEval "1 + 3"
p1 = parseAndEval "(fun X -> fun Y -> X) 1 2"
p2 = parseAndEval "(fun X -> fun Y -> Y) 1 2"

t0 = (eval factExp) Map.empty

s =  "let F = fun X -> if X <= 1 then 1 else F (X - 1) + F (X - 2) in F"

fib5 = parseAndEval ("(" ++ s ++ ") " ++ "5")

yExp = "(fun F -> (fun X -> F (X X)) (fun X -> F (X X)))"
fExp = "(fun FACT -> fun X -> if X <= 0 then 1 else X * FACT (X - 1)))"
p3 = parseAndEval ("(" ++ yExp ++ " " ++ fExp ++ " 5)")

replE :: IO ()
replE = do
   putStr "%> "
   line <- getLine
   case parse line of
     Just exp -> do
         putStrLn $ show (eval exp Map.empty)
         replE
     Nothing -> putStrLn "what?!?" >> replE

type Reader a = (->) Environment a

instance {-# OVERLAPPING #-} Monad ((->) Environment) where
  -- return :: a -> Environment -> a
  return x = undefined
  -- (>>=) :: (Environment -> a) -> (a -> (Environment -> b))
  --                             -> (Environment -> b)
  m >>=  k = undefined

evalM :: Expression -> (Environment -> Value)
evalM (Var x)       = slookup x

evalM (IntExp i)    = return (IntVal i)

evalM (BoolExp i)   = return (BoolVal i)

evalM (Op o e1 e2)  = undefined

evalM (If e1 e2 e3) = undefined

evalM (Fun x e)     = undefined

evalM (App fun arg) = undefined

evalM (Let x e1 e2) = undefined

