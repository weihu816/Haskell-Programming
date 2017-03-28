{-# LANGUAGE InstanceSigs #-}

module State (State,  get, put, modify, state, runState, evalState, execState) where
import Control.Monad (ap,liftM)

newtype State s a = S { runState :: s -> (a, s) }

state :: (s -> (a,s)) -> State s a
state = S

instance Monad (State s) where
  return :: a -> State s a
  return x   =  S $ \s -> (x,s)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f   = S $ \s -> let (a,s') = runState st s in
                             runState (f a) s'

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

evalState :: State s a -> s -> a
evalState st s = fst (runState st s)

execState :: State s a -> s -> s
execState st = snd . (runState st)

get :: State s s
get = S $ \s -> ( s, s)

put :: s -> State s ()
put s' = S $ \_ ->  ( ()  , s' )

modify :: (s -> s) -> State s ()
modify f = do
     s <- get
     put (f s)

