{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
             FlexibleInstances, KindSignatures, InstanceSigs,
             MultiParamTypeClasses, ScopedTypeVariables #-}
 
module Transformers where
 
import Control.Monad (liftM,ap)
 
import State (State)              -- State monad developed in lecture
import qualified State as S

data Expr = Val Int
          | Div Expr Expr
          deriving (Show)

eval            ::  Expr -> Int
eval (Val n)   =  n
eval (Div x y) =  eval x `div` eval y

ok  = (Val 1972 `Div` Val 2)
      `Div` Val 23
err = Val 2 `Div`
      (Val 1 `Div`
       (Val 2 `Div` Val 3))

evalDefault            ::  Expr -> Int
evalDefault (Val n)   =  n
evalDefault (Div x y) =
  let m = evalDefault y in
  if m == 0 then 0 else evalDefault x `div` m

evalMaybe ::  Expr -> Maybe Int
evalMaybe (Val n)   = return n
evalMaybe (Div x y) = do
   vx <- evalMaybe x
   vy <- evalMaybe y
   if vy == 0 then Nothing else return (vx `div` vy)

--evalEither ::  Expr -> Either String Int
evalEither (Val n)   = return n
evalEither (Div x y) = do
   vx <- evalEither x
   vy <- evalEither y
   if vy == 0 then throwError (errorS x y) else return (vx `div` vy)

errorS y m = "Error dividing " ++ show y ++ " by " ++ show m

type Prof = State Int

tickProf :: Prof ()
tickProf = do
  x <- S.get      -- use get and put from the state monad
  S.put (x + 1)

-- evalProf           :: Expr -> Prof Int
evalProf (Val n)   = return n
evalProf (Div x y) = do
  m <- evalProf x
  n <- evalProf y
  tickStateInt
  return (m `div` n)

goProf :: Expr -> IO ()
goProf e = putStrLn $ "value: " ++ show x ++ ", count: " ++ show s
           where (x,s) = S.runState (evalProf e) 0 :: (Int, Int)

class Monad m => MonadError e m where
  throwError :: e -> m a

instance MonadError s (Either s) where
  throwError :: s -> Either s a
  throwError = Left

class Monad m => MonadState s m where
  get      :: m s           -- State s s
  put      :: s -> m ()

tickStateInt :: MonadState Int m => m ()
tickStateInt = do
    (x :: Int) <- get
    put (x + 1)

instance MonadState s (State s) where
  get        = S.get
  put        = S.put

-- evalMega :: (MonadState Int m, MonadError String m) => Expr -> m Int
-- evalMega :: Expr -> Mega Int
evalMega (Val n)   = return n
evalMega (Div x y) = do
  n <- evalMega x
  m <- evalMega y
  if (m == 0)
    then throwError $ errorS n m
    else do
      tickStateInt
      return (n `div` m)

data Mega a = Mega { runMega :: Int -> Either String (a, Int) }
 
instance Monad Mega where
  return :: a -> Mega a
  return x   = Mega $ \s -> Right (x,s)
  (>>=) :: Mega a -> (a -> Mega b) -> Mega b
  ma >>= fmb = Mega $ \s -> case runMega ma s of
                               Left str -> Left str
                               Right (a, s') -> runMega (fmb a) s'
 
instance Applicative Mega where
  pure = return
  (<*>) = ap
instance Functor Mega where
  fmap = liftM
 
instance MonadError String Mega where
  throwError :: String -> Mega a
  throwError str = Mega $ \s -> Left str
instance MonadState Int Mega where
  get   = Mega $ \s -> Right (s,s)
  put x = Mega $ \_ -> Right ((), x)

data ExceptT e m a = MkExc { runExceptT :: m (Either e a) }

instance Monad m => Monad (ExceptT e m) where
 
  return :: forall a. a -> ExceptT e m a
  return x = MkExc (return (Right x) :: m (Either e a))
 
  (>>=) :: ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
  p >>= f  = MkExc $ runExceptT p >>= (\ x -> case x of
                                          Left e  -> return (Left e)
                                          Right a -> runExceptT (f a))

instance Monad m => Applicative (ExceptT e m) where
   pure = return
   (<*>) = ap
instance Monad m => Functor (ExceptT e m) where
   fmap = liftM

instance Monad m => MonadError e (ExceptT e m) where
  throwError :: e -> ExceptT e m a
  throwError msg = MkExc (return (Left msg))

newtype StateT s m a =  MkStateT { runStateT :: s -> m (a, s) }

instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return x = MkStateT $ \s -> return (x,s)
 
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  p >>= f = MkStateT $ \s -> do (r,s') <- runStateT p s
                                runStateT (f r) s'

instance Monad m => Applicative (StateT s m) where
  pure  = return
  (<*>) = ap

instance Monad m => Functor (StateT s m) where
  fmap  = liftM

instance Monad m => MonadState s (StateT s m) where

  get :: StateT s m s
  get = MkStateT getIt
    where getIt :: s -> m (s, s)
          getIt s = return (s, s)

  put :: s -> StateT s m ()
  put s = MkStateT putIt
    where putIt :: s -> m ((), s)
          putIt _ = return ((), s)

class MonadTrans (t :: (* -> *) -> * -> *) where   -- from Control.Monad.Trans (among other places)
  lift :: Monad m => m a -> t m a

instance MonadTrans (ExceptT e) where
  lift :: Monad m => m a -> ExceptT e m a
  -- MkExc :: m (Either e a) -> ExceptT e m a
  lift = MkExc . lift_ where
    lift_  :: (Monad m) => m a -> m (Either e a)
    lift_ mt = do x <- mt
                  return (Right x)

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  -- MkStateT  :: (s -> m (a,s)) -> StateT s m a
  lift ma = MkStateT $ \s -> do r <- ma
                                return (r,s)

instance MonadError e m => MonadError e (StateT s m) where
  throwError :: e -> StateT s m a
  throwError = lift . throwError

instance MonadState s m => MonadState s (ExceptT e m) where
 
  get :: ExceptT e m s
  get = lift get
 
  put :: s -> ExceptT e m ()
  put = lift . put
 

evalM :: Expr -> Mega Int
evalM = evalMega

evalExSt :: Expr -> StateT Int (Either String) Int
evalExSt = evalMega
 
evalStEx :: Expr -> ExceptT String Prof Int
evalStEx = evalMega

goExSt :: Expr -> IO ()
goExSt e = putStr $ pr (evalExSt e) where
    pr :: StateT Int (Either String) Int -> String
    pr f = case runStateT f 0 of
                     Left s         -> "Raise: " ++ s ++ "\n"
                     Right (v, cnt) -> "Count: " ++ show cnt ++ "\n" ++
                                        "Result: " ++ show v ++ "\n"

goStEx :: Expr -> IO ()
goStEx e = putStr $ pr (evalStEx e) where
   pr :: ExceptT String Prof Int -> String
   pr f = "Count: " ++ show cnt ++ "\n" ++ show r ++ "\n"
     where (r, cnt) = S.runState (runExceptT f) 0

data Id a = MkId a deriving Show

instance Monad Id where
  return x = MkId x
  (MkId p) >>= f  = (f p)

instance Applicative Id where
  pure  = return
  (<*>) = ap
instance Functor Id where
  fmap  = liftM

type State2 s  = StateT  s Id   -- isomorphic to State s
type Either2 s = ExceptT s Id   -- isomorphic to Either s

