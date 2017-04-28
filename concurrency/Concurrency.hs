{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches
    -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs,
    UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}

module Concurrency where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import System.IO
import Data.IORef

import Network (withSocketsDo, PortNumber, PortID(..), listenOn, accept)

--------------------------------------------------------------------

data Action =
       Atom (IO Action)      -- an atomic computation, returning a new action
     | Fork Action Action    -- create a new thread
     | Stop                  -- terminate this thread

writeAction :: String -> Action
writeAction "" = Stop
writeAction (c:cs) = Atom $ do
   putChar c
   return $ writeAction cs

writeAction' str = Atom $ do
   putStr str
   return Stop

prog :: Action
prog = Fork (writeAction "Hello\n") (writeAction "CIS 552\n")

sched :: [Action] -> IO ()
sched [] = return ()
sched (Atom io : cs)    = do
   a <- io
   sched (cs ++ [a])
sched (Fork a1 a2 : cs) = sched (cs ++ [a2,a1])
sched (Stop : cs)       = sched cs

prog2 :: Action
prog2 = Fork (writeAction' "Hello\n") (writeAction "CIS 552\n")

sequenceAction :: Action -> Action -> Action
sequenceAction a1 a2 = undefined

hello552 :: Action
hello552 = writeAction "Hello" `sequenceAction` writeAction "CIS 552"

writeComputation :: String -> Action -> Action
writeComputation "" k = k
writeComputation (c:cs) k = Atom $ do
   putChar c
   return $ writeComputation cs k

prog3 = writeComputation "Hello" (writeComputation " CIS 552" Stop)

sequenceComputation :: (Action -> Action)
                    -> (Action -> Action)
                    -> (Action -> Action)
sequenceComputation =  (.)

hello552Computation = writeComputation "Hello"   `sequenceComputation`
                      writeComputation "CIS 552"

readComputation :: (Char -> Action) -> Action
readComputation k = Atom $ do
   c <- getChar
   return $ k c

sequenceComp :: (CM a)            -- last action takes an arg.
              -> (a -> (CM b))    -- pass to another
              -> (CM b)
sequenceComp m f = \k -> m (\a -> f a k)

type CM a = (a -> Action) -> Action

sequenceCompM :: CM a -> (a -> CM b) -> CM b
sequenceCompM m f = \k -> m (\v -> f v k)

returnCompM :: a -> ((a -> Action) -> Action)
returnCompM x = \k -> k x

newtype C a = C { runC :: (a -> Action) -> Action }

instance Monad C  where
  (>>=) :: C a -> (a -> C b) -> C b
  m >>= f  = C $ \k -> runC m (\v -> runC (f v) k)
 
  return :: a -> C a
  return x = C $ \k -> k x

instance Applicative C where
    pure  = return
    (<*>) = ap

instance Functor C where
    fmap = liftM

atom :: IO a -> C a
atom m = C $ \k -> Atom $ do
                     r <- m
                     return $ k r

action :: C a -> Action
action m = runC m (\ _ -> Stop)

fork :: C () -> C ()
fork m = C $ \k -> Fork (action m) (k ())

run :: C a -> IO ()
run m = sched  [ action m ]

class Monad m => Output m where
   write :: String -> m ()

instance Output IO where
   write = putStr

loop :: Output m => String -> m ()
loop s = write s >> loop s
{-
loop s = go s where
  go "" = loop s
  go (c:cs) = write [c] >> go cs
-}

instance Output C where
--    write s = atom (write s)
   write []     = return ()
   write (c:cs) = atom (write [c]) >> write cs

example :: C ()
example = do write "It's raining..."
             fork (loop "dog\n")
             fork (loop "cat\n")

-- instance Output C where
--    write []     = atom (write [])
--    write (x:xs) = atom (write [x]) >> write xs

class Monad m => Input m where
   input :: m (Maybe String)

instance Input IO where
   input = do x <- hReady stdin
              if x then Just <$> getLine else return Nothing

ioloop :: (Input m, Output m) => String -> m String
ioloop s = do i <- input
              case i of
                Just x  -> return $ "Thread " ++ s ++ ":" ++ x
                Nothing -> do write s
                              ioloop s

instance Input C where
   input = atom input

example2 :: C ()
example2 = do
           fork $ ioloop "a" >>= write
           ioloop "b" >>= write

type MVar a = IORef (Maybe a)

class Monad m => MVarMonad m where
  newMVar   :: m (MVar a)
  writeMVar :: MVar a -> a -> m ()
  takeMVar  :: MVar a -> m (Maybe a)

instance MVarMonad IO where
  newMVar       = newIORef Nothing
  writeMVar v a = writeIORef v (Just a)
  takeMVar  v   = do x <- readIORef v
                     case x of
                       Just y -> writeIORef v Nothing >> return (Just y)
                       Nothing -> return Nothing

instance MVarMonad C where
  newMVar       = atom newMVar
  writeMVar v a = atom (writeMVar v a)
  takeMVar  v   = atom (takeMVar v)

readMVar :: (MVarMonad m) => MVar a -> m a
readMVar v = do mv <- takeMVar v
                case mv of
                   Just a  -> return a
                   Nothing -> readMVar v

data Msg =
   Add | Reset | Print | Quit

simulation :: MVar Msg -> Integer -> C ()
simulation mv i = do
  x <- takeMVar mv
  case x of
    Just Add   -> do write "Adding...\n"
                     simulation mv (i+1)
    Just Reset -> do write "Resetting...\n"
                     simulation mv 0
    Just Print -> do write ("Current value is " ++ show i ++ "\n")
                     simulation mv i
    Just Quit  -> do write ("Done\n")
    Nothing    -> simulation mv i

interface :: MVar Msg -> C (Maybe String) -> C ()
interface mv getInput = loop where
   loop = do
     maybeKey <- getInput
     case maybeKey of
       Just "a" -> writeMVar mv Add   >> loop
       Just "r" -> writeMVar mv Reset >> loop
       Just "p" -> writeMVar mv Print >> loop
       Just "q" -> writeMVar mv Quit
       Just s   -> write ("Unknown command: " ++ s ++ "\n") >> loop
       Nothing  -> loop

example6 = do
   mv <- newMVar
   fork $ simulation mv 0
   interface mv input

network :: PortNumber -> MVar Msg -> C ()
network port mv = do
  handle <- atom $ withSocketsDo $ do
              socket <- listenOn (PortNumber port)
              (handle, host, port) <- accept socket
              (hSetBuffering handle NoBuffering)
              return handle
  let network_input = do x <- hReady handle
                         if x then liftM Just (hGetLine handle)
                         else return Nothing
  interface mv (atom network_input)
  atom (hClose handle)

example7 :: PortNumber -> C ()
example7 port = do
   mv <- newMVar
   fork (simulation mv 0)
   fork (interface mv input)
   network port mv

