module Parsers where
import Data.Char
import Text.Read (readMaybe)
import Data.Maybe (maybeToList,isJust)
import Control.Applicative

newtype Parser a = P { doParse :: String -> [(a, String)] }

get :: Parser Char
get = P $ \s -> case s of
                  (c1 : cs) -> [ (c1, cs) ]
                  []        -> []

oneDigit :: Parser Int
oneDigit = P $ \s -> case s of
                       (c1 : cs) -> case (readMaybe [c1] :: Maybe Int) of
                                      Just i  -> [ (i, cs) ]
                                      Nothing -> []
                       [] -> []

oneOp :: Parser (Int -> Int)
oneOp = P $ \s -> case s of
                   ('+' : cs) -> [ (id, cs) ]
                   ('-' : cs) -> [ (negate, cs) ]
                   _          -> []

{-
oneX :: (Char -> Maybe b) -> Parser b
oneX f = P $ \s -> case s of
                   (c1 : cs) -> case f c1 of
                                 Just b -> [(b,cs)]
                                 Nothing -> []
                   []        -> []
-}

oneX :: (Char -> Maybe b) -> Parser b
oneX f = P $ \s -> do (c, cs) <- doParse get s
                      case f c of
                         Just x -> return (x,cs)
                         Nothing -> []

fmapMaybe :: (a -> Maybe b) -> Parser a -> Parser b
fmapMaybe f p = P $ \s -> do (c, cs) <- doParse p s
                             case f c of
                               Just x -> return (x,cs)
                               Nothing -> []

instance Functor Parser where
    --fmap :: (a -> b) -> Parser a -> Parser b
    {- fmap f p = P $ \s -> do (c, cs) <- doParse p s
                            return (f c, cs) -}

    fmap f p = fmapMaybe (\x -> Just (f x)) p

ensure :: (a -> Bool) -> Parser a -> Parser a
{-
ensure f p = P $ \s -> do (a, cs) <- doParse p s
                          if (f a) then return (a, cs) else []
-}

ensure f p = P $ \s -> [ (a,cs) | (a, cs) <- doParse p s, f a ]

satisfy ::  (Char -> Bool) -> Parser Char
satisfy f = ensure f get

alphaChar, digitChar :: Parser Char
alphaChar = satisfy isAlpha
digitChar = satisfy isDigit

oneDigit' :: Parser Int
oneDigit' = cvt <$> digitChar where    -- fmap!
  cvt c = ord c - ord '0'

char :: Char -> Parser Char
char c = satisfy (== c)

twoChar0 :: Parser (Char, Char)
twoChar0 = P $ \s -> do (c1, cs) <- doParse get s
                        (c2, cs') <- doParse get cs
                        return ((c1,c2), cs')

pairP0 ::  Parser a -> Parser b -> Parser (a,b)
pairP0 p1 p2 =  P $ \s -> do (c1, cs) <- doParse p1 s
                             (c2, cs') <- doParse p2 cs
                             return ((c1,c2), cs')

twoChar1 = pairP0 get get

zipWithP :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
zipWithP f p1 p2 = P $ \s -> do (c1, cs) <- doParse p1 s
                                (c2, cs') <- doParse p2 cs
                                return (f c1 c2, cs')

signedDigit0 :: Parser Int
signedDigit0 = P $ \ s -> do (f, s') <- doParse oneOp s
                             (x,s'') <- doParse oneDigit s'
                             return (f x, s'')

apP p1 p2 = P $ \ s -> do (f, s') <- doParse p1 s
                          (x,s'') <- doParse p2 s'
                          return (f x, s'')

pureP :: a -> Parser a
pureP x = P $ \s -> [(x,s)]

instance Applicative Parser where
  pure   = pureP
  (<*>)  = apP

twoChar :: Parser (Char, Char)
twoChar = p <*> get <*> get where
   p :: Parser (Char -> Char -> (Char,Char))
   p = pure (,)  --- (,) is the same as \x -> \y -> (x,y)
signedDigit = oneOp <*> oneDigit

pairP :: Parser a -> Parser b -> Parser (a,b)
pairP p1 p2 = pure (,) <*> p1 <*> p2

pairP' :: Parser a -> Parser b -> Parser (a,b)
pairP' = liftA2 (,)

string :: String -> Parser String
string ""     = pure ""
string (x:xs) = liftA2 (:) (char x) (string xs)

chooseP :: Parser a -> Parser a -> Parser a

p1 `chooseP` p2 = P $ \s ->  let
                               res1 = doParse p1 s
                               res2 = doParse p2 s
                             in res1 ++ res2

alphaNumChar = alphaChar `chooseP` digitChar

grabn :: Int -> Parser String
grabn 0 = pure ""
grabn n = liftA2 (:) get (grabn (n-1))

grab2or4 = grabn 2 `chooseP` grabn 4

intOp = plus `chooseP` minus `chooseP` times `chooseP` divide
  where plus   = char '+' *> pure (+)
        minus  = char '-' *> pure (-)
        times  = char '*' *> pure (*)
        divide = char '/' *> pure div

calc :: Parser Int
calc = oneDigit `chooseP` liftA3 (\i1 o i2 -> o i1 i2) oneDigit intOp calc

manyP :: Parser a -> Parser [a]
manyP p = liftA2 (:) p (manyP p) `chooseP` pure []

chooseFirstP :: Parser a -> Parser a -> Parser a
chooseFirstP p1 p2 = P $ \s -> take 1 (doParse (p1 `chooseP` p2) s)

instance Alternative Parser where
   empty = failP
   (<|>) = chooseFirstP

failP :: Parser a
failP = P $ \ s -> []

oneNat :: Parser Int
oneNat = fmapMaybe readMaybe (many digitChar)

calc1 ::  Parser Int
calc1 =  liftA3 (\i1 o i2 -> o i1 i2) oneNat intOp calc1 <|> oneNat

addOp :: Parser (Int -> Int -> Int)
addOp = char '+' *> pure (+) <|> char '-' *> pure (-)
 
mulOp :: Parser (Int -> Int -> Int)
mulOp = char '*' *> pure (*) <|> char '/' *> pure div

sumE :: Parser Int
sumE = liftA3 (\i1 o i2 -> o i1 i2) prodE addOp sumE <|> prodE

prodE :: Parser Int
prodE = liftA3 (\i1 o i2 -> o i1 i2) factorE mulOp prodE <|> factorE

factorE :: Parser Int
factorE = oneNat <|> ( char '(' *> sumE <* char ')')

sumE1 :: Parser Int
sumE1 = foldl comb <$> prodE1 <*> rest where
           comb = \ x (op,y) -> x `op` y
           rest = many ((,) <$> addOp <*> prodE1)

prodE1 :: Parser Int
prodE1 = foldl comb <$> factorE1 <*> rest where
           comb = \ x (op,y) -> x `op` y
           rest = many ((,) <$> mulOp <*> factorE1)

factorE1 :: Parser Int
factorE1 = oneNat <|>  ( char '(' *> sumE <* char ')')

chainl1 :: Parser b -> Parser (b -> b -> b) -> Parser b
p `chainl1` pop = foldl comb <$> p <*> rest where
           comb = \ x (op,y) -> x `op` y
           rest = many ((,) <$> pop <*> p)

parenP :: Char -> Parser b -> Char -> Parser b
parenP l p r = char l *> p <* char r

sumE2    = prodE2   `chainl1` addOp
prodE2   = factorE2 `chainl1` mulOp
factorE2 = parenP '(' sumE2 ')' <|> oneNat

