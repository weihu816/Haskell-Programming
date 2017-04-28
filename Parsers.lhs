Parsing with Applicative Functors
=================================

> module Parsers where
> import Data.Char
> import Text.Read (readMaybe)
> import Data.Maybe (maybeToList,isJust)
> import Control.Applicative

Announcements (3/23)
=============

* HW #7 extension -> now due next Thursday at midnight

* Download 'Parsers' module from website

* EC opportunity for HW7! omarsa@seas.upenn.edu

Announcements (3/28)
=============

* Weirich OH
   - today after class, here
   - also moved next week, also immediately after class

* Talk Monday
   - Effective ML
   - Yaron Minsky, CTO Jane Street

* Project
   - should have received feedback by email
   - three check-ins with your mentors, schedule now
   - due Monday, April 24th
   - use your githup repo

What is a Parser?
-----------------

"A parser for things
Is a function from strings
To lists of pairs
Of things and strings."

-- Graham Hutton


A parser is a piece of software that takes a raw `String` (or sequence
of bytes) and returns some structured object -- for example, a list of
options, an XML tree or JSON object, a program's Abstract Syntax Tree,
and so on.  Parsing is one of the most basic computational tasks.  For
example:

- Shell Scripts (command-line options)
- Web Browsers (duh!)
- Games (level descriptors)
- Routers (packets)
- etc.

(Indeed I defy you to find any serious system that does *not* do some
parsing somewhere!)

The simplest way to think of a parser is as a function -- i.e., its
type should be roughly this:

~~~~~{.haskell}
    type Parser = String -> StructuredObject
~~~~~


Composing Parsers
-----------------

The usual way to build a parser is by specifying a grammar and using a
parser generator (e.g., yacc, bison, antlr) to create the actual
parsing function. Despite its advantages, one major limitation of the
grammar-based approach is its lack of modularity. For example, suppose
we have two kinds of primitive values, `Thingy` and `Whatsit`.

       Thingy : ...rule...   { ...action... } ;

       Whatsit : ...rule...  { ...action... } ;

If we want a parser for *sequences of* `Thingy` and `Whatsit` we have
to painstakingly duplicate the rules:

      Thingies : Thingy Thingies  { ... }
                 EmptyThingy      { ... } ;
 
      Whatsits : Whatsit Whatsits { ... }
                 EmptyWhatsit     { ... } ;

That is, the languages in which parsers are usually described are
lacking in features for modularity and reuse.

In this lecture, we will see how to *compose* mini-parsers for
sub-values to get bigger parsers for complex values.

To do so, we will generalize the above parser type a little bit, by
noting that a (sub-)parser need not (indeed, in general will not)
consume *all* of its input, in which case we need to have the parser
return the unconsumed part of its input:

~~~~~{.haskell}
    type Parser = String -> (StructuredObject, String)
~~~~~








Of course, it would be silly to have different types for parsers for
different kinds of structured objects, so we parameterize the `Parser`
type over the type of structured object that it returns:

~~~~~{.haskell}
    type Parser a = String -> (a, String)
~~~~~





One last generalization is to allow a parser to return a *list* of
possible parse results, where the empty list corresponds to a failure
to parse:

~~~~~{.haskell}
    type Parser a = String -> [(a, String)]
~~~~~






As the last step, let's wrap this type definition up as a `newtype` and
define a record component to let us conveniently extract the parser:

> newtype Parser a = P { doParse :: String -> [(a, String)] }

~~~~~~{.haskell}
      ghci> :t doParse
      doParse :: Parser a -> String -> [(a,String)]
~~~~~~


This will make sure that we keep parsers distinct from other values of
this type and, more importantly, will allow us to make parsers an
instance of one or more typeclasses, if this turns out to be
convenient (see below!).

Below, we will define a number of operators on the `Parser` type, which will
allow us to build up descriptions of parsers compositionally.  The actual
parsing happens when we use a parser by applying it to an input string, using
`doParse`.

Now, the parser type might remind you of something else... Remember this?

~~~~~{.haskell}
    newtype State s a = S { runState :: s -> (a, s) }
~~~~~

Indeed, a parser, like a state transformer, [is a monad][2]! There are good
definitions of the `return` and `>>=` functions.

However, most of the time, don't need the full monadic structure for parsing,
just deriving the applicative operators for this type will allow us to parse
any context-free grammar. So in today's lecture, keep your eye out for
applicative structure for this type.

Now all we have to do is build some parsers!
 

Parsing a Single Character
--------------------------

Here's a *very* simple character parser that returns the first `Char`
from a (nonempty) string.

-- newtype Parser a = P { doParse :: String -> [(a, String)] }

> get :: Parser Char
> get = P $ \s -> case s of
>                   (c1 : cs) -> [ (c1, cs) ]
>                   []        -> []
 
~~~~~{.haskell}
    ghci> doParse get "hey!"
    ghci> doParse get ""
~~~~~

And here's a parser that looks at the first char of a (nonempty) string
and interprets it as an int.
 
> oneDigit :: Parser Int
> oneDigit = P $ \s -> case s of
>                        (c1 : cs) -> case (readMaybe [c1] :: Maybe Int) of
>                                       Just i  -> [ (i, cs) ]
>                                       Nothing -> []
>                        [] -> []
 
And here's a parser that looks at the first char of a (nonempty) string
and interprets it as a unary operator.

    ghci> doParse oneDigit "1"
    ghci> doParse oneDigit "12"
 
> oneOp :: Parser (Int -> Int)
> oneOp = P $ \s -> case s of
>                    ('+' : cs) -> [ (id, cs) ]
>                    ('-' : cs) -> [ (negate, cs) ]
>                    _          -> []


    ghci> fst (head (doParse oneOp "-")) 3
    ghci> fst (head (doParse oneOp "+")) 3
 
Can we generalize this pattern?

> {-
> oneX :: (Char -> Maybe b) -> Parser b
> oneX f = P $ \s -> case s of
>                    (c1 : cs) -> case f c1 of
>                                  Just b -> [(b,cs)]
>                                  Nothing -> []
>                    []        -> []
> -}

> oneX :: (Char -> Maybe b) -> Parser b
> oneX f = P $ \s -> do (c, cs) <- doParse get s
>                       case f c of
>                          Just x -> return (x,cs)
>                          Nothing -> []



And generalize again, so that it works for any parser, not just get...
 
> fmapMaybe :: (a -> Maybe b) -> Parser a -> Parser b
> fmapMaybe f p = P $ \s -> do (c, cs) <- doParse p s
>                              case f c of
>                                Just x -> return (x,cs)
>                                Nothing -> []

(Aside: the name is derived from the `mapMaybe` function found in `Data.Maybe`.)


Parser is a Functor
===================

Take a look at this!
 
> instance Functor Parser where
>     --fmap :: (a -> b) -> Parser a -> Parser b
>     {- fmap f p = P $ \s -> do (c, cs) <- doParse p s
>                             return (f c, cs) -}

>     fmap f p = fmapMaybe (\x -> Just (f x)) p


We now have a library of *Parser functions* that we can use to
build new parsers.
 
Let's look at other ways to define `oneDigit` for example.

First, note, that we can write a "filter" function for parsers.

> ensure :: (a -> Bool) -> Parser a -> Parser a
> {-
> ensure f p = P $ \s -> do (a, cs) <- doParse p s
>                           if (f a) then return (a, cs) else []
> -}

> ensure f p = P $ \s -> [ (a,cs) | (a, cs) <- doParse p s, f a ]




This may seem a little silly, but it's helpful for building up richer
parsers like the following, which parses a `Char` *if* it satisfies a
predicate `p`:
 
> satisfy ::  (Char -> Bool) -> Parser Char
> satisfy f = ensure f get

Note that we are working abstractly here: we can define `ensure` and `satisfy`
without using the `P` data constructor or `doParse`.

With this, we can write some simple parsers for particular characters.
The following definitions parse alphabetic and numeric characters
respectively (`isAlpha` and `isDigit` come from the standard Prelude).

> alphaChar, digitChar :: Parser Char
> alphaChar = satisfy isAlpha
> digitChar = satisfy isDigit

~~~~~~~~~~~~~~~~~~{.haskell}
    ghci> doParse alphaChar "123"
    ghci> doParse digitChar "123"
~~~~~~~~~~~~~~~~~~~

And now we can rewrite oneDigit (yet again!)

> oneDigit' :: Parser Int
> oneDigit' = cvt <$> digitChar where    -- fmap!
>   cvt c = ord c - ord '0'


~~~~~{.haskell}
    ghci> doParse oneDigit' "92"
    ghci> doParse oneDigit' "cat"
~~~~~

Finally, this parser will parse just one specific `Char`:

> char :: Char -> Parser Char
> char c = satisfy (== c)

~~~~~~~~~~~{.haskell}
    ghci> doParse (char 'a') "ab"
    ghci> doParse (char 'a') "ba"
~~~~~~~~~~~~~~~~~~~~~





Parser Composition
==================

What if we want to combine our parsers together?

Using `get` we can write a composite parser that returns a pair of
the first two `Char` values from the front of the input string:


> twoChar0 :: Parser (Char, Char)
> twoChar0 = P $ \s -> do (c1, cs) <- doParse get s
>                         (c2, cs') <- doParse get cs
>                         return ((c1,c2), cs')

     ghci> doParse twoChar0 "ab"



More generally, we can write a *parser combinator* that takes two
parsers and returns a new parser that uses first one and then the
other and returns the pair of resulting values...

> pairP0 ::  Parser a -> Parser b -> Parser (a,b)
> pairP0 p1 p2 =  P $ \s -> do (c1, cs) <- doParse p1 s
>                              (c2, cs') <- doParse p2 cs
>                              return ((c1,c2), cs')


and use that to rewrite 'twoChar' more elegantly like this:

> twoChar1 = pairP0 get get

~~~~~{.haskell}
    ghci> doParse twoChar1 "hey!"
    ghci> doParse twoChar1 ""
~~~~~

We can generalize pairP0 even more.  Maybe we don't want to
just throw the two results in a pair. Let's pass in a function
that determines what to do.

> zipWithP :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
> zipWithP f p1 p2 = P $ \s -> do (c1, cs) <- doParse p1 s
>                                 (c2, cs') <- doParse p2 cs
>                                 return (f c1 c2, cs')

Everytime, I see `zipWith`, I always think of `Applicative` functors.  The
reason is that we can define zipWith using `pure` and `<*>` !

   zipWith f x y = pure f <*> x <*> y

So our next goal is to derive somehow `pure` and `<*>` for the `Parser` type,
so that even zipWith can be defined via combinators.


Parser is Applicative
=====================

For example, suppose we want to parse *two* characters, where the first should
be a sign and the second a digit?

-- oneDigit :: Parser Int
-- oneOp    :: Parser (Int -> Int)

> signedDigit0 :: Parser Int
> signedDigit0 = P $ \ s -> do (f, s') <- doParse oneOp s
>                              (x,s'') <- doParse oneDigit s'
>                              return (f x, s'')

~~~~~{.haskell}
     ghci> doParse signedDigit0 "-1"
     ghci> doParse signedDigit0 "+3"
~~~~~



Can we generalize this pattern? What is the type when `oneOp` and `oneDigit`
are arguments to the combinator?

> apP p1 p2 = P $ \ s -> do (f, s') <- doParse p1 s
>                           (x,s'') <- doParse p2 s'
>                           return (f x, s'')


What does this combinator do?  It just grabs all function values out of the
first parser and then grabs all of the arguments (using the remaining part of
the string) from the second parser, and then return all of the applications.

What about pure?

The definition of `pure` is very simple -- we can let the types
guide us.  We ignore the input string (keeping the whole thing as the
"remainder after parsing") and just return the given value.

> pureP :: a -> Parser a
> pureP x = P $ \s -> [(x,s)]


> instance Applicative Parser where
>   pure   = pureP
>   (<*>)  = apP

Let's go back and reimplement our examples with the more general combinators:

> twoChar :: Parser (Char, Char)
> twoChar = p <*> get <*> get where
>    p :: Parser (Char -> Char -> (Char,Char))
>    p = pure (,)  --- (,) is the same as \x -> \y -> (x,y)
> signedDigit = oneOp <*> oneDigit

~~~~~{.haskell}
    ghci> doParse twoChar "hey!"
    ghci> doParse twoChar ""
    ghci> doParse signedDigit "-1"
    ghci> doParse signedDigit "+3"
~~~~~~

Parser Combinators
==================

Now we're picking up speed.  First, we can use our combinators to rewrite
`pairP` like this:

> pairP :: Parser a -> Parser b -> Parser (a,b)
> pairP p1 p2 = pure (,) <*> p1 <*> p2


We can even dip into the `Control.Applicative` library and write `pairP` even
more succinctly:

-- liftA2 f p1 p2 = pure f <*> p1 <*> p2

> pairP' :: Parser a -> Parser b -> Parser (a,b)
> pairP' = liftA2 (,)


Recursive Parsing
-----------------

To parse more interesting things, we need to add some kind of
recursion to our combinators. For example, it's all very well to parse
individual characters (as in `char` above), but it would a lot more
fun if we could recognize whole `String`s.

Let's try to write it!

> string :: String -> Parser String
> string ""     = pure ""
> string (x:xs) = liftA2 (:) (char x) (string xs)


Much better!

~~~~~{.haskell}
    ghci> doParse (string "mic") "mickeyMouse"
    ghci> doParse (string "mic") "donald duck"
~~~~~



Nondeterministic Choice
-----------------------


Next, let's write a combinator that takes two sub-parsers and
nondeterministically chooses between them.

> chooseP :: Parser a -> Parser a -> Parser a

How to write it?  Well, we want to return a succesful parse if
*either* parser succeeds. Since our parsers return multiple values, we
can simply return the *union* of all the results!

> p1 `chooseP` p2 = P $ \s ->  let
>                                res1 = doParse p1 s
>                                res2 = doParse p2 s
>                              in res1 ++ res2

We can use the above combinator to build a parser that
returns either an alphabet or a numeric character

> alphaNumChar = alphaChar `chooseP` digitChar

~~~~~{.haskell}
    ghci> doParse alphaNumChar "cat"
    ghci> doParse alphaNumChar "2cat"
    ghci> doParse alphaNumChar "2at"
~~~~~

If *both* parsers succeed, we get back all the results. For example,
here's a parser that grabs `n` characters from the input:

> grabn :: Int -> Parser String
> grabn 0 = pure ""
> grabn n = liftA2 (:) get (grabn (n-1))

(Challenge: can you remove the explicit recursion from that?)

Now, we can use our choice combinator

> grab2or4 = grabn 2 `chooseP` grabn 4

and now will get back both results if both parses are possible,

~~~~~{.haskell}
    ghci> doParse grab2or4 "mickeymouse"
~~~~~

and only one if the other is not possible:

~~~~~{.haskell}
    ghci> doParse grab2or4 "mic"
    ghci> doParse grab2or4 "m"
~~~~~


Even with just these rudimentary parsers we have at our disposal, we
can start doing some interesting things. For example, here is a little
calculator. First, we parse arithmetic operations as follows:

> intOp = plus `chooseP` minus `chooseP` times `chooseP` divide
>   where plus   = char '+' *> pure (+)
>         minus  = char '-' *> pure (-)
>         times  = char '*' *> pure (*)
>         divide = char '/' *> pure div

(Can you guess the type of the above parser?)  Then we parse
simple expressions

> calc :: Parser Int
> calc = oneDigit `chooseP` liftA3 (\i1 o i2 -> o i1 i2) oneDigit intOp calc

which, when run, will perform both parsing and calculation.

~~~~~{.haskell}
    ghci> doParse calc "8/2"
    ghci> doParse calc "8+2cat"
~~~~~





Parsing With a List of Sub-Parsers
----------------------------------

Let's write a combinator that takes a parser `p` that returns an `a`
and constructs a parser that recognizes a sequence of strings (each
recognized by `p`) and returns a *list* of `a` values. That is, it
keeps grabbing `a` values as long as it can and returns them as `[a]`.

We can do this by writing a parser that either succeeds without consuming any
input or parses one thing (if possible) and then calls itself recursively.

> manyP :: Parser a -> Parser [a]
> manyP p = liftA2 (:) p (manyP p) `chooseP` pure []





Beware: This parser can yield a lot of results!

~~~~~{.haskell}
    ghci> doParse (manyP oneDigit) "12345a"
~~~~~

What happens if we swap the order of the `chooseP`?

Deterministic Choice
--------------------

Often, what we want is a single maximal result, not a long list of
partial results.  To get this, we need a *deterministic* choice
combinator. This combinator returns no more than one result -- i.e.,
it runs the choice parser but discards extra results.

> chooseFirstP :: Parser a -> Parser a -> Parser a
> chooseFirstP p1 p2 = P $ \s -> take 1 (doParse (p1 `chooseP` p2) s)

We can use deterministic choice and failure together to make the `Parser` type an instance
of the `Alternative` type class from [Control.Applicative](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Applicative.html).

> instance Alternative Parser where
>    empty = failP
>    (<|>) = chooseFirstP

The `Alternative` class also requires us to have a *failure* parser that
always goes down in flames (i.e. one that always returns `[]`):

> failP :: Parser a
> failP = P $ \ s -> []


This instance automatically gives us definitions of the functions `many` and
`some`.  Their default definitions look something like this:

~~~~~~~~~~{.haskell}
    many :: Alternative f => f a -> f [a]
    many v = some v <|> pure []

    some :: Alternative f => f a -> f [a]
    some v = liftA2 (:) v (many v)
~~~~~~~~~~

The `many` combinator returns a single, maximal sequence produced by iterating
the given parser.

~~~~~{.haskell}
    ghci> doParse (many digitChar) "12345a"
~~~~~


Let's use the above to write a parser that will return an entire
natural number (not just a single digit.)

> oneNat :: Parser Int
> oneNat = fmapMaybe readMaybe (many digitChar)

~~~~~{.haskell}
    ghci> doParse oneNat0 "12345a"
    ghci> doParse oneNat0 ""
~~~~~

Parsing XML
===========

For a short digression, let's see a quick example that puts together
what we know about parsers. In 15 mins, we'll build a parser for
simple XML expressions.

See [Xml.lhs](Xml.lhs)



Parsing Arithmetic Expressions
==============================

Now let's use the above to build a small calculator, that parses and
evaluates arithmetic expressions. In essence, an expression is either
a binary operand applied to two sub-expressions or else an integer. We
can state this as:

> calc1 ::  Parser Int
> calc1 =  liftA3 (\i1 o i2 -> o i1 i2) oneNat intOp calc1 <|> oneNat

This works pretty well...

~~~~~{.haskell}
    ghci> doParse calc1 "1+2+33"
    ghci> doParse calc1 "11+22-33"
~~~~~

But things get a bit strange with minus:

~~~~~{.haskell}
    ghci> doParse calc1 "11+22-33+45"
~~~~~

Huh?  Well, if you look back at the code, you'll realize the
above was parsed as

~~~~~{.haskell}
    11 + ( 22 - (33 + 45))
~~~~~

because in each `binExp` we require the left operand to be an
integer. In other words, we are assuming that each operator is *right
associative* hence the above result.  Making this parser left
associative is harder than it looks---we can't just swap 'oneNat' and
'calc1' above.  (Why not?)

Even worse, we have no precedence, and so

~~~~~{.haskell}
    ghci> doParse calc1 "10*2+100"
~~~~~

is parsed as:

~~~~~{.haskell}
    10 * (2 + 100)
~~~~~

We'll fix the precedence issue first, then tackle associativity.


Precedence
----------

We can add both left associativity and precedence by stratifying the
parser into different levels.  Here, let's split our binary operations
into addition-like and multiplication-like ones.

> addOp :: Parser (Int -> Int -> Int)
> addOp = char '+' *> pure (+) <|> char '-' *> pure (-)
>
> mulOp :: Parser (Int -> Int -> Int)
> mulOp = char '*' *> pure (*) <|> char '/' *> pure div

Now, we can stratify our language into mutually recursive
sub-languages, where each top-level expression is parsed as a
*sum-of-products*

-- liftA3 (\i1 o i2 -> o i1 i2) oneNat intOp calc1 <|> oneNat

> sumE :: Parser Int
> sumE = liftA3 (\i1 o i2 -> o i1 i2) prodE addOp sumE <|> prodE

> prodE :: Parser Int
> prodE = liftA3 (\i1 o i2 -> o i1 i2) factorE mulOp prodE <|> factorE

> factorE :: Parser Int
> factorE = oneNat <|> ( char '(' *> sumE <* char ')')

~~~~~{.haskell}
    ghci> doParse sumE "10*2+100"
    ghci> doParse sumE "10*(2+100)"
~~~~~

Do you understand why the first parse returned `120` ?  What would
happen if we *swapped* the order of the alternatives?




Parsing Pattern: Chaining
-------------------------

There is not much point gloating about combinators if we are going to
write code like the above -- the bodies of `sumE` and `prodE` are
almost identical!

Let's take a closer look at them. In essence, a `sumE` is
of the form:

~~~~~{.haskell}
    prodE + ( prodE + ( prodE + ... prodE ))
~~~~~

That is, we keep chaining together `prodE` values and adding them for
as long as we can. Similarly a `prodE` is of the form

~~~~~{.haskell}
    factorE * ( factorE * ( factorE * ... factorE ))
~~~~~

where we keep chaining `factorE` values and multiplying them for as
long as we can.

But we're still not done: we need to fix the associativity problem:

~~~~~{.haskell}
    ghci> doParse sumE "10-1-1"
~~~~~

Ugh! I hope you understand why: it's because the above was parsed as
`10 - (1 - 1)` (right associative) and not `(10 - 1) - 1` (left
associative). You might be tempted to fix that simply by flipping the order,

~~~~~{.haskell}
    sumE = addE <|> prodE
      where addE = liftA3 (\x o y -> x `o` y) prodE addOp sumE
~~~~~

but this would be disastrous. Can you see why?  The parser for `sumE`
directly (recursively) calls itself *without consuming any input!*
Thus, it goes off the deep end and never comes back.

Instead, we want to make sure we keep consuming `prodE` values and adding them
up (rather like fold) and so we could do

> sumE1 :: Parser Int
> sumE1 = foldl comb <$> prodE1 <*> rest where
>            comb = \ x (op,y) -> x `op` y
>            rest = many ((,) <$> addOp <*> prodE1)

> prodE1 :: Parser Int
> prodE1 = foldl comb <$> factorE1 <*> rest where
>            comb = \ x (op,y) -> x `op` y
>            rest = many ((,) <$> mulOp <*> factorE1)

> factorE1 :: Parser Int
> factorE1 = oneNat <|>  ( char '(' *> sumE1 <* char ')')

The above is indeed left associative:

~~~~~{.haskell}
    ghci> doParse sumE1 "10-1-1"
~~~~~

Also, it is very easy to spot and bottle the chaining computation
pattern: the only differences are the *base* parser (`prodE1` vs
`factorE1`) and the binary operation (`addOp` vs `mulOp`).  We simply
make those parameters to our *chain-left* combinator:

> chainl1 :: Parser b -> Parser (b -> b -> b) -> Parser b
> p `chainl1` pop = foldl comb <$> p <*> rest where
>            comb = \ x (op,y) -> x `op` y
>            rest = many ((,) <$> pop <*> p)

Similarly, we often want to parse bracketed expressions, so we can
write a combinator

> parenP :: Char -> Parser b -> Char -> Parser b
> parenP l p r = char l *> p <* char r

after which we can rewrite the grammar in three lines:

> sumE2    = prodE2   `chainl1` addOp
> prodE2   = factorE2 `chainl1` mulOp
> factorE2 = parenP '(' sumE2 ')' <|> oneNat

~~~~~{.haskell}
    ghci> doParse sumE2 "10-1-1"
    ghci> doParse sumE2 "10*2+1"
    ghci> doParse sumE2 "10+2*1"
~~~~~


This concludes our in-class exploration of applicative parsing, but what we've
covered is really just the tip of an iceberg. Though parsing is a very old
problem, studied since the dawn of computing, algebraic structures in Haskell
bring a fresh perspective that has now been transferred from Haskell to many
other languages.
