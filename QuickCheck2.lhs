
Case study: Checking Compiler Optimizations
===========================================

QuickCheck case study: Checking Compiler Optimizations
======================================================

> module QuickCheck2 where

> import Test.QuickCheck (Arbitrary(..),Gen(..),Property(..),OrderedList(..),
>                         forAll,frequency,elements,sized,oneof,(==>),collect,
>                         quickCheck,sample,choose,quickCheckWith,
>                         classify,stdArgs,maxSuccess)
> import Control.Monad (liftM,liftM2,liftM3)
> import Data.List (sort,insert)
> import Data.Maybe (fromMaybe)

> import Data.Map (Map)
> import qualified Data.Map as Map


Recap
=====

The story so far...

- QuickCheck is a Haskell library for _property-based random testing_

- QC _properties_ are Boolean-valued functions that (partially)
specify the behavior of a function we want to test.  E.g.

> prop_revapp_ok :: [Int] -> [Int] -> Bool
> prop_revapp_ok xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

- More generally, QC provides a typeclass of `Testable` things, of
which Bool is one instance.

- We can give flags to quickCheck to control how many test cases it runs

> quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

- QC also offers a number of combinators for building up "Properties,"
which can be thought of as a generalized form of boolean expressions.

- The `==>` combinator builds _conditional properties_:

     prop_revapp_ok :: [Int] -> [Int] -> Bool
     prop_revapp_ok xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

The advantage of writing `p ==> q` instead of just `not p || q` is
that QC will keep track of the _discard ratio_ of conditional tests
that succeed trivially because the precondition fails.  

- To avoid too many discards, it is often necessary to build _custom
generators_ for random test data.  

    - Create a fresh type (e.g., using `newtype`) for the data that is
      to be generated, so that QC's default generators will not be used.

    - Make this type an instance of the class `Arbitrary`

         class Arbitrary a where
           arbitrary :: Gen a

      where `Gen a` can be read as the type of generators for random
      values of type a.

    - Use QC's _generator combinators_ to build an appropriate
      generator for the distribution you have in mind: `choose`, 
      `oneof`, `elements`, `frequency`, etc.

      (The _monad combinators_ `liftM`, `liftM2`, and `liftM3` are 
      also useful.)

    - Use the "debugging" combinators `collect` and `classify` 
      to help tune the distribution

-------------------------------------------------------------------------

Let's look in more detail at how QC can be used to generate structured
data, by doing a small case study on program expressions.

Consider the following little language of arithmetic expressions.

> data Expression =
>    Var  Variable
>  | Val  Value
>  | Op   Bop Expression Expression
>  deriving (Eq, Ord)

Variables and values are called "atomic expressions", whereas the last
case lets us build larger bits of arithmetic.

Variables are basically just strings, but instead of using `String`
directly, we create a newtype.

> newtype Variable = V String deriving (Eq, Ord)

Values include literal integer or boolean values.

> data Value =
>     IntVal Int
>   | BoolVal Bool
>   deriving (Eq, Ord)

Here are the possible boolean operators:

> data Bop = 
>     Plus     -- +  :: Int  -> Int  -> Int
>   | Minus    -- -  :: Int  -> Int  -> Int
>   | Times    -- *  :: Int  -> Int  -> Int
>   | Gt       -- >  :: Int -> Int -> Bool 
>   | Ge       -- >= :: Int -> Int -> Bool
>   | Lt       -- <  :: Int -> Int -> Bool
>   | Le       -- <= :: Int -> Int -> Bool
>   deriving (Eq, Ord)



Showing expressions
-------------------

A small digression is in order here. Before we go any further with
expressions, we'd like a way to look at them. We could just add
"deriving (Show)" to the datatypes above, but that will lead to fairly
unreadable examples. Instead, let's show them as traditional
mathematical expressions.

> instance Show Variable where 
>     show (V x) = x

> instance Show Value where
>     show (IntVal x)  = show x
>     show (BoolVal x) = show x

> instance Show Bop where 
>     show b = case b of 
>      Plus  -> "+"
>      Minus -> "-"
>      Times -> "*"
>      Gt    -> ">" 
>      Ge    -> ">="
>      Lt    -> "<"
>      Le    -> "<="

For nonatomic expressions, we'll use a little operator precedence to
conditionally add parens where necessary. Instead of defining 'show',
we define 'showsPrec' in the `Show` instance for `Expression`s.

    showsPrec :: Show a => Int -> a -> ShowS

    type ShowS = String -> String 	-- Defined in ‘GHC.Show’

The `showsPrec` function takes an argument `d` which is the operator
precedence of the enclosing context. In an operator expression, if the
precednce of the operator is *lower* than the precedence of the context, we
should add parens around the entire expression.  Furthermore, as we show the
subexpressions of the operator, we should use the precedence level of the 
operator for the context.

> instance Show Expression where
>    showsPrec d (Var x) = showsPrec d x
>    showsPrec d (Val x) = showsPrec d x
>    showsPrec d (Op bop e1 e2) = showParen (op_prec < d) $
>        showsPrec op_prec e1 .
>        showsPrec d bop .
>        showsPrec op_prec e2
>     where op_prec = precedence bop

> precedence :: Bop -> Int
> precedence Times = 8
> precedence Plus  = 7
> precedence Minus = 7
> precedence Gt    = 6
> precedence Ge    = 6
> precedence Lt    = 6
> precedence Le    = 6


    *Main> show (Op Plus (Val (IntVal 1)) (Op Times (Val (IntVal 2)) (Val (IntVal 3))))

    *Main> show (Op Times (Val (IntVal 1)) (Op Plus (Val (IntVal 2)) (Val (IntVal 3))))





Semantics
---------

To compute the value of an expression, we need a _store_, i.e. a map from
variables to their values.  Finite maps (AKA "dictionaries") are part
of the standard library Data.Map (which we imported at the top of this
file).

> type Store = Map Variable Value

With such a store, we can determine the value of any expression. If
there is any sort of error in the evaluation (i.e. the variable is not
in the map, or the wrong type of values are given to an operator)
we'll just return `null` ... er, `IntVal 0`.

> evalBop :: Bop -> Value -> Value -> Value
> evalBop Plus  (IntVal v1) (IntVal v2) = IntVal (v1 + v2)
> evalBop Times (IntVal v1) (IntVal v2) = IntVal (v1 * v2)
> evalBop Minus (IntVal v1) (IntVal v2) = IntVal (v1 - v2)
> evalBop Gt    (IntVal v1) (IntVal v2) = BoolVal (v1 > v2)
> evalBop Ge    (IntVal v1) (IntVal v2) = BoolVal (v1 >= v2)
> evalBop Lt    (IntVal v1) (IntVal v2) = BoolVal (v1 < v2)
> evalBop Le    (IntVal v1) (IntVal v2) = BoolVal (v1 <= v2)
> evalBop _     _           _           = IntVal 0

Next, we need an evaluator for expressions:

> eval :: Expression -> Store -> Value
> eval = undefined




Generating Expressions
----------------------

Next, let's write some simple expression generators.

First, let's write a generator for variables, where we assume that the
programs are over variables drawn from the uppercase alphabet
characters.  

> instance Arbitrary Variable where 
>   arbitrary = undefined





Second, we can write a generator for constant values (that can appear
in expressions) that simply chooses between randomly generated `Bool`
and `Int` values.

> instance Arbitrary Value where 
>   arbitrary = undefined






Third, we define generators for `Expression` and `Bop` which select
from the different cases.

> instance Arbitrary Bop where
>   arbitrary = elements [Plus, Times, Minus, Gt, Ge, Lt, Le]




For expressions, let's use `frequency` so we can play with relative
frequencies.  To start off, let's choosing binary operators five times
more often than the atomic expressions.

> instance Arbitrary Expression where
>   arbitrary = undefined




Let's try sampling a few expressions and see what we get...


~~~~~{.haskell}
*Main> sample (arbitrary :: Gen Expression)
~~~~~







Uh?  Whats going on?  Let's think a bit about how _big_ the
expressions will be that we generate...





Let's try adjusting the definition above to get a better distribution.












Hm....  Can we generate smaller expressions without getting too many
atomic expressions?


> arbnE :: Int -> Gen Expression
> arbnE n = frequency [ (1, liftM Var arbitrary), 
>                       (1, liftM Val arbitrary),
>                       (n, liftM3 Op arbitrary (arbnE (n `div` 2))
>                           (arbnE (n `div` 2))) ]









In the above, we keep *halving* the number of allowed nodes, and when that
number goes to 0 we just return an atomic expression (either a variable
or a constant.) We can now update the generator for expressions to

> {-
> instance Arbitrary Expression where
>   arbitrary = sized arbnE
> -}




Finally, we automatically have a generator for `Store` from the
generator for Maps.         

ghci> sample (arbitrary :: Gen Store)
    






Expression Equivalence
----------------------

Let `e1` and `e2` be two expressions. We say that `e1` is *equivalent
to* `e2` if for all stores `st` the value resulting from `e1` with
`st` is the same as that obtained by evaluating `e2` from
`st`. Formally:

> (===) ::  Expression -> Expression -> Property
> e1 === e2 = forAll arbitrary $ \st -> eval e1 st == eval e2 st





Checking An Optimization: Zero-Add-Elimination
----------------------------------------------

Excellent! Let's take our generators our for a spin, by checking some
*compiler optimizations*. Intuitively, a compiler optimization (or 
transformation) can be viewed as a *pair* of programs -- the input 
program `p_in` and a transformed program `p_out`. A transformation 
`(p_in, p_out)`is *correct* iff `p_in` is equivalent to `p_out`.

Here's a simple *sanity* check property corresponding to an
optimization we might like a compiler to make:

> prop_add_zero_elim :: Variable -> Expression -> Property
> prop_add_zero_elim x e = 
>  (Op Plus e $ Val (IntVal 0)) === e 




Let's check the properties!

~~~~~{.haskell}
*Main> quickCheck prop_add_zero_elim 
~~~~~






Whoops! Forgot about those pesky boolean expressions! If you think about it,

~~~~~{.haskell}
True + 0
~~~~~{.haskell}

evaluates to 0, whereas 

~~~~~{.haskell}
True 
~~~~~{.haskell}

is just True.





Ok, let's limit ourselves to *Integer* expressions (using a slightly
different way of controlling the size):

> intE :: Gen Expression
> intE = sized arbnEI 
>   where arbnEI 0 = oneof [ liftM Var arbitrary
>                          , liftM (Val . IntVal) arbitrary ]
>         arbnEI n = oneof [ liftM Var arbitrary
>                          , liftM (Val . IntVal) arbitrary
>                          , liftM2 (Op Plus)   (arbnEI n_by_2) (arbnEI n_by_2) 
>                          , liftM2 (Op Times)  (arbnEI n_by_2) (arbnEI n_by_2) 
>                          , liftM2 (Op Minus)  (arbnEI n_by_2) (arbnEI n_by_2) 
>                          ]
>                    where n_by_2 = n `div` 2

using which, we can tweak the property to limit ourselves to integer
expressions:

> prop_add_zero_elim' :: Property
> prop_add_zero_elim' = 
>   forAll intE $ \e -> (Op Plus e $ Val (IntVal 0)) === e






O, Quickcheck, what say you now?

~~~~~{.haskell}
*Main> quickCheck prop_add_zero_elim'
~~~~~







Of course! in the input state where `N` has the value `True`, the
result of evaluating `N` is quite different from executing `N + 0`. Oh
well, so much for that optimization.  Looks like we need some type
information before we can eliminate additions-to-zero!






Checking An Optimization: Constant Folding (sort of) 
----------------------------------------------------

The last example ran aground because expressions are untyped (tsk
tsk).  Let's look at another optimization that is not plagued by this
sort of confusion.

Suppose we evaluate an expression `e` to a value `v` and then name
that value `x` in the store.  Then evaluating `x` should be the same
as evaluating `e`, right?  This behavior is sort of like constant
propagation---i.e., replacing this code

~~~~~{.haskell}
X := e
Y := e
~~~~~

with this this one:

~~~~~{.haskell}
X := e
Y := X
~~~~~



Let's see how we might express the correctness of this transformation 
as a QC property

> prop_const_prop ::  Variable -> Expression -> Store -> Bool
> prop_const_prop x e s = eval (Var x) s' == eval e s' where
>   s' = Map.insert x (eval e s) s




Mighty QC, do you agree ?

~~~~~{.haskell}
*Main> quickCheck prop_const_prop 
~~~~~






Shrinking 
---------

Holy transfer function!! It fails?!! And what is that bizarre test? It
seems rather difficult to follow. Turns out, QC comes with a *test
shrinking* mechanism; all we need do is add to the `Arbitrary` instance
a function of type

~~~~~{.haskell}
shrink :: a -> [a]
~~~~~

which will take a candidate and generate a list of *smaller* candidates
that QC will systematically crunch through till it finds a minimally
failing test!

> {- 
> instance Arbitrary Expression where
>   arbitrary = sized arbnE
>
>   shrink = undefined
> -}





Let's try it again to see if we can figure it out!

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_const_prop 

*** Failed! Falsifiable (after 26 tests and 4 shrinks):    
D
U
A + D
fromList [(D,-638),(G,256),(H,False),(K,False),(O,True),(R,True),(S,-81),(T,926)]
~~~~~

Aha! Consider the two programs

~~~~~{.haskell}
D := A + D; 
U := A + D
~~~~~

and:

~~~~~{.haskell}
D := A + D; 
U := D
~~~~~

Are they equivalent? Pretty subtle, eh. 








Well, hopefully this has convinced you that QuickCheck is pretty
awesome.  The astonishing thing about it is its sheer simplicity -- a
fistful of typeclasses and a tiny pinch of monads and lo! a shocking
useful testing technique that can find a bunch of subtle bugs or
inconsistencies in your code.

Moral of the story -- types can go a long way towards making your code
*obviously correct*, but not the whole distance. Make up the
difference by writing properties, and have the machine crank out tests
for you!

There is a large literature on QuickCheck on the web. It is used for a
variety of commercial applications, both in Haskell and in pretty much
every modern language, including [Perl][10].  Even if you don't
implement a system in Haskell, you can use QuickCheck to test it, by
just using the nifty [data generation][9] facilities.

Credit: This lecture is adapted from one used at UCSD [12].

[9]: http://www.haskell.org/haskellwiki/QuickCheck_as_a_test_set_generator
[10]: http://community.moertel.com/~thor/talks/pgh-pm-talk-lectrotest.pdf
[12]: http://cseweb.ucsd.edu/classes/wi11/cse230/lectures/quickcheck.lhs
