> {-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

This file defines the abstract syntax, parser and pretty printer for
a simple imperative programming language extended with exceptions.
 
> module WhileExn where

> import Text.PrettyPrint hiding (parens,braces,sep)
> import qualified Text.PrettyPrint as PP
 
> import Control.Applicative
 
> import qualified Parser as P
> import qualified ParserCombinators as P

> import Test.QuickCheck

Abstract Syntax
===============
 
> type Variable = String


> data Block =
>     Block [ Statement ]                 -- { s1; ... sn; }
>   deriving (Eq, Show)
 
and statements themselves can be one of three flavors
 
> data Statement =
>     Assign Variable Expression          -- x = e;
>   | If Expression Block Block           -- if (e) { s1 } else { s2 }
>   | While Expression Block              -- while (e) { s }
>   | Try Block Variable Block            -- try { s1 } handle (x) { s2 }
>   | Throw Expression                    -- throw e;
>   deriving (Eq, Show)

> data Expression =
>     Var Variable                        -- x
>   | Val Value                           -- v
>   | Op Expression Bop Expression        -- e1 op e2
>   deriving (Eq, Show)

> data Bop =
>     Plus     -- +  :: Int -> Int -> Int
>   | Minus    -- -  :: Int -> Int -> Int
>   | Times    -- *  :: Int -> Int -> Int
>   | Divide   -- /  :: Int -> Int -> Int
>   | Gt       -- >  :: Int -> Int -> Bool
>   | Ge       -- >= :: Int -> Int -> Bool
>   | Lt       -- <  :: Int -> Int -> Bool
>   | Le       -- <= :: Int -> Int -> Bool
>   deriving (Eq, Show, Enum)

> data Value =
>     IntVal Int
>   | BoolVal Bool
>   deriving (Eq, Show)

 
> ---------------------------------------------

A Pretty Printer
================

> class PP a where
>   pp :: a -> Doc

> instance PP Bop where
>   pp Plus   = PP.char '+'
>   pp Minus  = PP.char '-'
>   pp Times  = PP.char '*'
>   pp Divide = PP.char '/'
>   pp Gt     = PP.char '>'
>   pp Ge     = PP.text ">="
>   pp Lt     = PP.char '<'
>   pp Le     = PP.text "<="

> oneLine :: PP a => a -> String
> oneLine = PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp

> indented :: PP a => a -> String
> indented = PP.render . pp


> instance PP Value where
>   pp (IntVal i)  = PP.int i
>   pp (BoolVal b) = if b then PP.text "true" else PP.text "false"

> instance PP Expression where
>   pp (Var x) = PP.text x
>   pp (Val x) = pp x
>   pp e@(Op _ _ _) = ppPrec 0 e  where
>      ppPrec n (Op e1 bop e2) =
>         parens (level bop < n) $
>            ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
>      ppPrec _ e' = pp e'
>      parens b = if b then PP.parens else id

> instance PP Block where
>   pp (Block [s])  = pp s
>   pp (Block ss)   = PP.vcat (map pp ss)
>
> ppSS :: [Statement] -> Doc
> ppSS ss  = PP.vcat (map pp ss)

> instance PP Statement where
>   pp (Assign x e) = PP.text x <+> PP.text "=" <+> pp e <> PP.semi
>   pp (If e (Block s1) (Block s2)) =
>     PP.vcat [PP.text "if" <+> PP.parens (pp e) <+> PP.text "{",
>          PP.nest 2 $ ppSS s1,
>          PP.text "}" <+> PP.text "else" <+> PP.text "{",
>          PP.nest 2 $ ppSS s2,
>          PP.text "}" ]
>   pp (While e (Block s))  =
>      PP.vcat [PP.text "while" <+> PP.parens (pp e) <+> PP.text "{",
>          PP.nest 2 $ ppSS s,
>          PP.text "}" ]
>   pp (Throw e) =
>      PP.text "throw" <+> pp e <> PP.semi
>   pp (Try (Block s1) x (Block s2)) =
>      PP.vcat [PP.text "try" <+> PP.text "{",
>          PP.nest 2 $ ppSS s1,
>          PP.text "}" <+> PP.text "handle" <+> PP.parens (PP.text x) <+> PP.text "{",
>          PP.nest 2 $ ppSS s2,
>          PP.text "}"]

> -- use the C++ precendence level table
> level :: Bop -> Int
> level Times  = 7
> level Divide = 7
> level Plus   = 5
> level Minus  = 5
> level _      = 3    -- comparison operators

> ------------------------------------------------------------------------

Parser
======
 

Parsing Constants
-----------------

> valueP :: P.Parser Value
> valueP = intP <|> boolP

> intP :: P.Parser Value
> intP = IntVal <$> P.int

> constP :: String -> a -> P.Parser a
> constP s x = P.string s *> pure x

> boolP :: P.Parser Value
> boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)


Parsing Expressions
-------------------
 
> opP :: P.Parser Bop
> opP =   constP "+" Plus
>     <|> constP "-" Minus
>     <|> constP "*" Times
>     <|> constP "/" Divide
>     <|> constP ">=" Ge     -- GOTCHA: Ge/Le must be before Gt/Lt
>     <|> constP "<=" Le
>     <|> constP ">" Gt
>     <|> constP "<" Lt


> varP :: P.Parser Variable
> varP = some P.lower

> wsP :: P.Parser a -> P.Parser a
> wsP = (<* many P.space)

> exprP :: P.Parser Expression
> exprP = compP where
>    compP   = sumP    `P.chainl1` opLevel (level Gt)
>    sumP    = prodP   `P.chainl1` opLevel (level Plus)
>    prodP   = factorP `P.chainl1` opLevel (level Times)
>    factorP = parens exprP <|> baseP
>    baseP   = Val <$> (wsP valueP) <|> Var <$> (wsP varP)

> opLevel :: Int -> P.Parser (Expression -> Expression -> Expression)
> opLevel l = (\o x y -> Op x o y) <$> P.ensure (\x -> level x == l) (wsP opP)

Parsing Statements
------------------
 
> parens :: P.Parser a -> P.Parser a
> parens x = P.between (sstring "(") x (sstring ")")

> braces :: P.Parser a -> P.Parser a
> braces x = P.between (sstring "{") x (sstring "}")

> sstring :: String -> P.Parser ()
> sstring s = P.string s *> many P.space *> pure ()

> statementP :: P.Parser Statement
> statementP = assignP <|> ifP <|> whileP <|> tryP <|> throwP <|> parens statementP where
>   ifP     = If <$> (sstring "if"   *> parens exprP)
>                <*> blockP
>                <*> (sstring "else" *> blockP)
>   whileP  = While <$> (sstring "while" *> parens exprP)
>                   <*> blockP
>   throwP  = Throw <$> (sstring "throw" *> exprP <* sstring ";")
>   tryP    = Try <$> (sstring "try" *> blockP) <*> (sstring "handle" *> parens (wsP varP)) <*> blockP
>   assignP = Assign <$> wsP varP <*> (sstring "=" *> exprP <* sstring ";")
>
> blockP :: P.Parser Block
> blockP = Block <$> braces (many statementP)

Parsing Blocks
--------------

> toplevelP :: P.Parser Block
> toplevelP = Block <$> many statementP

> parse :: String -> IO (Either P.ParseError Block)
> parse f = P.parseFromFile (const <$> toplevelP <*> P.eof) f
 
Testing Code for Parser/Pretty Printer
--------------------------------------
 
> prop_roundtrip :: Statement -> Bool
> prop_roundtrip s = P.parse statementP (indented s) == Right s


> instance Arbitrary Statement where
>   arbitrary = sized genStatement
>
>   shrink (Assign v e)     = [ Assign v e'      | e'  <- shrink e ]
>   shrink (If v e1 e2)     = [ If v' e1' e2'    | v'  <- shrink v
>                                                , e1' <- shrink e1
>                                                , e2' <- shrink e2 ]
>   shrink (While c e)      = [ While c' e'      | c'  <- shrink c
>                                                , e'  <- shrink e  ]
>   shrink (Throw e)        = [ Throw e'         | e'  <- shrink e ]
>   shrink (Try s1 x s2)    = [ Try s1' x s2'    | s1'  <- shrink s1, s2' <- shrink s2 ]

> genStatement :: Int -> Gen Statement
> genStatement 0 = Assign <$> arbVar <*> genExp 0
> genStatement n = frequency [ (1, liftA2 Assign arbVar (genExp n'))
>                            , (1, liftA3 If (genExp n')
>                                            (genBlock n')
>                                            (genBlock n'))
>                            , (1, liftA2 While (genExp n') (genBlock n'))
>                            , (1, liftA  Throw (genExp n'))
>                            , (1, liftA3 Try (genBlock n') arbVar (genBlock n'))
>                            ]
>   where n' = n `div` 2

> instance Arbitrary Block where
>   arbitrary = sized genBlock
>   shrink (Block ss)       = [ Block ss'        | ss' <- shrink ss ]

> genBlock n = Block <$> genStmts n

> genStmts :: Int -> Gen [Statement]
> genStmts 0 = return []
> genStmts n = frequency [ (1, return []),
>                          (4, (:) <$> genStatement n' <*> genStmts n')]
>   where n' = n `div` 2


> instance Arbitrary Expression where
>   arbitrary = sized genExp
>
>   shrink (Op e1 o e2) = [ Op e1' o e2' | e1' <- shrink e1, e2' <- shrink e2 ]
>   shrink _            = [ ]
>
> genExp :: Int -> Gen Expression
> genExp 0 = oneof  [ Var <$> arbVar
>                   , Val <$> arbitrary
>                   ]
> genExp n = frequency [ (1, Var <$> arbVar)
>                      , (1, Val <$> arbitrary)
>                      , (4, liftA3 Op (genExp n') arbitrary (genExp n')) ]
>   where n' = n `div` 2

> instance Arbitrary Bop where
>   arbitrary = elements [ Plus
>                        , Minus
>                        , Times
>                        , Divide
>                        , Gt
>                        , Ge
>                        , Lt
>                        , Le
>                        ]

> instance Arbitrary Value where
>   arbitrary = oneof   [ IntVal <$> arbitrary
>                       , BoolVal <$> arbitrary
>                       ]

> arbVar :: Gen Variable
> arbVar = elements $ map return ['a'..'z']
 
