XML parsing
===========

A very simple example of parsing very simple XML data.

> module Xml where

> import Control.Applicative (Alternative(..))
> import System.IO

We base this file on the definitions so far in lecture
(You can also import the `Parser` and `ParserCombinator` modules from your homework.)

> import Parsers (Parser, satisfy, char, string, doParse)

You will need these functions in addition to the Applicative and Alternative
type class instances for the `Parser` type.

Our goal: produce this structured data from a string

> -- | A simplified datatype for storing XML
> data SimpleXML =
>           PCDATA  String
>         | Element ElementName [SimpleXML]
>       deriving Show
>
> type ElementName = String
 
 
First: the characters `/`, `<`, and `>` are not allowed to appear in tags or
PCDATA.

> reserved :: Char -> Bool
> reserved c = c `elem` ['/', '<', '>']
 
Use this definition to parse a maximal nonempty sequence of nonreserved characters:

> text :: Parser String
> text = undefined

~~~~{.haskell}
    Xml> doParse text "skhdjf<ksjhdfhjksd"
~~~~

and then use this definition to parse raw text in an XML file

> pcdata :: Parser SimpleXML
> pcdata = undefined

~~~~{.haskell}
    Xml> doParse pcdata "skhdjf<ksjhdfhjksd"
~~~~

Parse an empty element, like `"<br/>"`

> emptyContainer :: Parser SimpleXML
> emptyContainer = undefined

~~~~~{.haskell}
    Xml> doParse emptyContainer "<br/>"
~~~~~

 
Parse a container element consisting of an open tag, a sequence of
some content, and matching a closing tag.  (For example, `<br></br>` or
`<title>A midsummer night's dream</title>`.)  You do NOT need to make
sure that the closing tag matches the open tag.

> container :: Parser SimpleXML -> Parser SimpleXML
> container p = undefined

~~~~~{.haskell}
    Xml> doParse (container pcdata) "<br></br>"
    Xml> doParse (container pcdata) "<title>A midsummer night's dream</title>"
     -- should also work, even though the tag is wrong
    Xml> doParse (container pcdata) "<title>A midsummer night's dream</br>"
~~~~~

Now put the above together to construct a parser for simple XML data:

> xml :: Parser SimpleXML
> xml = undefined

~~~~~{.haskell}
   Xml> doParse xml "<body>a</body>"
   Xml> doParse xml "<body><h1>A Midsummer Night's Dream</h1><h2>Dramatis Personae</h2>THESEUS, Duke of Athens.<br/>EGEUS, father to Hermia.<br/></body>"
~~~~~

Now let's try it on something a little bigger. How about [dream.html](dream.html)?

> -- | Run a parser on a particular input file
> parseFromFile :: Parser a -> String -> IO [(a,String)]
> parseFromFile parser filename = do
>   handle <- openFile filename ReadMode
>   str    <- hGetContents handle
>   return $ doParse parser str



~~~~~{.haskell}
    Xml> parseFromFile xml "dream.html"
~~~~~
