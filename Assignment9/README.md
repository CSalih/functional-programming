# Assignment 9

1. Read Chapter 10 of Real World Haskell.

2. Write an xmlToString function, such that, for example
``Xml "div" [Txt "test"]`` results in something similar to
``<div>test</div>``.

3. Implement a parser ``balpar :: Parser Char Int`` that accepts
strings of balanced parentheses according to the grammar
``{Pi} ::= {P}{P} | ({P}) | e`` and returns the total number of pairs of
parentheses. However, note that implementing the above grammar
directly leads to nontermination and is thus not an option.  
**Example:** ``ghci> parse balpar "()(()()())()"``  
Just 6

4. Implement a parser ``parseSet :: Parser Char (Set Int)`` for sets of integers.  
**Example:** ``ghci> parse parseSet "{1,10,3,1}"``  
Just {1,3,10}

5. Given the type of tokens
``` haskell
data T = LP | RP | DOT | LAM | VAR String
deriving Show
```
write a lexer for lambda terms, that is, a parser ``lexTerm :: Parser Char [T]`` that ignores white space and produces a list of tokens, where ``LP`` is a left parenthesis, ``RP`` a right parenthesis, ``DOT`` a dot, ``LAM`` a backslash, and ``VAR`` any sequence of characters excluding the preceding ones and white space.  
**Example:** ``ghci> parse lexTerm "\\x. (y"``  
Just [LAM,VAR "x",DOT,LP,VAR "y"]

6. Using the type ``T`` of the previous exercise, implement a parser ``parseTerm :: Parser T Term`` for fully-parenthesized lambda terms as given by the ``grammar t ::= x | (\x.t) | (t t)``  
**Example:** ``ghci> parse parseTerm [LP,VAR "x",VAR "y",RP]``
Just (x y)