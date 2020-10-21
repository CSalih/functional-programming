

-- |
-- = Solutions to Exercises for January 18, 2019
module S09 (
  -- * Exercise 2
  xmlToString,
  -- * Exercise 3
  balpar,
  -- * Exercise 4
  parseSet,
  -- * Exercise 5
  T,
  lexTerm,
  -- * Exercise 6
  parseTerm,
  parseTermChar
  ) where

import Xml hiding (lexer)
import Parse
import Set (Set)
import qualified Set
import Term (Term(..))
import qualified Term

-- | Transform a value of type 'Xml' into a 'String'.
xmlToString :: Xml -> String
xmlToString = tos 0
  where
    indent i = replicate (2*i) ' '
    tos i (Txt t) = unlines $ map (indent i ++) $ lines t
    tos i (Xml t ns) =
        indent i ++ "<" ++ t ++ ">\n" ++
          concat (map (tos (i + 1)) ns) ++
        indent i ++ "</" ++ t ++ ">\n"

-- | Consume longest prefix of input tokens that correspond to the grammar of
-- balanced parentheses and return the number of matching pairs of parentheses.
balpar :: Parser Char Int
balpar = do
  xs <- many (between (char '(') (char ')') balpar >>= return . succ)
  return $ sum xs

sepBy, sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 p s = do { x <- p; xs <- many (s >> p); return (x:xs) }
sepBy p s = sepBy1 p s <|> return []

-- | A parser for 'Set's of 'Int's.
parseSet :: Parser Char (Set Int)
parseSet = between (char '{') (char '}') $
    parseInt `sepBy` char ',' >>= return . setFromList
  where
    parseInt = many1 digit >>= return . read
    setFromList [] = Set.empty
    setFromList (x:xs) = x `Set.insert` setFromList xs

-- | A type of tokens for lambda terms.
data T = LP | RP | DOT | LAM | VAR String
  deriving Show

-- | A lexer for lambda 'Term's.
lexTerm :: Parser Char [T]
lexTerm = do
  spaces
  ts <- many $ do
    t <- (char '(' >> return LP)
      <|> (char ')' >> return RP)
      <|> (char '.' >> return DOT)
      <|> (char '\\' >> return LAM)
      <|> (ident >>= return . VAR)
    spaces
    return t
  eoi
  return ts
  where
    ident = many1 (noneof " \n\r\t().\\")

-- | A parser for lambda 'Term's that expects tokens of type 'T'.
parseTerm :: Parser T Term
parseTerm = token var <|> abs <|> app
  where
    par = between (token lp) (token rp)
      where
        lp LP = Just ()
        lp _ = Nothing
        rp RP = Just ()
        rp _ = Nothing
    var (VAR x) = Just (Var x)
    var _ = Nothing
    abs = par $ do
      token lam
      Var x <- token var
      -- for newer GHC versions you might have to replace
      -- the above line by the code block below
      -- t <- token var
      -- x <- case t of
      --        Var x -> return x
      --        _     -> token (const Nothing)
      token dot
      t <- parseTerm
      return $ Abs x t
      where
        lam LAM = Just ()
        lam _ = Nothing
        dot DOT = Just ()
        dot _ = Nothing
    app = par $ do
      t <- parseTerm
      u <- parseTerm
      return $ App t u

-- | A combined lexer/parser for lambda 'Term's that expects 'Char'acter tokesn.
parseTermChar :: Parser Char Term
parseTermChar = do
  ts <- lexTerm
  case parse (parseTerm >>= \t -> eoi >> return t) ts of
    Nothing -> token (const Nothing)
    Just t -> return t

