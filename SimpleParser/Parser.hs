module SimpleParser.Parser
    (
      parseMain
    , parseTest
    ) where

-- See this link https://serokell.io/blog/parser-combinators-in-haskell

import Data.List               (intercalate, foldl')
import Control.Applicative     (liftA, Alternative (..), (<**>))
import Control.Monad           (ap)

import SimpleParser.SyntaxTree ( SyntaxTree (..), Parentheses (..), Operator (..)
                               , append, descend, goUp, goTop, act, actAt)
import SimpleParser.ParseError (ParseError (..), InputShow, prependHistory)
import SimpleParser.ParseData  (ParseData (..), prependParsed)


{-------------------------------
  Parser type and its instances
--------------------------------}

newtype Parser i e a = Parser {
  runParser :: [i] -> [Either (ParseError e i) (ParseData i a)]
}

instance Functor (Parser i e) where
  fmap = liftA

instance Applicative (Parser i e) where
  pure = return
  (<*>) = ap

instance Monad (Parser i e) where
  return a = Parser $ \input -> [Right $ ParseData [] a input]
  p >>= f  = Parser $ \input -> concatMap action $ runParser p input where
    remember s (Left e)  = Left  $ prependHistory s e
    remember s (Right p) = Right $ prependParsed s p
    action (Left e)                            = [Left e]
    action (Right (ParseData parsed a input')) = map (remember parsed) $ runParser (f a) input'
                    

instance Alternative (Parser i e) where
  empty   = Parser $ const [Left $ NoAlternative []]
  p <|> q = Parser $ \input -> let
                                 runp = runParser p input
                                 runq = runParser q input
                               in 
                                 runp ++ runq

-- Define a shortcut for concatenating parsers
infixl 4  <+>
(<+>) :: (Semigroup a, Applicative m) => m a -> m a -> m a
p <+> q = (<>) <$> p <*> q

-- When the parse is done we keep only the final result
clean :: [Either (ParseError String i) (ParseData i a)] -> Either (ParseError String i) (ParseData i a)
clean []     = Left $ NoAlternative []
clean (r:rs) = foldl' pick r rs where
  pick (Left e) (Right d)    = Right d
  pick (Right d) (Left e)    = Right d
  pick (Left e) (Left f)     = Left $ max e f
  pick (Right (ParseData p1 a1 i1)) (Right (ParseData p2 a2 i2))
    | length i1 < length i2  = Right $ ParseData p1 a1 i1
    | length i1 > length i2  = Right $ ParseData p2 a2 i2
    | length i1 == length i2 = Left $ CustomError "parsing of this string was ambiguous: " p1

cleanShow :: (InputShow i, Show a) => [Either (ParseError String i) (ParseData i a)] -> String
cleanShow = action . clean where
   action (Left x)  = show x
   action (Right x) = show x 

-- Or we show them all
showAll :: (InputShow i, Show a) => [Either (ParseError String i) (ParseData i a)] -> String 
showAll = concatMap action where
  action (Left x)  = show x
  action (Right x) = show x


{-------------------------------
  Let us define some parsers
--------------------------------}

-- Check that a given token satisfies a predicate
check :: Show i => (i -> Bool) -> Parser i e i
check t = Parser rp where
  rp [] = [Left $ EndOfInput []]
  rp (i:is)
    | t i       = [Right $ ParseData [i] i is]
    | otherwise = [Left  $ Unexpected i [i]]

-- Matches the end of the stream
end :: (Show i, Monoid a) => Parser i e a
end = Parser p where
  p []    = [Right $ ParseData [] mempty []]
  p (i:_) = [Left  $ Unexpected i [i]]

-- Runs a parser but discards the result
discard :: Monoid b => Parser i e a -> Parser i e b
discard = fmap (const mempty)

-- Parser which consumes nothing
nothing :: Monoid a => Parser Char e a
nothing = return mempty

-- Consumes a given token
char :: (Eq i, Show i, Show e) => i -> Parser i e i
char a = check (a==)

-- Consumes any token that belongs to the given list
anyOf :: (Eq i, Show i) => [i] -> Parser i e i
anyOf a = check (`elem` a)

-- Consumes a string of tokens in order
string :: (Eq i, Show i, Show e) => [i] -> Parser i e [i]
string = traverse char

-- Consumes any single token
dot :: Show i => Parser i e i
dot = check (const True)

-- Consumes a word character
wchar :: Parser Char e Char 
wchar = anyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

-- Parses a word
word :: Parser Char e [Char]
word = some wchar

-- Parses a single digit
digit :: Parser Char e Char
digit = anyOf "0123456789"

-- Parses a decimal number
decimal :: Parser Char e (SyntaxTree Int)
decimal = Literal . read <$> many digit

-- Parses an operator
operator :: (Show e) => Parser Char e String
operator = some op where
  op = anyOf "`~!@#$%^&*=+:'<>?/."

-- Parser a floating-point number
number :: (Show e, Read a) => Parser Char e (SyntaxTree a)
number = Literal . read <$> 
  (   (nothing <|> string "-" <|> string "+") 
  <+> (some digit <|> many digit <+> string "." <+> many digit)
  )

-- Parses a comma-separated sequence of objects that 
-- match p and combines them in a "Empty" SyntaxTree
literals :: Show e => Parser Char e (SyntaxTree a) -> Parser Char e (SyntaxTree a)
literals p = nothing <|> literals' where
  literals' = p <|> p <+> discard (char ',') <+> literals'

-- Runs a parser and returns the popen method of the Parentesis class
openToken :: Parentheses b => Parser i e a -> Parser i e (b->b->b)
openToken = fmap (const popen)

-- Runs a parser and returns the pclose method of the Parentesis class
closeToken :: Parentheses b => Parser i e a -> Parser i e (b->b)
closeToken = fmap (const pclose)

-- Parses an expression of the form head[a1,a2,...] where a1,a2,... are parsed by p
simpleExpr :: Show e => Parser Char e (SyntaxTree a) -> Parser Char e (SyntaxTree a)
simpleExpr p =    (`Expr` []) <$> word
             <**> openToken (char '[')
             <*>  literals p 
             <**> closeToken (char ']')

-- Same as before but discards the final closing token
toplevelExpr :: Show e => Parser Char e (SyntaxTree a) -> Parser Char e (SyntaxTree a)
toplevelExpr p =    (`Expr` []) <$> word
               <**> openToken (char '[')
               <*>  literals p 
               <+>  discard (char ']')

-- Parses an expression of the form head[a1,a2,...] where
-- the entries may be expressions themselves, of the same form
expr :: Show e => Parser Char e (SyntaxTree a) -> Parser Char e (SyntaxTree a)
expr p = toplevelExpr (expr' p) <|> p where
  -- Haskell's lazy evaluation allows for this awesomeness
  expr' q = simpleExpr (expr' p) <|> p

{-------------------------------
  Main function definition
--------------------------------}

parseMain :: String -> String 
parseMain input = cleanShow $ runParser parser input where
                    parser :: Parser Char String (SyntaxTree Float)
                    parser = expr number

parseTest :: String 
parseTest = show $ goTop tree where
    Right tree = append newTree =<< goUp =<< actAt [2,4,3] f (syntaxTree, [])
    f :: Int -> Int
    f a = 10*a
    newTree = Expr
      { ehead = "new"
      , parts = [Literal 0, Literal (-1)]
      }    
    syntaxTree = Expr 
      { ehead = "plus"
      , parts = [ Literal 1
                , Literal 2
                , Expr { ehead ="times"
                       , parts = [ Literal 3
                                 , Literal 4
                                 , Expr { ehead ="asd"
                                        , parts=[ Literal 9
                                                , Literal 10
                                                ]
                                        }
                                 , Literal 7
                                 , Expr { ehead = "qwe"
                                        , parts = [ Literal 11
                                                  , Literal 12
                                                  , Literal 13
                                                  , Literal 14
                                                  , Literal 15
                                                  ]
                                        }
                                 ]
                       }
                ]
      }