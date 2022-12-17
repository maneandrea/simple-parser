module SimpleParser.SyntaxTree
  ( SyntaxTree (..)
  , Crumb (..)
  , Parentheses (..)
  , Operator (..)
  , descend
  , append
  , act
  , actAt
  , goUp
  , goTop
  ) where

import Data.List        (intercalate)
import Data.Monoid      ((<>))

-- Copied from https://hackage.haskell.org/package/ilist-0.4.0.1/docs/Data-List-Index.html#v:modifyAt
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (x:xs) = f x : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

{-------------------------------
  Define the syntax tree type
--------------------------------}

data SyntaxTree a
  = Literal a
  | Expr { ehead :: String, parts :: [SyntaxTree a]}
  | Empty [SyntaxTree a]
  -- Literal are the leaves of the tree
  -- Expr is the main structure, which can have zero or more parts
  -- Empty is an expression with no head, like "Sequence" in Mathematica

instance Functor SyntaxTree where
  fmap f (Literal a)   = Literal $ f a
  fmap f (Expr hd pts) = Expr hd $ map (fmap f) pts

-- This semigroup instance is very important:
-- It allows us to construct the tree with <> as the parser runs
instance Semigroup (SyntaxTree a) where
  (Empty p)  <> (Empty q)  = Empty  (p++q)
  (Expr h p) <> (Empty q)  = Expr h (p++q)
  (Empty p)  <> (Expr h q) = Expr h (p++q)  -- never used
  (Expr h p) <> Literal a  = Expr h (p++[Literal a])
  Literal a  <> (Expr h p) = Expr h (Literal a:[Expr h p])  -- never used
  Literal a  <> (Empty p)  = Empty  (Literal a:p)
  (Empty p)  <> Literal a  = Empty  (p++[Literal a])
  (Expr h p) <> (Expr g q) = Empty  (Expr h p:[Expr g q])
  Literal a  <> Literal b  = Empty  [Literal a, Literal b]

-- We also need a Monoid instance to initialize an empty tree
-- or to parse tokens and discard them
instance Monoid (SyntaxTree a) where
  -- strictly speaking this is wrong because it doesn't
  -- satisfy the law x <> mempty = mempty <> x for Literal
  -- but the important thing is that this is the identity for Expr
  mempty = Empty []

-- And we also make a new class: the parentesis class
-- It is used to control the construction of the tree
class Semigroup a => Parentheses a where
  popen  :: a -> a -> a
  pclose :: a -> a

instance Parentheses (SyntaxTree a) where
  popen (Expr h p) (Empty x) = Expr h (p++x)
  -- note in the line below the difference with <> when expr is Expr g q
  popen (Expr h p) expr      = Expr h (p++[expr])
  pclose x = Empty [x]

-- Next we make the type for infix operators
newtype Operator = Operator {opName :: String}

instance Show a => Show (SyntaxTree a) where
  show = showDepth (-2) where
    showDepth :: Show a => Int -> SyntaxTree a -> String
    showDepth n (Literal x)   = show x ++ "\n" ++ replicate n ' '
    showDepth n (Expr hd pts) = hd ++ "[ " ++ inside ++ close where
      hdlen  = length hd + 2
      inside = intercalate ", " $ map (showDepth $ n+hdlen) pts
      close  = "]\n" ++ replicate n ' '
    showDepth n (Empty pts)   = showDepth n (Expr "Sequence" pts)


{-------------------------------
  Define a zipper for the tree
--------------------------------}

data Crumb a = Crumb
  { chead  :: String
  , before :: [SyntaxTree a]
  , after  :: [SyntaxTree a]
  }

instance Show (Crumb a) where
  show (Crumb chead before after) = chead ++ "[@" ++ show (length before) ++ "]"

type BreadCrumbs a = [Crumb a]

type SyntaxZipper a = (SyntaxTree a, BreadCrumbs a)

data TreeError
  = PartTooBig Int Int
  | NoParts
  | NoHead
  | CannotAppend
  | NotLiteral

instance Show TreeError where
  show (PartTooBig a b) = "cannot take part " ++ show a ++ "of expression with length " ++ show b ++ "\n"
  show NoParts          = "cannot take part of a literal\n"
  show NoHead           = "cannot go up one more level\n"
  show CannotAppend     = "cannot append part to literal\n"
  show NotLiteral       = "cannot modify nonatomic expression\n"

-- Descends into the i'th subexpression
descend :: Int -> SyntaxZipper a -> Either TreeError (SyntaxZipper a)
descend i (Literal a, cs) = Left NoParts
descend i (Expr hd pts, cs)
  | length pts >= i       = let (pl, pr') = splitAt i pts
                                pr = tail pr'
                                pi = head pr'
                            in  Right (pi, Crumb hd pl pr: cs)
  | length pts < i        = Left $ PartTooBig i $ length pts

-- Appends a new subexpression
append :: SyntaxTree a -> SyntaxZipper a -> Either TreeError (SyntaxZipper a)
append e (Literal a, cs)   = Left CannotAppend
append e (Expr hd pts, cs) = Right (Expr hd (e:pts), cs)

-- Modify the current node in the zipper
modify :: Int -> (SyntaxTree a -> SyntaxTree a) -> SyntaxZipper a -> Either TreeError (SyntaxZipper a)
modify 0 f (Literal a, cs) = Right (f $ Literal a, cs)
modify i f (Literal a, cs) = Left NoParts
modify i f (Expr hd pts, cs)
  | length pts >= i        = Right (Expr hd (modifyAt i f pts), cs)
  | length pts < i         = Left $ PartTooBig i $ length pts

-- Act with a function in the current node
act :: (a -> a) -> SyntaxZipper a -> Either TreeError (SyntaxZipper a)
act f (Literal a, cs) = Right (Literal $ f a, cs)
act f x               = Left NotLiteral

-- Act in a given part specified by a list
actAt :: [Int] -> (a -> a) -> SyntaxZipper a -> Either TreeError (SyntaxZipper a)
actAt [] f z   = act f z
actAt (i:is) f z = actAt is f =<< descend i z

-- Go one level up
goUp :: SyntaxZipper a -> Either TreeError (SyntaxZipper a)
goUp (x, [])               = Left NoHead 
goUp (x, Crumb h b a : cs) = Right (Expr h (b ++ x:a), cs)

-- Go to the toplevel expression
goTop :: SyntaxZipper a -> SyntaxTree a
goTop (x, []) = x
goTop z       = goTop z' where
  Right z' = goUp z