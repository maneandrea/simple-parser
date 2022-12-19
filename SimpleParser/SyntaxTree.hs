module SimpleParser.SyntaxTree
  ( SyntaxTree (..)
  , Crumb (..)
  , Operator (..)
  , Fixity (..)
  , Precedence (..)
  , descend
  , append
  , act
  , actAt
  , goUp
  , goTop
  , compute
  ) where

import Data.List        (intercalate)

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

instance Eq a => Eq (SyntaxTree a) where
  Literal a == Literal b = a==b
  Empty a   == Empty b   = a==b
  Expr h p  == Expr g q  = (h==g) && p==q
  x         == y         = False


-- Next we make the type for infix operators
data Fixity = Infix Int | InfixL Int | InfixR Int
getPrecedence :: Fixity -> Int
getPrecedence (Infix  a) = a
getPrecedence (InfixL a) = a
getPrecedence (InfixR a) = a

data Operator = Operator {opName :: String, fixity :: Fixity}

class Precedence a where
  precedence :: a -> a -> Either (a,a) Ordering

instance Precedence Fixity where
  precedence a b
    | getPrecedence a <  getPrecedence b = Right LT
    | getPrecedence a >  getPrecedence b = Right GT
    | getPrecedence a == getPrecedence b = case (a,b) of
      (InfixR _, InfixR _) -> Right LT
      (InfixL _, InfixL _) -> Right GT
      x                    -> Left x

instance Precedence Operator where
  precedence a b = case precedence (fixity a) (fixity b) of
    Right x -> Right x
    Left  y -> Left  (a,b)

-- Finally, we define a method to compute known parts of an expression
compute :: (Num a, Fractional a) => SyntaxTree a -> SyntaxTree a
compute (Expr op [Literal x, Literal y]) = case op of
  "+" -> Literal $ x+y
  "-" -> Literal $ x-y
  "*" -> Literal $ x*y
  "/" -> Literal $ x/y
  _   -> Expr op [Literal x, Literal y]
compute (Expr op [a,b]) = case map compute [a,b] of
  [Literal aa, Literal bb] -> compute $ Expr op [Literal aa, Literal bb]
  z                        -> Expr op z
compute (Expr op ab)    = Expr op $ map compute ab
compute (Empty [a])     = case compute a of
  Literal x -> Literal x
  y         -> Empty [y]
compute x = x

instance Show a => Show (SyntaxTree a) where
  show = showDepth (-2) where
    showDepth :: Show a => Int -> SyntaxTree a -> String
    showDepth n (Literal x)   = show x ++ "\n" ++ replicate n ' '
    showDepth n (Expr hd pts) = hd ++ "[ " ++ inside ++ close where
      hdlen  = length hd + 2
      inside = intercalate ", " $ map (showDepth $ n+hdlen) pts
      close  = "]\n" ++ replicate n ' '
    showDepth n (Empty pts)   = "( " ++ inside ++ close where
      inside = intercalate ", " $ map (showDepth $ n+2) pts
      close  = ")\n" ++ replicate n ' '

instance Show Operator where
  show (Operator op fix) = op ++ " [" ++ show fix ++ "]"

instance Show Fixity where
  show (Infix i)  = "infix "  ++ show i
  show (InfixL i) = "infixl " ++ show i
  show (InfixR i) = "infixr " ++ show i

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