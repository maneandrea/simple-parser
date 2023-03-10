module SimpleParser.PatternMatch
    ( Pattern (..)
    , Replacement (..)
    , match
    , replaceTop
    , replace
    , Rules
    , showRule
    ) where


import SimpleParser.SyntaxTree (SyntaxTree (..), showDepth)
import Control.Monad           (foldM)          
import Data.List               (intercalate)
import Data.Maybe              (catMaybes, fromMaybe)

-- Type that represents a pattern
data Pattern a = Variable String | Some String | Many String | Constant a

-- Type that represents a backreference
data Replacement a = BackReference String | Literally a


instance Show a => Show (Pattern a) where
    show (Variable s) = init s
    show (Constant a) = show a

instance Eq a => Eq (Pattern a) where
    Variable s == Variable r = s == r
    Constant s == Constant r = s == r
    x == y = False

instance Eq a => Eq (Replacement a) where
    BackReference s == BackReference r = s == r
    Literally s == Literally r = s == r
    x == y = False

-- Type for the final assignments pattern -> subexpression
type Rules a = [(String, SyntaxTree a)]

-- We cannot make a type synonym an instance, so we define a new show function
showRule :: Show a => Rules a -> String
showRule []           = ""
showRule ((s, e):rs)  = s ++ " -> " ++ showDepth (length s + 2) e ++ showRule rs
 

-- Appends a rule to a list of rules only if the key is not present.
-- If the key is present with the same value it returns Just that,
-- otherwise it returns Nothing
addRule :: Eq a => (String, SyntaxTree a) -> Rules a -> Maybe (Rules a) 
addRule (s, e) r = case lookup s r of
    Nothing -> Just $ (s, e):r
    Just f  -> if e == f then Just r else Nothing

-- Iterate the function above on a list of rules
addRules :: Eq a => Rules a -> Rules a -> Maybe (Rules a)
addRules [] s     = Just s
addRules (r:rs) s = addRules rs =<< addRule r s

-- Replaces the given rules in an expression
subs :: Rules a -> SyntaxTree (Replacement a) -> SyntaxTree a
subs r (Literal (Literally a))     = Literal a
subs r (Literal (BackReference a)) = case lookup a r of
    Nothing -> Expr a []
    Just t  -> t
subs r (Expr h p)                  = Expr h $ reconstruct p where
    reconstruct []     = []
    reconstruct (q:qs) = let w            = subs r q
                             go (Empty l) = l ++ reconstruct qs
                             go s         = s :  reconstruct qs
                         in  go w 

-- Compares an expression against a pattern. If it doesn't match returns
-- Nothing, otherwise it returns Just the list of assignments
match :: Eq a => Rules a -> SyntaxTree a -> SyntaxTree (Pattern a) -> Maybe (Rules a)

match r (Literal x) (Literal (Constant y))
    | x == y    = Just r
    | otherwise = Nothing

match r e (Literal (Variable v)) = addRule (v, e) r

match r e (Literal (Some v)) = addRule (v, Empty [e]) r

match r e (Literal (Many v)) = addRule (v, Empty [e]) r

match r (Expr h p) (Expr g q)
    | h == g    = let zipped  = segmentedZip p q
                      rules   = map sequence zipped
                      foldAdd Nothing  = Nothing
                      foldAdd (Just t) = foldM addRules r t
                      rules'  = map foldAdd rules
                      rules'' = catMaybes rules'
                  in  case rules'' of
                        []     -> Nothing
                        (y:ys) -> Just y
    | otherwise = Nothing

match r e f = Nothing 

segmentedZip :: Eq a => [SyntaxTree a] -> [SyntaxTree (Pattern a)] -> [[Maybe (Rules a)]]
segmentedZip [] []         = [[]]

segmentedZip (x:xs) []     = []

segmentedZip [] (y:ys)     = case y of
    Literal (Many a) -> (Just [(a, Empty [])] :) <$> segmentedZip [] ys
    _                -> []

segmentedZip (x:xs) (y:ys) = case y of
        Literal (Some a) -> go a [x] xs
        Literal (Many a) -> go a [] (x:xs)
        _                -> (match [] x y :) <$> segmentedZip xs ys
    where
        go a z []     = (Just [(a, Empty z)] :) <$> segmentedZip [] ys
        go a z (t:ts) = ((Just [(a, Empty z)] :) <$> segmentedZip (t:ts) ys) ++ go a (z++[t]) ts 

-- Compares an expression against a pattern and, if it matches, it applies a replacement
replaceTop :: Eq a => SyntaxTree a -> SyntaxTree (Pattern a) -> SyntaxTree (Replacement a) -> SyntaxTree a
replaceTop a b c = case match [] a b of
    Nothing -> a
    Just r  -> subs r c

-- Compares an expression or its subexpressions against a pattern and, if it matches, it applies a replacement
-- The primed version returns Just the espression if the replacement happened and Nothing otherwise   
replace' :: Eq a => SyntaxTree a -> SyntaxTree (Pattern a) -> SyntaxTree (Replacement a) -> Maybe (SyntaxTree a)
replace' (Literal a) b c = (`subs` c) <$> match [] (Literal a) b
replace' (Expr h pt) b c = case match [] (Expr h pt) b of
    Just r  -> Just $ subs r c
    Nothing -> let go x []     = Nothing
                   go x (q:qs) = case replace' q b c of
                                    Nothing -> go (x++[q]) qs
                                    Just s  -> Just $ x ++ s : qs
                   pt' = go [] pt
               in  Expr h <$> pt'

replace :: Eq a => SyntaxTree a -> SyntaxTree (Pattern a) -> SyntaxTree (Replacement a) -> SyntaxTree a
replace a b c = fromMaybe a (replace' a b c)
