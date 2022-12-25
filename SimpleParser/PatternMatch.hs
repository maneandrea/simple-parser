module SimpleParser.PatternMatch
    ( Pattern (..)
    , match
    , Rules
    , showRule
    ) where


import SimpleParser.SyntaxTree (SyntaxTree (..), showDepth)
import Safe.Exact              (zipWithExactMay)
import Control.Monad           (foldM)          
import Data.List               (intercalate)

-- Type that represents a pattern
data Pattern a = Variable String | Constant a

instance Show a => Show (Pattern a) where
    show (Variable s) = init s
    show (Constant a) = show a

instance Eq a => Eq (Pattern a) where
    Variable s == Variable r = s == r
    Constant s == Constant r = s == r
    x == y = False

-- Type for the final assignments pattern -> subexpression
type Rules a = [(String, SyntaxTree a)]

-- We cannot make a type synonym an instance, so we define a new show function
showRule :: Show a => Rules a -> String
showRule []           = ""
showRule ((s, e):rs)  = init s ++ " -> " ++ showDepth (length s + 1) e ++ showRule rs

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

-- Compares an expression against a pattern. If it doesn't match returns
-- Nothing, otherwise it returns Just the list of assignments
match :: Eq a => Rules a -> SyntaxTree a -> SyntaxTree (Pattern a) -> Maybe (Rules a)

match r (Literal x) (Literal (Constant y))
    | x == y    = Just r
    | otherwise = Nothing

match r e (Literal (Variable v)) = addRule (v, e) r

match r (Expr h p) (Expr g q)
    | h == g    = let rules    = zipWithExactMay (match []) p q
                      rules'   = sequence =<< rules
                      allRules = case rules' of
                        Nothing -> Nothing
                        Just t  -> foldM addRules r t
                  in  allRules
    | otherwise = Nothing