module SimpleParser.PatternMatch
    ( Pattern (..)
    ) where

-- Type that represents a pattern
data Pattern a = Variable String | Constant a

instance Show a => Show (Pattern a) where
    show (Variable s) = s
    show (Constant a) = show a

instance Eq a => Eq (Pattern a) where
    Variable s == Variable r = s == r
    Constant s == Constant r = s == r
    x == y = False