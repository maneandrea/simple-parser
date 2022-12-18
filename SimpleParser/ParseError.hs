{-# LANGUAGE FlexibleInstances #-}

module SimpleParser.ParseError
  ( ParseError (..)
  , prependHistory
  , InputShow
  ) where

data ParseError e i
  = EndOfInput [i]
  | Unexpected i [i]
  | CustomError e [i]
  | NoAlternative [i]

errstr :: String
errstr = "\ESC[91m\ESC[1mParseError\ESC[0m: "
lerr :: Int
lerr = 12

class Show a => InputShow a where
  inputShow :: a -> String
  inputShow = show

instance InputShow String where
  inputShow = id

instance InputShow Int

instance InputShow Char where
  inputShow = (:[])

instance (InputShow i, InputShow e) => Show (ParseError e i) where
  show (EndOfInput parsed)    = errstr ++ eoi ++ str ++ "\n" ++ markers ++ "^\n" where
                                  eoi     = "unexpected end of input in "
                                  str     = concatMap inputShow parsed
                                  markers =  replicate (length eoi + lerr) ' ' 
                                          ++ replicate (length str) '~'
  show (Unexpected i parsed)  = errstr ++ cnp ++ sym ++ " in " ++ str ++ "\n" ++ markers ++ "\n" where
                                  cnp     = "unexpected symbol "
                                  sym     = show i
                                  str     = concatMap inputShow parsed
                                  markers =  replicate (lerr + length cnp + length sym + 4) ' '
                                          ++ replicate (length $ init str) '~'
                                          ++ replicate (length [last str]) '^'
  show (CustomError e parsed) = errstr ++ cus ++ str ++ "\n" ++ markers ++ "\n" where
                                  cus     = inputShow e
                                  str     = concatMap inputShow parsed
                                  markers =  replicate (lerr + length cus) ' '
                                          ++ replicate (length str) '~'
  show (NoAlternative parsed) = errstr ++ emt ++ str ++ "\n" ++ markers ++ "^\n" where
                                  emt     = "no matching alternatives in "
                                  str     = concatMap inputShow parsed
                                  markers =  replicate (lerr + length emt) ' '
                                          ++ replicate (length str) '~'

instance Eq (ParseError e i) where
  e == f = len e <= len f where
    len (EndOfInput is)    = length is
    len (Unexpected a is)  = length is
    len (CustomError e is) = length is
    len (NoAlternative is) = length is

instance Ord (ParseError e i) where
  e <= f = len e <= len f where
    len (EndOfInput is)    = length is
    len (Unexpected a is)  = length is
    len (CustomError e is) = length is + 1  -- we give a bit of precedence to those
    len (NoAlternative is) = length is

-- Some useful methods
prependHistory :: [i] -> ParseError e i -> ParseError e i
prependHistory s (EndOfInput i)    = EndOfInput (s++i)
prependHistory s (Unexpected a i)  = Unexpected a (s++i)
prependHistory s (CustomError e i) = CustomError e (s++i)
prependHistory s (NoAlternative i) = NoAlternative (s++i)