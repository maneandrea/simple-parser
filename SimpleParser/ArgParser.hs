module SimpleParser.ArgParser
  ( parseArgs
  , Args (..)
  , Param (..)
  , Positional (..)
  , Optional (..)
  , getParamValue
  ) where
import Data.List (intercalate)
import Control.Applicative ((<|>))

data ParseError
  = UnrecognizedOption String
  | InvalidValue String String
  | ExpectedInput Int
  | UnexpectedInput

data Positional 
  = MandatoryParam String String
  | MandatoryParamMulti String [String]

data Optional
  = OptionalFlag String 
  | OptionalParam String String 
  | OptionalParamMulti String [String]

newtype Param = Param ([Positional], [Optional])

newtype Args = Args {runArgs :: Either ParseError (Param, [String])}

class ReadParam a where
  name :: a -> String
  vals :: a -> [String]

instance Show ParseError where
  show (UnrecognizedOption a) = "unrecognized option: " ++ a
  show (InvalidValue a b)     = "invalid value for " ++ a ++ ": " ++ b
  show (ExpectedInput a)      = "expected more positional arguments, found only " ++ show a
  show UnexpectedInput        = "too many positional arguments provided"

instance Show Positional where
  show (MandatoryParam a b)      = a ++ "=" ++ b
  show (MandatoryParamMulti a b) = a ++ "=" ++ intercalate "," b

instance Show Optional where
  show (OptionalFlag a)         = a ++ "=true"
  show (OptionalParam a b)      = a ++ "=" ++ b
  show (OptionalParamMulti a b) = a ++ "=" ++ intercalate "," b

instance Show Param where
  show (Param (pos, opt)) = intercalate "\n" $ map show pos ++ map show opt

instance ReadParam Positional where
  name (MandatoryParam a b) = a
  name (MandatoryParamMulti a b) = a
  vals (MandatoryParam a b) = [b]
  vals (MandatoryParamMulti a b) = b

instance ReadParam Optional where
  name (OptionalFlag a) = a
  name (OptionalParam a b) = a
  name (OptionalParamMulti a b) = a
  vals (OptionalFlag a) = []
  vals (OptionalParam a b) = [b]
  vals (OptionalParamMulti a b) = b

{-- Examples:
parsePositional :: Int -> Maybe Positional
parsePositional x = case x of
  -- Positional parameters that consume one token
  0 -> Just $ MandatoryParam "INPUT" ""
  -- Positional parameters that consume multiple tokens
  1 -> Just $ MandatoryParamMulti "OUTPUT" []
  _ -> Nothing

parseFlag :: String -> Maybe Optional
parseFlag x = case x of
  -- Flags that do not consume extra parameters
  "--help"   -> Just $ OptionalFlag "help"
  "-h"       -> Just $ OptionalFlag "help"
  -- Flags that consume one extra parameter
  "-o"       -> Just $ OptionalParam "option" ""
  "--option" -> Just $ OptionalParam "option" ""
  -- Flags that consume more parameters
  "-m"       -> Just $ OptionalParamMulti "multi" []
  "--multi"  -> Just $ OptionalParamMulti "multi" []
  _          -> Nothing
 --}

unparsedArgs :: [String] -> Args
unparsedArgs s = Args $ Right (Param ([], []), s)

processArgs :: (Int -> Maybe Positional) -> (String -> Maybe Optional) -> Args -> Args

processArgs parsePositional parseFlag (Args (Left x)) = Args $ Left x

processArgs parsePositional parseFlag (Args (Right (Param (p, o), []))) = case parsePositional (length p) of
  Just _   -> Args $ Left $ ExpectedInput $ length p 
  Nothing  -> Args $ Right (Param (p, o), [])

processArgs parsePositional parseFlag (Args (Right (Param (p, o), x:xs)))
  | isPositional x = case parsePositional (length  p) of
    Just (MandatoryParam y z)       -> processArgs parsePositional parseFlag $ Args $ Right (Param (MandatoryParam y x:p, o), xs)
    Just (MandatoryParamMulti y []) -> processArgs parsePositional parseFlag $ Args $ Right (Param (MandatoryParamMulti y headX:p, o), tailX) where
                                        (headX, tailX) = consumeUntil ([], x:xs)
    Nothing                         -> Args $ Left UnexpectedInput
  | otherwise = case parseFlag x of
    Just (OptionalFlag y)           -> case y of
                                        "help" -> Args $ Right (Param ([], [OptionalFlag y]), [])
                                        _      -> processArgs parsePositional parseFlag $ Args $ Right (Param (p , OptionalFlag y:o), xs)
    Just (OptionalParam "INPUT" _)  -> case xs of
                                        []     -> Args $ Left (InvalidValue "INPUT" "")
                                        z:zs   -> processArgs parsePositional parseFlag $ Args $ Right (Param (MandatoryParam "INPUT" z:p, o), zs)
    Just (OptionalParam y _)        -> case xs of
                                        []     -> Args $ Left (InvalidValue y "")
                                        z:zs   -> processArgs parsePositional parseFlag $ Args $ Right (Param (p , OptionalParam y z:o), zs)
    Just (OptionalParamMulti y _)  -> processArgs parsePositional parseFlag $ Args $ Right (Param (p, OptionalParamMulti y headX:o), tailX) where
                                        (headX, tailX) = consumeUntil ([], xs)
    Nothing                         -> Args $ Left (UnrecognizedOption x)
  where
    isPositional s = case s of
      ""    -> False
      '-':s -> False
      s     -> True
    consumeUntil (a, [])   = (a, [])
    consumeUntil (a, b:bs)
      | isPositional b = consumeUntil (b:a, bs)
      | otherwise      = (a, b:bs)


parseArgs :: (Int -> Maybe Positional) -> (String -> Maybe Optional) ->  [String] -> Args
parseArgs x y = processArgs x y . unparsedArgs

getParamValue :: String -> Param -> Maybe [String]
getParamValue s (Param (p, o)) = get s p <|> get s o where
  get :: (ReadParam a) => String -> [a] -> Maybe [String]
  get s [] = Nothing
  get s (x:xs)
    | name x == s = Just $ vals x
    | otherwise   = get s xs

