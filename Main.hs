module Main where

import System.Environment     (getArgs)
import System.IO              (stderr)
import Data.Maybe             (isJust)

import SimpleParser.Parser    (parseMain, parseTest)
import SimpleParser.ArgParser (Args (..), Param (..), Positional (..), Optional (..), parseArgs, getParamValue)

getInput :: Param -> String
getInput p = case getParamValue "INPUT" p of
  Just [input] -> input
  _            -> ""

parsePositional :: Int -> Maybe Positional
parsePositional x = case x of
  0 -> Just $ MandatoryParam "INPUT" ""
  _ -> Nothing

parseFlag :: String -> Maybe Optional
parseFlag x = case x of
  -- Flags that do not consume extra parameters
  "--help"   -> Just $ OptionalFlag "help"
  "-h"       -> Just $ OptionalFlag "help"
  "--file"   -> Just $ OptionalFlag "file"
  "-f"       -> Just $ OptionalFlag "file"
  "--test"   -> Just $ OptionalFlag "test"
  "-t"       -> Just $ OptionalFlag "test"
  -- Flags that consume one extra parameter
  "-o"       -> Just $ OptionalParam "output" ""
  "--output" -> Just $ OptionalParam "output" ""
  "--"       -> Just $ OptionalParam "INPUT" ""
  _          -> Nothing

helpString :: String 
helpString = "usage: simple-parser [-h] INPUT\n\
              \\n\
              \Parses an expression and returns the parse tree\n\
              \\n\
              \positional arguments:\n\
              \  INPUT                 input string or path of input file\n\
              \\n\
              \options:\n\
              \  -h, --help            show this help message and exit"


main :: IO ()
main = do
  args <- getArgs
  case parseArgs parsePositional parseFlag args of
    Args (Left e)                       -> putStrLn $ "\ESC[91m\ESC[1mError\ESC[0m: " ++ show e
    Args (Right (p, _))
      | isJust $ getParamValue "help" p -> putStrLn helpString
      | isJust $ getParamValue "test" p -> putStrLn $ parseTest (getInput p)
      | otherwise                       -> putStr $ parseMain (getInput p)