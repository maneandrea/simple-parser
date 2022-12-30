module Main where

import System.Environment     (getArgs)
import System.IO              (stderr)
import Data.Maybe             (isJust)

import SimpleParser.Parser    (parseMatch, parseReplace, parseTest)
import SimpleParser.ArgParser (Args (..), Param (..), Positional (..), Optional (..), parseArgs, getParamValue)

getInput :: Param -> String
getInput p = case getParamValue "INPUT" p of
  Just [input] -> input
  _            -> ""

getPattern :: Param -> String
getPattern p = case getParamValue "PATTERN" p of
  Just [pattern] -> pattern
  _              -> "_"

getRepl :: Param -> String
getRepl p = case getParamValue "repl" p of
  Just [input] -> input
  _            -> ""


parsePositional :: Int -> Maybe Positional
parsePositional x = case x of
  0 -> Just $ MandatoryParam "INPUT" ""
  1 -> Just $ MandatoryParam "PATTERN" "_"
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
  "-r"       -> Just $ OptionalParam "repl" ""
  "--repl"   -> Just $ OptionalParam "repl" ""
  _          -> Nothing

helpString :: String 
helpString = "usage: pattern-match [-h] [-r REPL] INPUT PATTERN\n\
              \\n\
              \Matches an expression with a pattern\n\
              \\n\
              \positional arguments:\n\
              \  INPUT                 input string or path of input file\n\
              \  PATTERN               pattern string to match against INPUT\n\
              \\n\
              \options:\n\
              \  -r, --repl            do not return a list of assignments but replaces the pattern with this\n\
              \  -h, --help            show this help message and exit"

main :: IO ()
main = do
  args <- getArgs
  case parseArgs parsePositional parseFlag args of
    Args (Left e)                       -> putStrLn $ "\ESC[91m\ESC[1mError\ESC[0m: " ++ show e
    Args (Right (p, _))
      | isJust $ getParamValue "help" p -> putStrLn helpString
      | isJust $ getParamValue "repl" p -> putStr $ parseReplace (getInput p) (getPattern p) (getRepl p)
      | otherwise                       -> putStr $ parseMatch (getInput p) (getPattern p)