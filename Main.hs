module Main where

import System.Environment     (getArgs)
import System.IO              (stderr)
import Data.Maybe             (isJust)

import SimpleParser.Parser    (parseMain, parseTest)
import SimpleParser.ArgParser (Args (..), Param, parseArgs, getParamValue, helpString)

getInput :: Param -> String
getInput p = case getParamValue "INPUT" p of
  Just [input] -> input
  _            -> ""

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Args (Left e)                       -> putStrLn $ "\ESC[91m\ESC[1mError\ESC[0m: " ++ show e
    Args (Right (p, _))
      | isJust $ getParamValue "help" p -> putStrLn helpString
      | isJust $ getParamValue "test" p -> putStrLn $ parseTest (getInput p)
      | otherwise                       -> putStr $ parseMain (getInput p)