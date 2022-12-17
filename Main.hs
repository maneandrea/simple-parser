module Main where

import System.Environment     (getArgs)
import System.IO              (stderr)
import Data.Maybe             (isJust)

import SimpleParser.Parser    (parseMain, parseTest)
import SimpleParser.ArgParser (Args (..), parseArgs, getParamValue, helpString)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Args (Left e)                       -> putStrLn $ "\ESC[91m\ESC[1mError\ESC[0m: " ++ show e
    Args (Right (p, _))
      | isJust $ getParamValue "help" p -> putStrLn helpString
      | isJust $ getParamValue "test" p -> putStrLn parseTest
      | otherwise                       -> putStr $ parseMain input where 
                                             Just [input] = getParamValue "INPUT" p
 