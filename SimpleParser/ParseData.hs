module SimpleParser.ParseData
    ( ParseData (..)
    , prependParsed
    ) where
 
data ParseData i a = ParseData
    { parsed   :: [i]
    , struct   :: a
    , unparsed :: [i]
    }

parstr :: String
parstr = "\ESC[32m\ESC[1mParsed\ESC[0m:      "
resstr :: String
resstr = "\ESC[34m\ESC[1mParseResult\ESC[0m:\n"
unpstr :: String
unpstr = "\ESC[93m\ESC[1mUnparsed\ESC[0m:    "

instance (Show a, Show i) => Show (ParseData i a) where
    show ParseData
      { parsed   = p
      , struct   = s
      , unparsed = u
      } = parstr ++ show p ++ "\n" ++ unpstr ++ show u ++ "\n" ++ resstr ++ show s ++ "\n"

prependParsed :: [i] -> ParseData i a -> ParseData i a
prependParsed s (ParseData p a u) = ParseData (s++p) a u