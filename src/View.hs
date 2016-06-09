{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import Data.Maybe
import Text.Megaparsec
import VariationalCompiler.Entities
import VariationalCompiler.Json
import VariationalCompiler.View
import Data.Aeson
import Data.ByteString.Lazy.Char8(ByteString, putStrLn, getContents, pack)
import Prelude hiding (putStrLn, getContents)


-- | Decode input data from stdin, the parsed ast and 
--   the selection, and use that info to generate a 
--   call to getView. Encode the result of the view 
--   in JSON and return it to stdout.
main :: IO ()
main = do
  rawInput <- getContents
  either (\s -> putStrLn (pack s) >> exitWith (ExitFailure 1)) (\(p,sel) -> putStrLn (encode (getView sel p))) $ eitherDecode rawInput 
