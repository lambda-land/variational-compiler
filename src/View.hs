{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import Data.Maybe
import Text.Megaparsec
import VariationalCompiler.Entities
import VariationalCompiler.Json
import VariationalCompiler.View
import Data.Aeson
import Data.ByteString.Lazy.Char8(ByteString, putStrLn, getContents,pack)
import Prelude hiding (putStrLn, getContents)

main :: IO ()
main = do
  rawInput <- getContents
  let n = decode rawInput >>= \(p,sel) -> return $ getView sel p
  case n of
    (Just a) -> putStrLn (encode a)
    Nothing  -> exitWith (ExitFailure 1)
