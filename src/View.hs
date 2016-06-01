{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import Data.Maybe
import Text.Megaparsec
import VariationalCompiler.Entities
import VariationalCompiler.Json
import Data.Aeson
import Data.ByteString.Lazy.Char8(ByteString, putStrLn, getContents,pack)
import Prelude hiding (putStrLn, getContents)

main :: IO ()
main = do
  rawInput <- getContents
  let prog = decode rawInput
  let i = dummy prog
  putStrLn (pack i)
  case Left 0 of
    Right _ -> exitSuccess
    Left _ -> exitWith (ExitFailure 1)
    where dummy :: Maybe Program -> String
          dummy = show . fromJust
