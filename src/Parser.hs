{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import Text.Megaparsec
import VariationalCompiler.Parser
import VariationalCompiler.Json
import Data.Aeson
import Data.ByteString.Lazy.Char8(ByteString, putStrLn)
import Prelude hiding (putStrLn)
import Data.Text

-- | Read concrete type from stdin and then parse into simple
--   abstract syntax tree. The ast or errors from parsing are
--   converted to JSON and passed to stdout.
main :: IO ()
main = do
  maybeTree <- runParser program "stdin" <$> getContents
  putStrLn (either encode encode maybeTree)
  case maybeTree of
    Left _ -> exitSuccess
    Right _ -> exitWith (ExitFailure 1)
