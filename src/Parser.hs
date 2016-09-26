{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import Text.Megaparsec
import VariationalCompiler.Parser
import VariationalCompiler.Json
import Data.Aeson
import Data.ByteString.Lazy.Char8(ByteString, putStrLn)
import Prelude hiding (putStrLn)
import Data.Text

-- Aeson instances for Megaparsec

-- | Allows for custom encoding behavior of Either values.
newtype Either' a b = E (Either a b)

-- | Instance allows us to customize how either is converted to JSON
instance (ToJSON a, ToJSON b) => ToJSON (Either' a b) where
  toJSON (E (Left a)) = toJSON a
  toJSON (E (Right b)) = toJSON b

instance ToJSON Dec where
  toJSON d = String $ pack $ show d

-- | Read concrete type from stdin and then parse into simple
--   abstract syntax tree. The ast or errors from parsing are
--   converted to JSON and passed to stdout.
main :: IO ()
main = do
  maybeTree <- runParser program "stdin" <$> getContents
  putStrLn (either encode encode maybeTree)
  case maybeTree of
    Right _ -> exitSuccess
    Left _ -> exitWith (ExitFailure 1)
