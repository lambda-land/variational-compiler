{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import Text.Megaparsec
import VariationalCompiler.Parser
import VariationalCompiler.Json
import Data.Aeson
import Data.ByteString.Lazy.Char8(ByteString, putStrLn)
import Prelude hiding (putStrLn)

-- Aeson instances for Megaparsec

-- | Allows for custom encoding behavior of Either values.
newtype Either' a b = E (Either a b)

instance (ToJSON a, ToJSON b) => ToJSON (Either' a b) where
  toJSON (E (Left a)) = toJSON a
  toJSON (E (Right b)) = toJSON b


main :: IO ()
main = do
  maybeTree <- runParser program "stdin" <$> getContents
  putStrLn (either encode encode maybeTree)
  case maybeTree of
    Right _ -> exitSuccess
    Left _ -> exitWith (ExitFailure 1)
