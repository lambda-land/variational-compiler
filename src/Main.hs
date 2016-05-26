{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import Text.Megaparsec
import VariationalJava
import Data.Aeson
import Data.ByteString.Lazy.Char8(ByteString, putStrLn)
import Prelude hiding (putStrLn)

-- Aeson instances for Megaparsec

instance ToJSON SourcePos where
  toJSON pos = object [
      "sourceName" .= sourceName pos
    , "sourceLine" .= sourceLine pos
    , "sourceColumn" .= sourceColumn pos
    ]

instance ToJSON Message where
  toJSON = toJSON . messageString

instance ToJSON ParseError where
  toJSON err = object [
      "errorPos" .= errorPos err,
      "errorMessages" .= errorMessages err
    ]

-- | Allows for custom encoding behavior of Either values.
newtype Either' a b = E (Either a b)

instance (ToJSON a, ToJSON b) => ToJSON (Either' a b) where
  toJSON (E (Left a)) = toJSON a
  toJSON (E (Right b)) = toJSON b


main :: IO ()
main = do
  maybeTree <- runParser vjProgram "stdin" <$> getContents
  putStrLn (encode (E maybeTree))
  case maybeTree of
    Right _ -> exitSuccess
    Left _ -> exitWith (ExitFailure 1)
