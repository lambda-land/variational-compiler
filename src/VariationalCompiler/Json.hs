{-# LANGUAGE OverloadedStrings #-}

module VariationalCompiler.Json where

import VariationalCompiler.Entities
import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad
import Data.Functor.Identity
import Text.Megaparsec
import Data.Aeson
import Data.Text
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM

instance ToJSON Pos where
   toJSON = toJSON . unPos

instance ToJSON SourcePos where
  toJSON spos = String $ pack $ show spos

instance ToJSON Dec where
  toJSON d = String $ pack $ show d

instance (ToJSON t) => ToJSON (ErrorItem t)

instance (ToJSON t, ToJSON e) => ToJSON (ParseError t e)

instance ToJSON Span
instance ToJSON Choice
instance ToJSON Content

addTag :: Text -> Value -> Value
addTag t (Object o) = Object $ HM.insert "type" (String t) o
addTag t v = v

instance ToJSON Segment where
    toJSON (ChoiceSeg co) = addTag "choice" (genericToJSON defaultOptions co)
    toJSON (ContentSeg ch) = addTag "text" (genericToJSON defaultOptions ch)

    -- toEncoding avoids all the memory allocation from creating Values and seems faster but
    -- probably will need to migrate some of the records to use sum types again for it
    --toEncoding (ChoiceSeg co) = addTag "choice" (genericToEncoding defaultOptions co)
    --toEncoding (ContentSeg ch) = addTag "text" (genericToEncoding defaultOptions ch)

instance ToJSON Alternative where
    toJSON LeftBranch = "left"
    toJSON RightBranch = "right"

instance FromJSON Span
instance FromJSON Choice
instance FromJSON Content
instance FromJSON Segment where
    parseJSON (Object o) = case HM.lookup "type" o of
      Just (String "choice") -> ChoiceSeg <$> (parseJSON (Object o) :: Parser Choice)
      Just (String "text") -> ContentSeg <$> (parseJSON (Object o) :: Parser Content)
      Just v -> typeMismatch "Only objects containing a type field with the value \"choice\" or \"text\" can decode into a Segment" v
      Nothing -> fail "Expected an object with a \"type\" field with the value \"choice\" or \"text\""

instance FromJSON Alternative where
    parseJSON (String "left") = return LeftBranch
    parseJSON (String "right") = return RightBranch
    parseJSON v = typeMismatch "Only the strings \"left\" or \"right\" can decode into an Alternative value." v

instance FromJSON Projection where
    parseJSON = genericParseJSON defaultOptions
instance FromJSON Selection where
    parseJSON = genericParseJSON defaultOptions
