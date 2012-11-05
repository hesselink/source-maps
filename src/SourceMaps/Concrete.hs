{-# LANGUAGE TemplateHaskell #-}
module SourceMaps.Concrete (SourceMap (..), parse) where

import Data.Text (Text)
import Data.Aeson.TH (deriveJSON)
import Control.Monad.Error (throwError)
import Data.Aeson (json', fromJSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson           as Json
import qualified Data.Attoparsec.Lazy as A

import SourceMaps.Types (Result)

data SourceMap = SourceMap
  { version    :: Int
  , file       :: FilePath
  , lineCount  :: Int
  , sources    :: [FilePath]
  , names      :: [Text]
  , mappings   :: Text
  } deriving Show

deriveJSON id ''SourceMap

parse :: ByteString -> Result SourceMap
parse bs =
  case A.parse json' bs of
    A.Done _ v ->
      case fromJSON v of
        Json.Success a -> return a
        Json.Error   e -> throwError e
    A.Fail _ _ e -> throwError e
