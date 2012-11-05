{-# LANGUAGE TemplateHaskell #-}
module SourceMaps.Concrete (SourceMap (..)) where

import Data.Text (Text)
import Data.Aeson.TH (deriveJSON)

data SourceMap = SourceMap
  { version    :: Int
  , file       :: FilePath
  , lineCount  :: Int
  , sources    :: [FilePath]
  , names      :: [Text]
  , mappings   :: Text
  } deriving Show

deriveJSON id ''SourceMap
