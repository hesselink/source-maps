module SourceMaps.Types (SourceMap(..), Mapping(..), Location(..), Result) where

import Data.Text (Text)

data SourceMap = SourceMap
  { outputFile :: FilePath
  , mappings :: [Mapping]
  } deriving Show

data Mapping
  = GeneratedCode
    { generatedLocation :: Location }
  | OriginalMapping
    { generatedLocation :: Location
    , originalFile      :: FilePath
    , originalLocation  :: Location
    , originalName      :: Maybe Text
    }
  deriving Show

data Location = Location { line :: Int, column :: Int } deriving Show

type Result = Either String
