module SourceMaps.Types
  ( SourceMap(..)

  , Mapping(GeneratedCode, OriginalMapping)
  , generatedLocation
  , originalFile
  , originalLocation
  , originalName

  , Location(..)

  , Result
  ) where

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
    , _originalFile      :: FilePath
    , _originalLocation  :: Location
    , _originalName      :: Maybe Text
    }
  deriving Show

data Location = Location { line :: Int, column :: Int } deriving Show
originalFile :: Mapping -> Maybe FilePath
originalFile m@OriginalMapping{} = Just (_originalFile m)
originalFile _                   = Nothing

originalLocation :: Mapping -> Maybe Location
originalLocation m@OriginalMapping{} = Just (_originalLocation m)
originalLocation _                   = Nothing

originalName :: Mapping -> Maybe Text
originalName m@OriginalMapping{} = _originalName m
originalName _                   = Nothing


type Result = Either String
