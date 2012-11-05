{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}
module SourceMaps.Generate (module SourceMaps.Types, generate) where

import Control.Applicative
import Control.Monad.State (evalState, State)
import Data.Function
import Data.List (sortBy, groupBy, nub, elemIndex)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (mconcat)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Traversable (forM)
import Safe
import Data.Label (mkLabel, (:->))
import qualified Data.Label.PureM     as S
import qualified Data.Text as Text

import Base64VLQ
import SourceMaps.Types
import qualified SourceMaps.Concrete as Concrete

data WorkerState = WorkerState
  { _sourceOffset   :: Int
  , _nameOffset     :: Int
  , _prevGenColumn  :: Int
  , _prevOrigLine   :: Int
  , _prevOrigColumn :: Int
  }

mkLabel ''WorkerState

type Worker = State WorkerState

generate :: SourceMap -> Concrete.SourceMap
generate sm =
  Concrete.SourceMap
  { Concrete.version   = 3
  , Concrete.file      = outputFile sm
  , Concrete.lineCount = lastDef 0 . map (line . generatedLocation) $ sortedMappings
  , Concrete.sources   = sources
  , Concrete.names     = names
  , Concrete.mappings  = generateMappings sources names sortedMappings
  }
  where
    sortedMappings = sortBy (comparing generatedLocation) (mappings sm)
    sources        = nub . mapMaybe originalFile $ sortedMappings
    names          = nub . mapMaybe originalName $ sortedMappings

generateMappings :: [FilePath] -> [Text] -> [Mapping] -> Text
generateMappings sources names ms = evalState (Text.intercalate ";" <$> mapM (generateSegments sources names) grouped) (WorkerState 0 0 0 1 0)
  where
    grouped = groupBy ((==) `on` (line . generatedLocation)) ms

generateSegments :: [FilePath] -> [Text] -> [Mapping] -> Worker Text
generateSegments sources names ms =
  do S.puts prevGenColumn 0
     Text.intercalate "," <$> mapM (generateSegment sources names) ms

generateSegment :: [FilePath] -> [Text] -> Mapping -> Worker Text
generateSegment _       _     (GeneratedCode loc) = encodeInt (column loc) prevGenColumn
generateSegment sources names (OriginalMapping genLoc origFile origLoc mOrigName) = mconcat <$> sequence
  [ encodeInt (column genLoc) prevGenColumn
  , encodeIndex "source" sources sourceOffset origFile
  , encodeInt (line origLoc) prevOrigLine
  , encodeInt (column origLoc) prevOrigColumn
  , fromMaybe "" <$> forM mOrigName (encodeIndex "name" names nameOffset)
  ]

encodeIndex :: Eq a => String -> [a] -> (WorkerState :-> Int) -> a -> Worker Text
encodeIndex thing vals stateField val =
 let idx = fromMaybe (error $ "Impossible: " ++ thing ++ " not found in " ++ thing ++ "s list.") (elemIndex val vals)
 in  encodeInt idx stateField

encodeInt :: Int -> (WorkerState :-> Int) -> Worker Text
encodeInt val stateField =
  do prev <- S.gets stateField
     S.puts stateField val
     return $ encodeBase64VLQ (val - prev)
