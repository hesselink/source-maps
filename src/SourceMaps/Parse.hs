{-# LANGUAGE TemplateHaskell, OverloadedStrings, DoAndIfThenElse #-}
module SourceMaps.Parse (module SourceMaps.Types, parse) where

import Control.Applicative
import Control.Monad.Error (throwError)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)
import Data.Label (mkLabel)
import Data.Text (Text)
import Data.Traversable
import Safe
import qualified Data.Label           as L
import qualified Data.Label.PureM     as S
import qualified Data.Text            as Text

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

type Worker = StateT WorkerState Result

parse :: Concrete.SourceMap -> Result SourceMap
parse sm = SourceMap (Concrete.file sm) <$> evalStateT (parseMappings (Concrete.sources sm) (Concrete.names sm) (Concrete.mappings sm)) (WorkerState 0 0 0 1 0)

parseMappings :: [FilePath] -> [Text] -> Text -> Worker [Mapping]
parseMappings sources names = fmap concat . traverse (parseMapping sources names) . zip [1..] . map (filter (not . Text.null) . Text.splitOn ",") . Text.splitOn ";"

parseMapping :: [FilePath] -> [Text] -> (Int, [Text]) -> Worker [Mapping]
parseMapping sources names (ln, segments) =
  do S.puts prevGenColumn 0
     traverse (parseSegment sources names ln) segments

parseSegment :: [FilePath] -> [Text] -> Int -> Text -> Worker Mapping
parseSegment sources names genLine segment =
  do (genCol, os) <- lift (decodeSegment segment)
     S.modify prevGenColumn (+ genCol)
     case os of
       Just (srcOffset, ln, origCol, mNameOffset) ->
         do S.modify sourceOffset   (+ srcOffset)
            S.modify nameOffset     (+ (fromMaybe 0 mNameOffset))
            S.modify prevOrigLine   (+ ln)
            S.modify prevOrigColumn (+ origCol)
            st <- get
            source <- find "Source file" sources (L.get sourceOffset st)
            mName  <- forM mNameOffset (const $ find "Name" names (L.get nameOffset st))
            return $ OriginalMapping (Location genLine (L.get prevGenColumn st)) source (Location (L.get prevOrigLine st) (L.get prevOrigColumn st)) mName
       Nothing ->
         do newGenCol <- S.gets prevGenColumn
            return (GeneratedCode (Location genLine newGenCol))

find :: String -> [a] -> Int -> Worker a
find thing names nameIdx =
  maybe
    (throwError $ thing ++ " not found at index: " ++ show nameIdx)
    return
    (names `atMay` nameIdx)

decodeSegment :: Text -> Result (Int, Maybe (Int, Int, Int, Maybe Int))
decodeSegment str = flip evalStateT str $
  do genCol <- decodeBase64VLQState
     empty0 <- isEmpty
     if empty0
     then return (genCol, Nothing)
     else do
     src     <- decodeBase64VLQState
     ln      <- decodeBase64VLQState
     origCol <- decodeBase64VLQState
     empty1 <- isEmpty
     nm <- if empty1 then return Nothing else Just <$> decodeBase64VLQState
     return (genCol, Just (src, ln, origCol, nm))

isEmpty :: StateT Text Result Bool
isEmpty = Text.null <$> get
