{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, TemplateHaskell #-}
module SourceMaps where

import Control.Applicative
import Control.Monad.Error (throwError)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Bits
import Data.Char (ord)
import Data.Maybe (fromJust, fromMaybe)
import Data.Label (mkLabel)
import Data.Text (Text)
import Data.Traversable
import Safe
import qualified Data.Aeson           as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Label           as L
import qualified Data.Label.PureM     as S
import qualified Data.Text            as Text

import qualified SourceMaps.Concrete as Concrete

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

decodeBase64VLQState :: StateT Text Result Int
decodeBase64VLQState =
  do t <- get
     (i, t') <- lift (decodeBase64VLQ t)
     put t'
     return i

decodeBase64VLQ :: Text -> Result (Int, Text)
decodeBase64VLQ = go 0 0
  where
    go result shft str =
      case Text.uncons str of
        Just (c, rest) ->
          do digit <- fromBase64 c
             let newResult = result + (digit .&. baseMask) `shiftL` shft
             if isContinuation digit
             then go newResult (shft + baseShift) rest
             else return (fromVLQSigned newResult, rest)
        Nothing   -> throwError "Insufficient bits in decodeBase64VLQ." -- TODO: show original string

fromBase64 :: Char -> Result Int
fromBase64 c | c >= 'A' && c <= 'Z' = pure (ord c - 65)
             | c >= 'a' && c <= 'z' = pure (ord c - 71)
             | c >= '0' && c <= '9' = pure (ord c + 4)
             | c == '+'             = pure 62
             | c == '/'             = pure 63
             | otherwise            = throwError $ "Incorrect character in fromBase64: " ++ [c]

isContinuation :: Int -> Bool
isContinuation digit = digit .&. continuationBit > 0

fromVLQSigned :: Int -> Int
fromVLQSigned i =
  let negative = i .&. 1 == 1
      result = i `shiftR` 1
  in  if negative then -result else result

baseShift :: Int
baseShift = 5

continuationBit :: Int
continuationBit = 1 `shiftL` baseShift

baseMask :: Int
baseMask = continuationBit - 1

{-
readJson :: Text -> Json -- external
parse :: Json -> SourceMap
merge :: SourceMap -> SourceMap -> SourceMap
print :: SourceMap -> Json
showJson :: Json -> Text -- external
-}

jsonFromFile :: FilePath -> IO (Result SourceMap)
jsonFromFile fp =
  do bs  <- LBS.readFile fp
     let csm = fromJust (Json.decode' bs)
     print csm
     return (parse csm)
