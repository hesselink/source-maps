{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, TemplateHaskell #-}
module SourceMaps
  ( module SourceMaps.Types

  , parse
  , parseFile
  , generate
  , generateFile
  , merge
  ) where

import Control.Monad.Error
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortBy, find)
import Data.Ord (comparing)
import Safe

import SourceMaps.Types
import qualified SourceMaps.Concrete  as Concrete
import qualified SourceMaps.Generate  as G
import qualified SourceMaps.Parse     as SM


parseFile :: FilePath -> IO (Result SourceMap)
parseFile = fmap parse . LBS.readFile

parse :: ByteString -> Result SourceMap
parse = SM.parse <=< Concrete.parse

generateFile :: FilePath -> SourceMap -> IO ()
generateFile fp sm = LBS.writeFile fp (generate sm)

generate :: SourceMap -> ByteString
generate = Concrete.generate . G.generate

merge :: [SourceMap] -> SourceMap -> Result SourceMap
merge sources final =
  do newMappings <- mapM mp (mappings final)
     return final { mappings = newMappings }
  where
    mp gc@GeneratedCode{}                          = return gc
    mp (OriginalMapping genLoc origFile origLoc _) =
      do sms <- maybe
                  (throwError "merge: final file refers to file that isn't in sources")
                  return
                  (find ((origFile ==) . outputFile) sources)
         sm  <- maybe (throwError $ "Location not found in merge: " ++ show origLoc ++ " from " ++ origFile) return (lookupLocation origLoc sms)
         return sm { generatedLocation = genLoc }

lookupLocation :: Location -> SourceMap -> Maybe Mapping
lookupLocation loc sm =
  let sortedMappings = sortBy (flip $ comparing generatedLocation) (mappings sm)
  in  headMay [m | m <- sortedMappings, generatedLocation m <= loc]
