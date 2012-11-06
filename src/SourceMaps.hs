{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, TemplateHaskell #-}
module SourceMaps
  ( module SourceMaps.Types

  , parse
  , parseFile
  , generate
  , generateFile
  , merge
  ) where

import Control.Monad ((<=<))
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as LBS

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

merge :: [SourceMap] -> SourceMap -> SourceMap
merge sources final = final { mappings = map mp (mappings final) }
  where
    srcs = map (\m -> (outputFile m, mappings m)) sources
    mp gc@GeneratedCode{}                          = gc
    mp (OriginalMapping genLoc origFile origLoc _) =
      let sms = fromMaybe
                  (error "merge: final file refers to file that isn't in sources")
                  (lookup origFile srcs)
          sm  = last [m | m <- sms, generatedLocation m <= origLoc]
      in sm { generatedLocation = genLoc }
