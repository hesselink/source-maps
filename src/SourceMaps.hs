{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, TemplateHaskell #-}
module SourceMaps (module SourceMaps.Types, parse, parseFile) where

import Control.Monad ((<=<))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

import SourceMaps.Types
import qualified SourceMaps.Concrete  as Concrete
import qualified SourceMaps.Parse     as SM


parseFile :: FilePath -> IO (Result SourceMap)
parseFile = fmap parse . LBS.readFile

parse :: ByteString -> Result SourceMap
parse = SM.parse <=< Concrete.parse
