{-# LANGUAGE DoAndIfThenElse #-}
module Base64VLQ (encodeBase64VLQ, decodeBase64VLQ, decodeBase64VLQState) where

import Control.Applicative
import Control.Monad.Error (throwError)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans (lift)
import Data.Bits
import Data.Char (ord, chr)
import Data.Text (Text)
import qualified Data.Text as Text

import SourceMaps.Types

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

encodeBase64VLQ :: Int -> Text
encodeBase64VLQ = go . toVLQSigned
  where
    go vlq =
      if done
      then Text.singleton c
      else c `Text.cons` go rest
      where
        digit = (vlq .&. baseMask) .|. if done then 0 else continuationBit
        rest  = vlq `shiftR` baseShift
        done  = rest == 0
        c     = toBase64 digit

fromBase64 :: Char -> Result Int
fromBase64 c | c >= 'A' && c <= 'Z' = pure (ord c - 65)
             | c >= 'a' && c <= 'z' = pure (ord c - 71)
             | c >= '0' && c <= '9' = pure (ord c + 4)
             | c == '+'             = pure 62
             | c == '/'             = pure 63
             | otherwise            = throwError $ "Incorrect character in fromBase64: " ++ [c]

toBase64 :: Int -> Char
toBase64 i | i >= 0  && i <= 25 = chr (i + 65)
           | i >= 26 && i <= 51 = chr (i + 71)
           | i >= 52 && i <= 61 = chr (i - 4)
           | i == 62            = '+'
           | i == 63            = '/'
           | otherwise          = error $ "Incorrect integer in toBase64: " ++ show i

isContinuation :: Int -> Bool
isContinuation digit = digit .&. continuationBit > 0

fromVLQSigned :: Int -> Int
fromVLQSigned i =
  let negative = i .&. 1 == 1
      result = i `shiftR` 1
  in  if negative then -result else result

toVLQSigned :: Int -> Int
toVLQSigned i =
  abs i `shiftL` 1 + if i < 0 then 1 else 0

baseShift :: Int
baseShift = 5

continuationBit :: Int
continuationBit = 1 `shiftL` baseShift

baseMask :: Int
baseMask = continuationBit - 1
