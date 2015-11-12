{-# LANGUAGE OverloadedStrings #-}

module Data.Zipkin.Types where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (maybeToList)
import Data.String (IsString)
import Data.Word (Word64)
import Numeric (showHex, readHex)
import Safe (readMay)
import System.Random.Mersenne.Pure64

newtype Identifier = Identifier { toWord64 :: Word64 }

instance Show Identifier where
  show (Identifier x) = let hex = showHex x "" in replicate (16 - length hex) '0' ++ hex

instance Read Identifier where
  readsPrec _ = fmap (fmap (\(x, s) -> (Identifier x, s))) readHex

parseIdentifier :: String -> Maybe Identifier
parseIdentifier = readMay

data TraceInfo = TraceInfo
  { traceId :: Identifier
  , spanId :: Identifier
  , parentSpanId :: Maybe Identifier
  }

hashTraceInfo :: TraceInfo -> Word64
hashTraceInfo (TraceInfo traceId spanId parentSpanId) = maybe h (xor h . toWord64) parentSpanId
  where h = toWord64 traceId `xor` toWord64 spanId

fromHeaders :: (Eq s, IsString s) => [(s, ByteString)] -> Maybe TraceInfo
fromHeaders hs = do
  traceId <- lookup "X-B3-TraceId" hs >>= parseIdentifier . unpack
  spanId <- lookup "X-B3-SpanId" hs >>= parseIdentifier . unpack
  let parentSpanId = lookup "X-B3-ParentSpanId" hs >>= parseIdentifier . unpack
  return $ TraceInfo traceId spanId parentSpanId

toHeaders :: IsString s => TraceInfo -> [(s, ByteString)]
toHeaders (TraceInfo traceId spanId parentSpanId) =
  [ Just ("X-B3-TraceId", pack (show traceId))
  , Just ("X-B3-SpanId", pack (show spanId))
  , (\x -> ("X-B3-ParentSpanId", pack (show x))) <$> parentSpanId
  ] >>= maybeToList
