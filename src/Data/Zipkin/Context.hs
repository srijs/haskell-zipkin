{-# LANGUAGE FlexibleContexts #-}

module Data.Zipkin.Context where

import Data.Bits (xor)
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64

import Data.Zipkin.Types

data TraceContext = TraceContext
  { traceInfo :: TraceInfo
  , traceGen :: PureMT
  }

mkContext :: TraceInfo -> TraceContext
mkContext info = TraceContext
  { traceInfo = info
  , traceGen = pureMT $ hashTraceInfo info
  }

getTraceInfo :: MonadState TraceContext m => m TraceInfo
getTraceInfo = gets traceInfo

newTraceInfo :: MonadState PureMT m => m TraceInfo
newTraceInfo = do
  traceId <- Identifier <$> state randomWord64
  spanId <- Identifier <$> state randomWord64
  return $ TraceInfo traceId spanId Nothing

forkTraceInfo :: MonadState TraceContext m => m TraceInfo
forkTraceInfo = do
  (TraceContext info gen) <- get
  let (newId, gen') = randomWord64 gen
  put (TraceContext info gen')
  return $ TraceInfo (traceId info) (Identifier newId) (Just (spanId info))
