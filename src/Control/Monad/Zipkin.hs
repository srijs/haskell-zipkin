{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Zipkin
  ( Identifier, parseIdentifier
  , TraceInfo(..), fromHeaders, toHeaders, newTraceInfo
  , TraceT, getTraceInfo, forkTraceInfo, runTraceT
  ) where

import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64

import Data.Zipkin.Types
import qualified Data.Zipkin.Context as Ctx

newtype TraceT m a = TraceT { run :: StateT Ctx.TraceContext m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

newTraceInfo :: IO TraceInfo
newTraceInfo = evalState Ctx.newTraceInfo <$> newPureMT

getTraceInfo :: Monad m => TraceT m TraceInfo
getTraceInfo = TraceT Ctx.getTraceInfo

forkTraceInfo :: Monad m => TraceT m TraceInfo
forkTraceInfo = TraceT Ctx.forkTraceInfo

runTraceT :: Monad m => TraceT m a -> TraceInfo -> m a
runTraceT m = evalStateT (run m) . Ctx.mkContext
