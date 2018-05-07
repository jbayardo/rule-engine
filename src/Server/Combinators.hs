{-# LANGUAGE RankNTypes #-}

module Server.Combinators where

import qualified Conduit as C
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.Extra (
  newBarrier,
  myThreadId,
  forkFinally,
  throwTo,
  signalBarrier,
  waitBarrier)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Monad.Extra (whileM, whenJust)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)

-- Due to Neil Mitchell. See his post on this operator from: http://neilmitchell.blogspot.com.ar/2015/07/parallelpipelined-conduit.html
pipelineC ::
     Int -> (forall t. C.ConduitT o t IO r) -> (forall t. C.ConduitT o t IO r)
pipelineC buffer sink = do
  sem <- liftIO $ newQSem buffer -- how many are in flow, to avoid excess memory
  chan <- liftIO newChan -- the items in flow (type o)
  bar <- liftIO newBarrier -- the result type (type r)
  me <- liftIO myThreadId
  liftIO $
    forkFinally
      (C.runConduit $
       whileM
          (do x <- liftIO $ readChan chan
              liftIO $ signalQSem sem
              whenJust x C.yield
              return $ isJust x) C..|
       sink)
      (either (throwTo me) (signalBarrier bar))
  C.awaitForever $ \x ->
    liftIO $ do
      waitQSem sem
      writeChan chan $ Just x
  liftIO $ writeChan chan Nothing
  liftIO $ waitBarrier bar
