-- |
-- Module      :  Network.HTTP.Req.Machine
-- Copyright   :  © 2018 Ben Sinclair
-- License     :  BSD 3 clause
--
-- Maintainer  :  Ben Sinclair <ben.d.sinclair@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module extends functionality available in "Network.HTTP.Req" with
-- Machines helpers for streaming big request and response bodies.

-- This was heavily based on the req-conduits package.

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Network.HTTP.Req.Machine
  ( ReqBodySource(..)
  , bodyReaderSource
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int64)
import Data.Machine (SourceT, Step(..), construct, runMachineT, yield)
import Network.HTTP.Req (HttpBody(getRequestBody))

import qualified Data.ByteString     as B
import qualified Network.HTTP.Client as L

----------------------------------------------------------------------------
-- Streaming request bodies

-- | This body option streams contents of request body from the given
-- 'M.SourceT'. The 'Int64' value may be the size of the data in bytes.  If the
-- size is not given then the request is sent as chunked data.
--
-- Using this body option does not set the @Content-Type@ header.

data ReqBodySource = ReqBodySource (Maybe Int64) (SourceT IO ByteString)

instance HttpBody ReqBodySource where
  getRequestBody (ReqBodySource (Just size) src) =
    L.RequestBodyStream size (srcToPopperIO src)
  getRequestBody (ReqBodySource Nothing src) =
    L.RequestBodyStreamChunked (srcToPopperIO src)

srcToPopperIO :: SourceT IO ByteString -> L.GivesPopper ()
srcToPopperIO src0 f = do
  v <- newIORef src0
  let popper :: IO ByteString
      popper = do
        src <- readIORef v
        x <- runMachineT src
        case x of
          Stop          -> return B.empty
          Yield bs src' -> writeIORef v src' >> if B.null bs then popper else return bs
          Await{}       -> error "impossible"
  f popper

bodyReaderSource :: MonadIO m => L.BodyReader -> SourceT m ByteString
bodyReaderSource br = construct go
  where
    go = do
      bs <- liftIO $ L.brRead br
      unless (B.null bs) (yield bs >> go)
