{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.HTTP.Req.Machine.Test (tests) where

import Control.Exception (catch, finally)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left, runEitherT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Default.Class (def)
import Data.Machine (source)
import Data.String (fromString)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Text.Printf (printf)

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as BS
import qualified Data.Streaming.Network as Network
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Req.Machine as Req
import qualified Network.Socket as Network
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types.Status as Http

newtype M a = M{ runM :: EitherT Req.HttpException (ReaderT Req.HttpConfig IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Req.MonadHttp M where
  handleHttpException = M . left
  getHttpConfig = M (lift ask)

tests :: TestTree
tests = withHttpServer $ \getUrl -> testGroup "ALL"
  [ test_chunked getUrl
  , test_knownLength getUrl
  ]

test_chunked :: IO (Req.Url 'Req.Http, Req.Option 'Req.Http) -> TestTree
test_chunked getUrlOpts = testCase "Chunked" $ do
  (url, opts) <- getUrlOpts
  let chunks = ["foo", "bar", "quux"]
  resp <- flip runReaderT def $ runEitherT $ runM $
    Req.req Req.POST url (Req.ReqBodySource Nothing (source chunks)) Req.bsResponse opts
  case resp of
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right r  -> do
      let expected = printf "ChunkedBody %s" (show chunks)
      assertEqual "" (fromString expected) (Req.responseBody r)

test_knownLength :: IO (Req.Url 'Req.Http, Req.Option 'Req.Http) -> TestTree
test_knownLength getUrlOpts = testCase "Known Length" $ do
  (url, opts) <- getUrlOpts
  let chunks = ["foo", "bar", "quux"]
      len = fromIntegral (BS.length (BS.concat chunks))
  resp <- flip runReaderT def $ runEitherT $ runM $
    Req.req Req.POST url (Req.ReqBodySource (Just len) (source chunks)) Req.bsResponse opts
  case resp of
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right r  -> do
      let expected = printf "KnownLength %d %s" len (show [BS.concat chunks])
      assertEqual "" (fromString expected) (Req.responseBody r)

withHttpServer :: (IO (Req.Url 'Req.Http, Req.Option 'Req.Http) -> TestTree) -> TestTree
withHttpServer k = withResource open close (k . fmap (\(urlopts,_,_,_) -> urlopts))
  where
    open = do
      (port, sock) <- Network.bindRandomPortTCP "localhost"
      pid <- Async.async (Warp.runSettingsSocket Warp.defaultSettings sock server)
      let Just (url, opts) = Req.parseUrlHttp ("http://localhost:" <> fromString (show port))
      return ((url, opts), port, sock, pid)
    close (_urlopts, _port, sock, pid) =
      (`finally` Network.close sock) $ do
        Async.cancel pid
        catch (Async.wait pid) $ \Async.AsyncCancelled -> return ()
    server req done = do
      let go bss = do
            bs <- Wai.requestBody req
            if BS.null bs
              then return bss
              else go (bss++[bs])
      bss <- go []
      let respBody = printf "%s %s" (show $ Wai.requestBodyLength req) (show bss)
      done $ Wai.responseLBS Http.status200 mempty (fromString respBody)
