-- | Here is an example GET request that streams the response body to standard
--   output:
--
-- > import Pipes
-- > import Pipes.HTTP
-- > import qualified Pipes.ByteString as PB
-- >
-- > main = do
-- >     req <- parseUrl "https://www.example.com"
-- >     withManager tlsManagerSettings $ \m ->
-- >         withHTTP req m $ \resp ->
-- >             runEffect $ responseBody resp >-> PB.stdout
--
--   Here is an example POST request that also streams the request body from
--   standard input:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Pipes
-- > import Pipes.HTTP
-- > import qualified Pipes.ByteString as PB
-- >
-- > main = do
-- >     req <- parseUrl "https://www.example.com"
-- >     let req' = req
-- >             { method = "POST"
-- >             , requestBody = stream PB.stdin
-- >             }
-- >     withManager tlsManagerSettings $ \m ->
-- >         withHTTP req' m $ \resp ->
-- >             runEffect $ responseBody resp >-> PB.stdout


module Pipes.HTTP (
    -- * http-client
    -- $httpclient
      module Network.HTTP.Client
    , module Network.HTTP.Client.TLS

    -- * Pipes Interface
    , withHTTP
    , streamN
    , stream

    ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Pipes

{- $httpclient
    This module is a thin @pipes@ wrapper around the @http-client@ and
    @http-client-tls@ libraries.

    Read the documentation in the "Network.HTTP.Client" module of the
    @http-client@ library to learn about how to:

    * manage connections using connection pooling,

    * use more advanced request\/response features,

    * handle exceptions, and:

    * manage cookies.

    @http-client-tls@ provides support for TLS connections (i.e. HTTPS).
-}

-- | Send an HTTP 'Request' and wait for an HTTP 'Response'
withHTTP
    :: Request
    -- ^
    -> Manager
    -- ^
    -> (Response (Producer ByteString IO ()) -> IO a)
    -- ^ Handler for response
    -> IO a
withHTTP r m k = withResponse r m k'
  where
    k' resp = do
        let p = (from . brRead . responseBody) resp
        k (resp { responseBody = p})
{-# INLINABLE withHTTP #-}

-- | Create a 'RequestBody' from a content length and 'Producer'
streamN :: Int64 -> Producer ByteString IO () -> RequestBody
streamN n p = RequestBodyStream n (to p)
{-# INLINABLE streamN #-}

{-| Create a 'RequestBody' from a 'Producer'

    'stream' is more flexible than 'streamN', but requires the server to support
    chunked transfer encoding.
-}
stream :: Producer ByteString IO () -> RequestBody
stream p = RequestBodyStreamChunked (to p)
{-# INLINABLE stream #-}

to :: Producer ByteString IO () -> (IO ByteString -> IO ()) -> IO ()
to p0 k = do
    ioref <- newIORef p0
    let readAction :: IO ByteString
        readAction = do
            p <- readIORef ioref
            x <- next p
            case x of
                Left   ()      -> do
                    writeIORef ioref (return ())
                    return B.empty
                Right (bs, p') -> do
                    writeIORef ioref p'
                    return bs
    k readAction 

from :: IO ByteString -> Producer ByteString IO ()
from io = go
  where
    go = do
        bs <- lift io
        unless (B.null bs) $ do
            yield bs
            go 
