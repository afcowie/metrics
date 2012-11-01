--
-- Statistics collection
--
-- Copyright © 2012 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made available
-- to you by its authors as open source software: you can redistribute it
-- and/or modify it under the terms of the GNU General Public License version
-- 2 ("GPL") as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GPL for more details.
--
-- You should have received a copy of the GPL along with this program. If not,
-- see http://www.gnu.org/licenses/. The authors of this program may be
-- contacted through http://research.operationaldynamics.com/
--

{-# LANGUAGE OverloadedStrings #-}

module HttpServer (site) where

import Prelude hiding (catch)

import Snap.Core
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (catch)
import Control.Exception (SomeException)
import System.IO (stderr, hFlush)

import ParseCommand

--
-- Top level URL routing logic.
--

site :: Snap ()
site = catch
        (routeRequests)
        (\e -> serveError "Splat\n" e)

routeRequests :: Snap ()
routeRequests =
    path "collectd" serveCollection
    <|> serveNotFound


serveCollection :: Snap ()
serveCollection = do
    r <- getRequest

    let m = rqMethod r
    case m of
        POST    -> handlePostMethod
        _       -> serveBadRequest


--
-- Handle inbound measurements
--


handlePostMethod :: Snap ()
handlePostMethod = do
    r <- getRequest
    let mm = getHeader "Content-Type" r

    case mm of
        Just "text/plain"        -> handleAsPlain
        Just "application/json" -> handleAsJSON
        Just x                  -> debug x
        _                       -> serveUnsupported


handleAsPlain :: Snap ()
handleAsPlain = do
    modifyResponse $ setResponseStatus 201 "Created"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setContentType "text/plain"
    
    bl <- readRequestBody 65535
    let b' = S.concat $ L.toChunks bl
    
    let Right m = processInput b'
    debug $ S.pack $ show m

--
-- Stub in case we switch to JSON format
--

handleAsJSON = undefined

--
-- Error responses. Terminate handling the request and flush to client.
--

terminate :: Snap a
terminate = do
    r <- getResponse
    finishWith r
    

serveNotFound :: Snap a
serveNotFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    modifyResponse $ setHeader "Content-Type" "text/plain"
    writeBS "400 Not Found\n"
    terminate


serveBadRequest :: Snap ()
serveBadRequest = do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    writeBS "400 Bad Request\n"
    terminate


serveUnsupported :: Snap ()
serveUnsupported = do
    modifyResponse $ setResponseStatus 415 "Unsupported Media Type"
    writeBS "415 Unsupported Media Type\n"
    terminate


--
-- The exception will be dumped to the server's stdout, while the supplied
-- message will be sent out with the response (ideally only for debugging
-- purposes, but easier than looking in log/error.log for details). 
--

serveError :: ByteString -> SomeException -> Snap ()
serveError x' e = do
    debug m'
    modifyResponse $ setResponseStatus 500 "Internal Server Error"
    writeBS x'
    terminate
  where
    m' = S.pack $ show (e :: SomeException)


debug :: ByteString -> Snap ()
debug cs = do
    liftIO $ do
        S.hPutStrLn stderr ""
        S.hPutStrLn stderr cs
        hFlush stderr
