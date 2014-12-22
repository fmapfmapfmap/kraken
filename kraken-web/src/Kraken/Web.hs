{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Kraken.Web where


import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Monad                  (when)
import           Control.Monad.Trans.Either     hiding (left)
import           Data.Aeson                     (FromJSON(..), ToJSON(..))
import           Data.ByteString                (ByteString, hGetContents)
import           Data.Either                    (lefts, rights)
import           Data.Graph.Wrapper
import           Data.Maybe
import           Data.List                      (intercalate)
import           Data.Proxy
import           Data.String.Conversions
import           GHC.Generics                   (Generic)
import           Network.HTTP.Types
import           Network.Wai                    as Wai
import           Network.Wai.Handler.Warp.Run
import           Servant
import           Servant.Client
import           Servant.Docs
import           System.Exit
import           System.IO
import           System.Process                 (CreateProcess (..),
                                                 StdStream (..), createProcess,
                                                 proc, waitForProcess)
import qualified Kraken.Daemon                  as Daemon
import           Kraken.Dot
import           Kraken.TargetGraph

import           Kraken.Web.Config


run :: IO ()
run = do
  config <- loadConfig
  documentRoot <- getDocumentRoot
  runWarp (Kraken.Web.Config.port config) (application documentRoot (krakenUris config))

application :: FilePath -> [BaseUrl] -> Application
application documentRoot krakenUris =
  serve webApi $
  server documentRoot krakenUris

-- * API

type WebApi =
       "targetGraph.pdf" :> Raw
  :<|> "targetGraph.dot" :> Raw
  :<|> "docs" :> Get Documentation
  :<|> Raw

webApi :: Proxy WebApi
webApi = Proxy

server :: FilePath -> [BaseUrl] -> Server WebApi
server documentRoot krakenUris =
       targetGraph krakenUris Pdf
  :<|> targetGraph krakenUris Dot
  :<|> serveDocs webApi
  :<|> serveDirectory documentRoot

data FileFormat
  = Dot
  | Pdf

type ServantResponse a = EitherT (Int, String) IO a

serveDocs :: HasDocs layout => Proxy layout -> ServantResponse Documentation
serveDocs api = return . Documentation $ cs . markdown $ docs api

newtype Documentation = Documentation String
    deriving (Show, Eq, Generic)

instance ToJSON Documentation
instance FromJSON Documentation

instance ToSample Documentation where
  toSample = return $ Documentation "Some documentation"

-- * Graphs

targetGraph :: [BaseUrl] -> FileFormat -> Application
targetGraph krakenUris fileFormat request respond = do
  let prefixes =
        (\ xs -> if null xs then Nothing else Just xs) $
        map cs $
        catMaybes $
        map snd $
        filter ((== "prefix") . fst) $
        Wai.queryString request
  graphPartsEither <- sequence $ runEitherT . getTargetGraph <$> krakenUris
  --- Add the erroring uris to the error message
  let graphPartsWithErr = zipWith (\x -> ((\y -> show x ++ ":\n" ++ y) +++ id))
          krakenUris graphPartsEither
  let krakenUrisErr str = respond $ responseLBS status503
          [("Content-Type", "text/plain")] (cs str)
  case lefts graphPartsWithErr of
      xs@(_:_) -> krakenUrisErr $ intercalate "\n\n" xs
      [] -> do
        let graphParts = rights graphPartsEither
        let dot = Kraken.Web.toDot prefixes $ mergeGraphs graphParts
        case fileFormat of
          Pdf -> do
            (exitCode, pdf) <- readProcess "dot" ["-Tpdf"] dot
            when (exitCode /= ExitSuccess) $
              throwIO (ErrorCall ("dot exited with: " ++ show exitCode))
            respond $ responseLBS ok200 [("Content-Type", "application/pdf")] (cs pdf)
          Dot ->
            respond $ responseLBS ok200 [("Content-Type", "text/vnd.graphviz")] (cs dot)

mergeGraphs :: [TargetGraph] -> TargetGraph
mergeGraphs graphs = TargetGraph $ fromListLenient $
  concatMap (\ (TargetGraph g) -> toList g) graphs

toDot :: Maybe [String] -> TargetGraph -> String
toDot prefixes (TargetGraph g) = Kraken.Dot.toDot False prefixes True (fmap Kraken.Dot.fromWebNode g)


-- * daemon api

getTargetGraph :: BaseUrl -> EitherT String IO TargetGraph
(     getTargetGraph
 :<|> _)
    = client Daemon.daemonApi


-- * utils

readProcess :: FilePath -> [String] -> String -> IO (ExitCode, ByteString)
readProcess path args input = do
  let process = (proc path args){
        std_in = CreatePipe,
        std_out = CreatePipe
       }
  (Just std_in, Just std_out, _, processHandle) <- createProcess process
  System.IO.hPutStr std_in input
  hClose std_in
  result <- Data.ByteString.hGetContents std_out
  exitCode <- waitForProcess processHandle
  return (exitCode, result)
