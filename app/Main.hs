{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Amazonka.Data
import Amazonka.S3 (BucketName)
import Amazonka.S3.GetObject
import Amazonka.S3.ListBuckets
import Amazonka.S3.Types.Bucket
import Blaze.ByteString.Builder (fromByteString, toByteString)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Conduit
import GHC.Generics
import Network.Wai
import qualified Amazonka
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Text.Lazy as LT
import Web.Scotty

newtype Files = Files {files :: [String]} deriving (Show, Generic)
instance ToJSON Files
instance FromJSON Files

-- bucketName :: BucketName
-- bucketName = "hat-revenue-ratio"

main :: IO ()
main = do
    env <- Amazonka.newEnv Amazonka.discover
    scotty 3000 (app env)

app :: Amazonka.Env -> ScottyM ()
app env = do
    get "/" getAction
    post "/" postAction
    get "/buckets" $ do
        allBuckets <- getAllBuckets env
        text $ LT.pack (show allBuckets)

    get "/bucket/:bucket/:file" $ do
        b <- captureParam "bucket"
        f <- captureParam "file"
        text $ b <> " " <> f

    post "/stream" $ do
        createStream <- jsonData :: ActionM Files
        json createStream

    post "/stream-file" $ streamFile env

bodyToStream :: ResponseBody -> StreamingBody
bodyToStream b send flush = sinkBody b (CC.map fromByteString .| CC.mapM_ (liftIO . send) >> liftIO flush)

streamFile :: Amazonka.Env -> ActionM ()
streamFile env = do
    resp <- runResourceT $ Amazonka.send env (newGetObject "hat-revenue-ratio" "first")
    let body = bodyToStream $ resp ^. getObjectResponse_body
    setHeader "Content-Type" "text/plain"
    stream body

getAllBuckets env = do
    resp <- liftIO $ runResourceT $ Amazonka.send env newListBuckets
    return $ resp ^. listBucketsResponse_buckets

getAction :: ActionM ()
getAction = do
    text "This was a GET request!"

postAction :: ActionM ()
postAction = do
    text "This was a POST request!"


getFile env = do
    resp <- runResourceT $ Amazonka.send env (newGetObject "hat-revenue-ratio" "first")
    return $ resp ^. getObjectResponse_body
