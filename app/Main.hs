{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell,
             TypeFamilies, NoImplicitPrelude, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

-- import Data.Aeson
import qualified Amazonka
import Amazonka.Data
import Amazonka.S3.GetObject
import Amazonka.S3.ListBuckets
import Amazonka.S3.Types.Bucket
import Amazonka.S3.Internal
import Blaze.ByteString.Builder (fromByteString)
import ClassyPrelude.Yesod
import Control.Lens ((^.))
import qualified Data.Conduit.Combinators as CC

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/stream StreamR GET
/buckets BucketsR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello, World!|]

getStreamR :: Handler TypedContent
getStreamR = do
    runResourceT $ do
        env <- liftIO $ Amazonka.newEnv Amazonka.discover
        resp <- Amazonka.send env (newGetObject "hat-revenue-ratio" "huge")
        let bodyProducer = resp ^. getObjectResponse_body
        b <- liftIO $ runConduit $ bodyProducer `sinkBody` (CC.map fromByteString .| CC.foldMap id)
        return $ TypedContent typeOctet $ toContent b

getBucketsR :: Handler Value
getBucketsR = do
    env <- liftIO $ Amazonka.newEnv Amazonka.discover
    allBuckets <- getAllBuckets env
    case allBuckets of
        Nothing -> return $ object ["buckets" .= ("" :: Text)]
        Just b -> return $ object ["buckets" .= bs]
            where bs = map (fromBucketName . (^. bucket_name)) b

getAllBuckets :: MonadIO m => Amazonka.Env -> m (Maybe [Bucket])
getAllBuckets env = do
    resp <- liftIO $ runResourceT $ Amazonka.send env newListBuckets
    return $ resp ^. listBucketsResponse_buckets

main :: IO ()
main = warp 3000 App
