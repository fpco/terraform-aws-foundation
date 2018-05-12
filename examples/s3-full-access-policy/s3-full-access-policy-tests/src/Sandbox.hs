{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Sandbox
    ( checkBucketPolicy
    ) where

import Control.Lens (view, (^.), set, (<&>))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.AWS as AWST

import Data.Monoid ( (<>) )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson
import Data.Conduit ((=$=), ($$))
import qualified Data.Conduit.List as CL

import qualified Network.AWS as AWS
import qualified Network.AWS.S3 as S3
import Network.AWS.Data (toText, ToText(..), toBS)
import Network.AWS.Auth (credProfile)

import System.IO (stdout)

accessKey = "AWS_ACCESS_KEY_ID"
secretKey = "AWS_SECRET_ACCESS_KEY"
sessToken = "AWS_SESSION_TOKEN"
region    = "AWS_REGION"

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn

listAll :: IO ()
listAll = do
    lgr <- AWST.newLogger AWST.Debug stdout
    env <- AWST.newEnv
             (AWST.FromEnv accessKey secretKey (Just sessToken) (Just region))
             <&> set AWST.envLogger lgr

    let val :: ToText a => Maybe a -> Text
        val   = maybe "Nothing" toText

        lat v = maybe mempty (mappend " - " . toText) (v ^. S3.ovIsLatest)
        key v = val (v ^. S3.ovKey) <> ": " <> val (v ^. S3.ovVersionId) <> lat v

    AWST.runResourceT . AWST.runAWST env $ do
        say "Listing Buckets .."
        bs <- view S3.lbrsBuckets <$> AWS.send S3.listBuckets
        say $ "Found " <> toText (length bs) <> " Buckets."

        forM_ bs $ \(view S3.bName -> b) -> do
            bp <- view S3.gbprsPolicy <$> AWS.send (S3.getBucketPolicy b)
            say $ "Found Bucket: " <> toText b
            say $ "With policy: " <> toText (show bp)

        forM_ bs $ \(view S3.bName -> b) -> do
            say $ "Listing Object Versions in: " <> toText b
            AWS.paginate (S3.listObjectVersions b)
                =$= CL.concatMap (view S3.lovrsVersions)
                 $$ CL.mapM_     (say . mappend " -> " . key)

checkBucketPolicy :: Text -> IO ()
checkBucketPolicy bucketName = do
  lgr <- AWST.newLogger AWST.Debug stdout
  env <- AWST.newEnv
            (AWST.FromEnv accessKey secretKey (Just sessToken) (Just region))
            <&> set AWST.envLogger lgr

  let target  = (S3.BucketName bucketName) :: S3.BucketName

  AWST.runResourceT . AWST.runAWST env $ do
    -- check if bucket exists
    AWST.await S3.bucketExists $ S3.headBucket target
    say $ "Found Bucket: " <> toText bucketName
    -- get bucket policy
    bp <- view S3.gbprsPolicy <$> AWS.send (S3.getBucketPolicy target)
    say $ "With policy: "  <> toText (show bp)
