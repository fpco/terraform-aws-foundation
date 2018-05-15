{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Verify
Description : Test the s3-full-access-policy terraform module.
Copyright   : (c) 2015-2018 FP Complete
License     : MIT
Maintainer  : mike@fpcomplete.com

-}
module Verify
  ( testIamUser
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

-- A data type to store the parsed output that terraform creates.
data Output = Output
  { accessK    :: Text
  , secretK    :: Text
  , bucketList :: [Text]
  }

instance FromJSON Output where
  parseJSON (Object v) = do
    accKeyObj     <- v .: "access_key"
    secKeyObj     <- v .: "secret_key"
    bucketListObj <- v .: "bucket_list"
    accessK       <- accKeyObj .: "value"
    secretK       <- secKeyObj .: "value"
    bucketList    <- bucketListObj .: "value"
    return Output{..}

accessKey = "AWS_ACCESS_KEY_ID"
secretKey = "AWS_SECRET_ACCESS_KEY"
sessToken = "AWS_SESSION_TOKEN"
region    = "AWS_REGION"

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn

-- | retrieveOutput
--
-- A function that will retrieve and parse the output file created by Terraform
-- called ../config.json
retrieveOutput :: IO (Maybe Output)
retrieveOutput = do
  conf <- BSL.readFile "../config.json"
  return (decode' conf)


-- | testWithEnv
-- 
-- Run tests to verify access to the given bucket with the given
-- enviroment/credentials
testWithEnv :: S3.BucketName -> AWS.Env -> IO () 
testWithEnv bucketName@(S3.BucketName bucketTextName) env = do
  AWST.runResourceT . AWST.runAWST env $ do

    -- check if bucket exists
    AWST.await S3.bucketExists $ S3.headBucket bucketName
    say $ "Able to find  Bucket: " <> bucketTextName

    -- upload a test file with put object
    let key :: S3.ObjectKey
        key  = S3.ObjectKey "test.txt"
        c :: AWS.ChunkSize
        c  = AWS.defaultChunkSize
        f :: FilePath
        f  = "test.txt"
    body <- AWS.chunkedFile c f
    void . AWS.send $ S3.putObject bucketName key body

    -- list objects in bucket
    say $ "Listing Object Versions in: " <> bucketTextName
    objList <- view S3.lorsContents <$> AWS.send (S3.listObjects bucketName)
    forM_ objList $ \(view S3.oKey -> (S3.ObjectKey k)) -> do
        say $ "Found object: " <> k

    -- delete the object that the test tried to put into the bucket
    deletionRes <- view S3.dorsResponseStatus <$>
                     AWS.send (S3.deleteObject bucketName key)
    say (toText $ "Deletion returned response status: " <> show deletionRes)

    -- list objects in bucket to show object was deleted
    say $ "Listing Object Versions in: " <> bucketTextName
    objList' <- view S3.lorsContents <$> AWS.send (S3.listObjects bucketName)
    forM_ objList' $ \(view S3.oKey -> (S3.ObjectKey k)) -> do
        say $ "Found object: " <> k


-- | testIamUser
--
-- A function to test if the IAM user and credentials created by terraform
-- can be accessed and used
testIamUser :: IO ()
testIamUser = do
  mOutput <- retrieveOutput

  let (bucketL, cred) = case mOutput of
        Just out ->
          ( bucketList out
          , (AWST.FromKeys
               (AWS.AccessKey $ toBS (accessK out))
               (AWS.SecretKey $ toBS (secretK out)))
          )
        Nothing  ->
          ( ["s3-full-access-policy-bucket"]
          , (AWST.FromEnv accessKey secretKey (Just sessToken) (Just region))
          )
      bucketTextName = head bucketL
      bucketName = (S3.BucketName bucketTextName) :: S3.BucketName
      -- approximate what the public requests would look like (without cred).
      noCred = AWST.FromKeys (AWS.AccessKey "") (AWS.SecretKey "")

  lgr <- AWST.newLogger AWST.Debug stdout

  -- TODO set region based on Terraform output
  env <- AWST.newEnv cred
           <&> set AWST.envLogger lgr . set AWST.envRegion AWST.Oregon

  envPublic <- AWST.newEnv noCred
                 <&> set AWST.envLogger lgr . set AWST.envRegion AWST.Oregon

  -- test with full access
  testWithEnv bucketName env

  -- test as public
  testWithEnv bucketName envPublic
