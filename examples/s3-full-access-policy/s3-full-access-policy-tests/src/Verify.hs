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
  ( testS3Access
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
  { accessKeyFullAccess :: Text
  , secretKeyFullAccess :: Text
  , accessKeyNoAccess :: Text
  , secretKeyNoAccess :: Text
  , bucketList :: [Text]
  }

instance FromJSON Output where
  parseJSON (Object v) = do

    accKeyObj <- v .: "access_key_full_access_user"
    accessKeyFullAccess <- accKeyObj .: "value"
    secKeyObj <- v .: "secret_key_full_access_user"
    secretKeyFullAccess <- secKeyObj .: "value"

    accKeyNoAccessObj <- v .: "access_key_no_access_user"
    accessKeyNoAccess <- accKeyNoAccessObj .: "value"
    secKeyNoAccessObj <- v .: "secret_key_no_access_user"
    secretKeyNoAccess <- secKeyNoAccessObj .: "value"

    bucketListObj <- v .: "bucket_list"
    bucketList    <- bucketListObj .: "value"
    return Output{..}


-- Set default environmental variables
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


-- TODO have this report a test result rather than ()
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


-- | testS3Access
--
-- A function to test access to the s3 resources created by terraform.
--
-- This will run the same access tests in each of the three scenario environments:
--
--   * The iam user which terraform created and attached the full-access policy.
--   * With requests from the iam user created which does not have any policy attached giving access. 
--   * Simulated public requests - no user and no access.
--
testS3Access :: IO ()
testS3Access = do
  mOutput <- retrieveOutput

  let (bucketL, credFullAcc, credNoAcc) = case mOutput of
        Just out ->
          ( bucketList out
          , (AWST.FromKeys
               (AWS.AccessKey $ toBS (accessKeyFullAccess out))
               (AWS.SecretKey $ toBS (secretKeyFullAccess out)))
          , (AWST.FromKeys
               (AWS.AccessKey $ toBS (accessKeyNoAccess out))
               (AWS.SecretKey $ toBS (secretKeyNoAccess out)))
          )
        Nothing ->
          ( ["s3-full-access-policy-bucket"]
          , AWST.FromEnv accessKey secretKey (Just sessToken) (Just region)
          , AWST.FromKeys (AWS.AccessKey "") (AWS.SecretKey "")
          )
      bucketTextName = head bucketL -- TODO handle scenario with more than one bucket
      bucketName     = (S3.BucketName bucketTextName) :: S3.BucketName
      noCred         = AWST.FromKeys (AWS.AccessKey "") (AWS.SecretKey "")
      reg            = AWST.Oregon
      -- TODO set region based on Terraform output

  lgr <- AWST.newLogger AWST.Debug stdout

  -- test enviroment with full access
  envFullAcc <- AWST.newEnv credFullAcc
                 <&> set AWST.envLogger lgr . set AWST.envRegion reg

  -- test enviroment with iam user with credentials that do not give access
  envNoAcc <- AWST.newEnv credNoAcc
                 <&> set AWST.envLogger lgr . set AWST.envRegion reg

  -- simulate enviroment with no credentials
  envPublic <- AWST.newEnv noCred
                 <&> set AWST.envLogger lgr . set AWST.envRegion reg

  -- test with full access
  testWithEnv bucketName envFullAcc

  -- test with credentials that don't grant access
  testWithEnv bucketName envNoAcc

  -- test as public
  testWithEnv bucketName envPublic
