{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Spec
Description : Test the s3-full-access-policy terraform module.
Copyright   : (c) 2015-2018 FP Complete
License     : MIT
Maintainer  : mike@fpcomplete.com

-}
module Spec
  ( testS3Access
  ) where

import Control.Lens (view, (^.), set, (<&>))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.AWS as AWST
import Control.Exception.Safe (tryAny, SomeException)

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
import Network.AWS.Waiter (Accept(..))

import Test.Hspec

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

-- | UserTestCase
--
-- A data type which represents the two possible user test scenarios that are
-- currently being scrutinized.
data UserTestCase = FullAccessUser
                  | NoAccessUser
                  | Public
                  deriving Show


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


-- | This will see if the user in each case is able to find the target bucket.
-- The function is set up assuming that there is only one bucket.
bucketTest :: S3.BucketName -> AWS.Env -> IO Accept
bucketTest bucketName env =
  AWST.runResourceT . AWST.runAWST env $ do
    AWST.await S3.bucketExists $ S3.headBucket bucketName

bucketReport :: S3.BucketName -> AWS.Env -> UserTestCase -> SpecWith ()
bucketReport bucketName env FullAccessUser = do
  describe "bucket report" $ do
    it "Full access user can list and find target bucket" $ do
      a <- bucketTest bucketName env
      shouldSatisfy a (==AcceptSuccess)
bucketReport bucketName env _ = do
  describe "bucket report" $ do
    it "Users without access cannot list and find bucket" $ do
      shouldThrow (bucketTest bucketName env) anyException


uploadTest :: S3.ObjectKey -> AWS.RqBody -> S3.BucketName -> AWS.Env -> IO (AWS.Rs S3.PutObject)
uploadTest key body bucketName env =
  AWST.runResourceT . AWST.runAWST env $ do
    AWS.send $ S3.putObject bucketName key body

uploadReport :: S3.ObjectKey -> AWS.RqBody -> S3.BucketName -> AWS.Env -> UserTestCase -> SpecWith ()
uploadReport key body bucketName env FullAccessUser = do
  describe "upload report" $ do
    it "full access user put" $ do
      status <- uploadTest key body bucketName env
      shouldSatisfy (view S3.porsResponseStatus status) (\i -> (i >= 200) && (i < 300))
uploadReport key body bucketName env _ = do
  describe "upload report" $ do
    it "upload" $ do
      -- this should handle the exeception thrown in upload test for this case
      shouldThrow (uploadTest key body bucketName env) anyException

listTest :: S3.BucketName -> AWS.Env -> IO [S3.Object]
listTest bucketName env = do
  AWST.runResourceT . AWST.runAWST env $ do
    view S3.lorsContents <$> AWS.send (S3.listObjects bucketName)

-- | Report whether the full access user was able to list out the objects
-- in the bucket (and the other user cases were not).
listReport :: S3.ObjectKey -> S3.BucketName -> AWS.Env -> UserTestCase -> SpecWith ()
listReport key bucketName env FullAccessUser = do
  describe "list report" $ do
    it "full access can list objects" $ do
      objList <- listTest bucketName env
      length objList `shouldSatisfy` (>=1)
listReport key bucketName env _ = do
  describe "list report" $ do
    it "non-access user try to list objects" $ do
      shouldThrow (listTest bucketName env) anyException

-- | Evaluate whether or not the uploaded object was actually uploaded
-- and is now in the bucket.
putReport :: S3.ObjectKey -> S3.BucketName -> AWS.Env -> UserTestCase -> SpecWith ()
putReport key bucketName env FullAccessUser = do
  describe "put report" $ do
    it "Full access user found uploaded item" $ do
      objList <- listTest bucketName env
      elem key (map (\o -> view S3.oKey o) objList) `shouldBe` True
putReport _key bucketName env _ = do
  describe "put report" $ do
    it "User without access trying to list objects" $ do
      shouldThrow (listTest bucketName env) anyException

deleteTest :: S3.ObjectKey -> S3.BucketName -> AWS.Env -> IO Int
deleteTest key bucketName env =
  AWST.runResourceT . AWST.runAWST env $ do
    view S3.dorsResponseStatus <$> AWS.send (S3.deleteObject bucketName key)

-- | Evaluate the response given for if the user has access to delete
deleteReport :: S3.ObjectKey -> S3.BucketName -> AWS.Env -> UserTestCase -> SpecWith ()
deleteReport key bucketName env FullAccessUser = do
  describe "delete report" $ do
    it "full access user should be able to send delete request" $ do
      statusCode <- deleteTest key bucketName env
      shouldSatisfy statusCode (\i -> (i >= 200) && (i < 300))
deleteReport key bucketName env _ = do
  describe "delete report" $ do
    it "Users without access should not be able to make delete requests" $ do
      shouldThrow (deleteTest key bucketName env) anyException

-- | Evaluate whether or not the uploaded object was actually deleted and is no
-- longer in the bucket.
removedReport :: S3.ObjectKey -> S3.BucketName -> AWS.Env -> UserTestCase -> SpecWith ()
removedReport key bucketName env FullAccessUser = do
  describe "removed report" $ do
    it "Full access user should not find deleted object" $ do
      objList <- listTest bucketName env
      elem key (map (\o -> view S3.oKey o) objList) `shouldBe` False
removedReport _key bucketName env _ = do
  describe "removed report" $ do
    it "User without access trying to list objects" $ do
      shouldThrow (listTest bucketName env) anyException

generateReport :: S3.ObjectKey -> AWS.RqBody -> S3.BucketName -> AWS.Env -> UserTestCase -> IO ()
generateReport key body bucketName env FullAccessUser = do
  hspec $ do
    bucketReport bucketName env FullAccessUser
    uploadReport key body bucketName env FullAccessUser
    listReport key bucketName env FullAccessUser
    putReport key bucketName env FullAccessUser
    deleteReport key bucketName env FullAccessUser
    removedReport key bucketName env FullAccessUser
generateReport key body bucketName env userTestCase = do
  hspec $ do
    bucketReport bucketName env userTestCase
    uploadReport key body bucketName env userTestCase
    listReport key bucketName env userTestCase
    putReport key bucketName env userTestCase
    deleteReport key bucketName env userTestCase
    removedReport key bucketName env userTestCase

-- | testWithEnv
--
-- Run tests to verify access to the given bucket with the given
-- environment/credentials
testWithEnv :: S3.BucketName -> AWS.Env -> UserTestCase -> IO ()
testWithEnv bucketName env userTestCase = do
  AWST.runResourceT . AWST.runAWST env $ do
    -- create test file parts to use to put/upload to bucket
    let key :: S3.ObjectKey
        key  = S3.ObjectKey "test.txt"
        chunkSize :: AWS.ChunkSize
        chunkSize  = AWS.defaultChunkSize
        file :: FilePath
        file  = "test.txt"
    body <- AWS.chunkedFile chunkSize file

    liftIO $ do
      Text.putStrLn "Running tests."
      generateReport key body bucketName env userTestCase

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
      -- TODO handle scenario with more than one bucket
      bucketName     = (S3.BucketName (head bucketL)) :: S3.BucketName
      reg            = AWST.Oregon
      -- TODO set region based on Terraform output

  lgr <- AWST.newLogger AWST.Debug stdout

  -- test environment with full access
  envFullAcc <- AWST.newEnv credFullAcc
                 <&> set AWST.envLogger lgr . set AWST.envRegion reg

  -- test environment with iam user with credentials that do not give access
  envNoAcc <- AWST.newEnv credNoAcc
                 <&> set AWST.envLogger lgr . set AWST.envRegion reg

  -- test with full access
  say "\nTesting full access.\n"
  fullAccessResult <- testWithEnv bucketName envFullAcc FullAccessUser

  -- test with credentials that don't grant access
  say "\nTesting user with no access.\n"
  noAccessResult <- testWithEnv bucketName envNoAcc NoAccessUser

  say "\nTests complete.\n"
