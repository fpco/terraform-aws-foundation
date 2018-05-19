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

-- A data type to store the test reporting internally.
data Report = Report
  { userTestCase    :: !UserTestCase
  , foundBucket     :: !(Either Text Text) -- ^ found the target bucket
  , uploadResponse  :: !(Either Text Text) -- ^ upload/put request response
  , listResponseOne :: !(Either Text Text) -- ^ able to list objects in bucket
  , foundPutObject  :: !(Either Text Text) -- ^ object was in bucket
  , deleteObjRes    :: !(Either Text Text) -- ^ delete object request result
  , listResponseTwo :: !(Either Text Text) -- ^ second listing of objects
  , objectDeleted   :: !(Either Text Text) -- ^ object deleted and was not in listed objects
  } deriving Show

-- A simple helper function to evaluate the report datatype
allTestsPassed :: Report -> Bool
allTestsPassed
  (Report _ (Right _) (Right _) (Right _) (Right _) (Right _) (Right _) (Right _))
  = True
allTestsPassed _ = False


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

-- | Parse bucket test for each scenario.
-- This is to see if the user in each case is able to find the target bucket.
-- The function is set up assuming that there is only one bucket.
bucketReport :: UserTestCase -> Either SomeException Accept -> Either Text Text
bucketReport FullAccessUser (Right AcceptSuccess) = Right "Found bucket"
bucketReport FullAccessUser (Right a) = Left "Could not find bucket"
bucketReport FullAccessUser (Left  e) = Left $ toText $ "Test failed. Exception caught: " <> show e
bucketReport _              (Right AcceptSuccess) = Left "Found bucket"
bucketReport _              (Right a) = Right "Could not find bucket"
bucketReport _              (Left  e) = Right $ toText $ "Bucket Test passed. Exception caught: " <> show e

-- | Parse upload test response for each scenario. This will not determine
-- whether the object is actually in the bucket beyond what the server response
-- indicates.
uploadReport :: UserTestCase -> Either SomeException S3.PutObjectResponse -> Either Text Text
uploadReport FullAccessUser (Right a) =
  case (view S3.porsResponseStatus a) <= 200 of
    True  -> Right $ toText $ "Upload test passed with status code: "
                                <> show (view S3.porsResponseStatus a)
    False -> Left  $ toText $ "Upload test failed for full access user. Status code: "
                                <> show (view S3.porsResponseStatus a)
uploadReport FullAccessUser (Left  e) =
  Left $ toText $ "Upload test failed. Exception caught: " <> show e
uploadReport _ (Right a) =
  case (view S3.porsResponseStatus a) <= 200 of
    False -> Right $ toText $ "Upload test passed with Status code: "
                              <> show (view S3.porsResponseStatus a)
    True -> Left "Upload test failed. Access given to users that should not have it."
uploadReport _ (Left  e) =
  Right $ toText $
           "Upload test passed. Access not given to to users that should not have\
           \ it. Exception caught: " <> show e

-- | Create the report for whether the full access user was able to list out the objects
-- in the bucket (and the other user cases were not).
listReport :: UserTestCase -> Either SomeException [S3.Object] -> Either Text Text
listReport FullAccessUser (Right _) =
  Right "Test passed. Full user able to list bucket objects"
listReport FullAccessUser (Left  e) =
  Left $ toText $
    "Test failed. Full user unable to list bucket objects: Exception caught:"
    <> show e
listReport _ (Right _) =
  Left "Test failed. User without access - or public - able to list bucket objects"
listReport _ (Left  e) =
  Right $ toText $
    "Test passed. User without access - or public - not able to \
    \list bucket objects. Exception caught: " <> show e

-- | Evaluate whether or not the uploaded object was actually uploaded
-- and is now in the bucket.
putReport :: UserTestCase -> Either SomeException [S3.Object] -> S3.ObjectKey -> Either Text Text
putReport FullAccessUser (Right objs) key =
   case elem key (map (\o -> view S3.oKey o) objs) of
     True -> Right "Test passed. Object was uploaded and found."
     False -> Left "Test failed. Object was not found in the bucket."
putReport FullAccessUser (Left e) key =
  Left $ toText $
    "Test failed. Full user unable to list bucket objects to find object. \
    \ Exception caught: " <> show e
putReport _ (Right _) key =
  Left  "Test failed. User without access - or public - able to list bucket objects"
putReport _ (Left e) key =
  Right $ toText $
    "Test passed. User without access - or public - not able to list bucket \
    \ objects. Exception caught: " <> show e

-- | Evaluate the response given for each scenario if the user can delete
deleteReport :: UserTestCase -> Either SomeException Int -> Either Text Text
deleteReport FullAccessUser (Right i) =
  case (i >= 200) && (i < 300) of
    True  -> Right $ toText $
               "Test passed. Full user able to send delete request. Status code: "
               <> show i
    False -> Left $ toText $
               "Test failed. Full user unable to send delete request. Status code: "
               <> show i
deleteReport FullAccessUser (Left e) =
  Left $ toText $
    "Test failed. Full user unable to send delete request. Exception caught: "
    <> show e
deleteReport _ (Right i) =
  case (i >= 200) && (i < 300) of
    True  -> Left  $
             toText $ "Test failed. User without access - or public - able to \
                      \ send delete request. Status code: " <> show i
    False -> Right $
             toText $ "Test passed. User without access - or public - not able \
                      \ to send delete request. Status code: " <> show i
deleteReport _ (Left e) =
  Right $ toText $
    "Test passed. User without access - or public - not able to send delete \
    \ request. Exception caught" <> show e


-- | Evaluate whether or not the uploaded object was actually deleted and is no
-- longer in the bucket.
removedReport :: UserTestCase -> Either SomeException [S3.Object] -> S3.ObjectKey -> Either Text Text
removedReport FullAccessUser (Right objs) key =
   case elem key (map (\o -> view S3.oKey o) objs) of
     False -> Right "Test passed. Deleted object was not found in the bucket."
     True  -> Left  "Test failed. Object was not deleted and is in the bucket."
removedReport FullAccessUser (Left e) key =
  Left $ toText $
    "Test failed. Full access user unable to list bucket objects to find \
    \object. Exception caught: " <> show e
removedReport _ (Right _) key =
  Left  "Test failed. User without access - or public - able to list bucket objects"
removedReport _ (Left  e) key =
  Right $ toText $
    "Test passed. User without access - or public - not able to list bucket \
    \objects. Exception caught: " <> show e

-- | testWithEnv
--
-- Run tests to verify access to the given bucket with the given
-- environment/credentials
testWithEnv :: S3.BucketName -> AWS.Env -> UserTestCase -> IO Report
-- testWithEnv :: S3.BucketName -> AWS.Env -> UserTestCase -> IO Report
testWithEnv bucketName@(S3.BucketName bucketTextName) env userTestCase = do
  AWST.runResourceT . AWST.runAWST env $ do

    -- check if bucket exists - catch and report any errors that `await` function throws.
    foundBucketRes <- tryAny $ AWST.await S3.bucketExists $ S3.headBucket bucketName
    let foundBucket = bucketReport userTestCase foundBucketRes

    say "\nTest upload\n"
    -- upload a test file with put object
    let key :: S3.ObjectKey
        key  = S3.ObjectKey "test.txt"
        chunkSize :: AWS.ChunkSize
        chunkSize  = AWS.defaultChunkSize
        file :: FilePath
        file  = "test.txt"
    body <- AWS.chunkedFile chunkSize file
    uploadObjRes <- tryAny $ AWS.send $ S3.putObject bucketName key body
    let uploadResponse = uploadReport userTestCase uploadObjRes

    -- list objects in bucket
    say $ "\nListing Object Versions in: " <> bucketTextName <> "\n"
    objList <- tryAny $ view S3.lorsContents <$> AWS.send (S3.listObjects bucketName)
    let listResponseOne = listReport userTestCase objList
        foundPutObject  = putReport  userTestCase objList key
        -- look for object in bucket

    say "\nDelete object\n"
    -- delete the object that the test tried to put into the bucket
    deletionRes <- tryAny $ view S3.dorsResponseStatus <$>
                     AWS.send (S3.deleteObject bucketName key)
    let deleteObjRes = deleteReport userTestCase deletionRes

    -- list objects in bucket to show object was deleted
    say $ "Listing Object Versions in: " <> bucketTextName
    objList' <- tryAny $ view S3.lorsContents <$> AWS.send (S3.listObjects bucketName)
    let listResponseTwo = listReport userTestCase objList'
        objectDeleted   = removedReport userTestCase objList' key

    return Report{..}


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

  let testResults = [ fullAccessResult
                    , noAccessResult
                    ]
      testsPassed    = and (map allTestsPassed testResults)
      testsThatFailed = filter (not . allTestsPassed) testResults

  -- print the test that failed
  case testsPassed of
    True  -> say "All tests passed for each case."
    False -> do
      say "\nThe following tests failed: \n"
      mapM_ (say . toText . show) testsThatFailed
