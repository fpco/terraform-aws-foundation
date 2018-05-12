{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test
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
          , (AWST.FromKeys (AWS.AccessKey $ toBS (accessK out)) (AWS.SecretKey $ toBS (secretK out)))
          )
        Nothing  ->
          ( ["s3-full-access-policy-bucket"]
          , (AWST.FromEnv accessKey secretKey (Just sessToken) (Just region))
          )
      bucketName = head bucketL
      target  = (S3.BucketName bucketName) :: S3.BucketName

  lgr <- AWST.newLogger AWST.Debug stdout
  env <- AWST.newEnv cred
           <&> set AWST.envLogger lgr . set AWST.envRegion AWST.Oregon
           -- TODO set region based on Terraform output

  AWST.runResourceT . AWST.runAWST env $ do
    -- check if bucket exists
    AWST.await S3.bucketExists $ S3.headBucket target
    say $ "Able to find  Bucket: " <> toText bucketName
    -- upload a test file with put object
    let key :: S3.ObjectKey
        key  = S3.ObjectKey "test.txt"
        c :: AWS.ChunkSize
        c  = AWS.defaultChunkSize
        f :: FilePath
        f  = "test.txt"
    body <- AWS.chunkedFile c f
    void . AWS.send $ S3.putObject target key body
    -- TODO List, Delete and then List objects to further demonstrate access
