{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Test code for make AWS api calls. Based on the example at:
-- https://github.com/brendanhay/amazonka/blob/master/examples/src/Example/S3.hs

module Main where

import Cred (getCreds)

import Control.Lens (view, (^.), set, (<&>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.AWS as AWST

import Data.Monoid ( (<>) )
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Data.Conduit ((=$=), ($$))
import qualified Data.Conduit.List as CL

import qualified Network.AWS as AWS
import qualified Network.AWS.S3 as S3
import Network.AWS.Data (toText, ToText(..))
import Network.AWS.Auth (credProfile)

import System.IO (stdout)

main :: IO ()
main = listAll AWS.NorthVirginia

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn

-- profile :: Text
-- profile = "fpco-dev-sandbox-admin"

accessKey = "AWS_ACCESS_KEY_ID"
secretKey = "AWS_SECRET_ACCESS_KEY"
sessToken = "AWS_SESSION_TOKEN"
region = "AWS_REGION"

listAll :: AWS.Region -- ^ Region to operate in.
        -> IO ()
listAll r = do
    lgr <- AWST.newLogger AWST.Debug stdout
    cred <- getCreds
    -- env <- AWST.newEnv AWST.Discover <&> set AWST.envLogger lgr . set AWST.envRegion r
    -- env <- AWST.newEnv cred <&> set AWST.envLogger lgr . set AWST.envRegion r
    env <- AWST.newEnv (AWST.FromEnv accessKey secretKey (Just sessToken) (Just region) ) <&> set AWST.envLogger lgr . set AWST.envRegion r

    let val :: ToText a => Maybe a -> Text
        val   = maybe "Nothing" toText

        lat v = maybe mempty (mappend " - " . toText) (v ^. S3.ovIsLatest)
        key v = val (v ^. S3.ovKey) <> ": " <> val (v ^. S3.ovVersionId) <> lat v

    AWST.runResourceT . AWST.runAWST env $ do
        say "Listing Buckets .."
        bs <- view S3.lbrsBuckets <$> AWS.send S3.listBuckets
        say $ "Found " <> toText (length bs) <> " Buckets."

        forM_ bs $ \(view S3.bName -> b) -> do
            say $ "Listing Object Versions in: " <> toText b
            AWS.paginate (S3.listObjectVersions b)
                =$= CL.concatMap (view S3.lovrsVersions)
                 $$ CL.mapM_     (say . mappend " -> " . key)
