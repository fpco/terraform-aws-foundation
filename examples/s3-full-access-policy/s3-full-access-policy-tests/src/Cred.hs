module Cred
  ( getCreds
  ) where

import qualified Control.Monad.Trans.AWS as AWST
import System.Environment (getEnv, getEnvironment)
import Data.String (IsString(..))

getEnvVar :: IsString s => String -> IO s
getEnvVar varName = fmap fromString $ getEnv varName

getCreds :: IO AWST.Credentials
getCreds = do
  accessKeyId <- getEnvVar "AWS_ACCESS_KEY_ID"
  secret      <- getEnvVar "AWS_SECRET_ACCESS_KEY"
  creds   <- return $ AWST.FromKeys (AWST.AccessKey accessKeyId) (AWST.SecretKey secret)
  return creds
