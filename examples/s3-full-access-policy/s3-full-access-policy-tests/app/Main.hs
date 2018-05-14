-- |
-- Test code for make AWS api calls. Based on the example at:
-- https://github.com/brendanhay/amazonka/blob/master/examples/src/Example/S3.hs

module Main where


import Verify
import Sandbox

main :: IO ()
main = do
  -- checkBucketPolicy "s3-full-access-policy-bucket"
  testIamUser
