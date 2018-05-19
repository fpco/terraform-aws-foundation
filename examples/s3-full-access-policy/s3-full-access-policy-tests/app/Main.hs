{-|
Module      : Main
Description : Test the s3-full-access-policy terraform module.
Copyright   : (c) 2015-2018 FP Complete
License     : MIT
Maintainer  : mike@fpcomplete.com

Run the test code for make AWS api calls against the the s3-full-access-policy
terraform module.
-}
module Main where

import qualified Spec

main :: IO ()
main = Spec.testS3Access
