module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Network.HTTP.Req.Machine.Test as Test

main :: IO ()
main = defaultMain Test.tests
