module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Control.Monad.Catch.Tests

main :: IO ()
main = defaultMain $
  testGroup "exceptions"
    [ Control.Monad.Catch.Tests.tests
    ]
