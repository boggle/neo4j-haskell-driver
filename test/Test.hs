module Main(
  main
) where

import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners

import qualified Codec.Bolt.ValueTest
import qualified Codec.Packstream.Test

main :: IO ()
main = tests >>= defaultMainWithIngredients [ rerunningTests [ listingTests, consoleTestReporter ] ]

tests :: IO TestTree
tests =
  let
    packstreamUnitTests = Codec.Packstream.Test.unitTests
  in do
    boltValueSpecTests <- Codec.Bolt.ValueTest.specTests
    return $ testGroup "all tests" [
        testGroup "unit tests" [ packstreamUnitTests ],
        testGroup "bolt specification tests" [ boltValueSpecTests ]
      ]
