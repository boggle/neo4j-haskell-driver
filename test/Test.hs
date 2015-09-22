module Main(
  main
) where

import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners

import qualified Codec.Packstream.AtomTest
import qualified Codec.Packstream.CodingTest

main :: IO ()
main = tests >>= defaultMainWithIngredients [ rerunningTests [ listingTests, consoleTestReporter ] ]

tests :: IO TestTree
tests =
  let
    packstreamCodingUnitTests = Codec.Packstream.CodingTest.unitTests
  in do
    packstreamAtomUnitTests <- Codec.Packstream.AtomTest.unitTests
    return $ testGroup "all tests" [
        testGroup "unit tests" [ packstreamCodingUnitTests, packstreamAtomUnitTests ]
      ]
