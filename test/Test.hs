import           Test.Tasty
import           Test.Tasty.Hspec

import           Codec.Bolt.Encode as E

main = do
    encodingTests <- createEncodingTests
    defaultMain (testGroup "All tests" [encodingTests])

createEncodingTests =
  testSpec "Codec.Bolt.Encode" $
    describe "encoder" $ do
      it "encodes true" $ (show E.true) `shouldBe` "c3"
      it "encodes false" $ (show E.false) `shouldBe` "c2"
      it "encodes null" $ (show E.null) `shouldBe` "c0"
