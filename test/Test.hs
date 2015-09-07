import           Test.Tasty
import           Test.Tasty.Hspec

import           Codec.Bolt.Encode as E

main = do
    encodingTests <- createEncodingTests
    defaultMain (testGroup "All tests" [encodingTests])

createEncodingTests =
  testSpec "Codec.Bolt.Encode" $
    describe "encoding" $ do
      it "encodes null" $ (show E.null) `shouldBe` "C0"
      it "encodes double" $ (show (E.double 1.1)) `shouldBe` "C1 3F F1 99 99 99 99 99 9A"
      it "encodes false" $ (show E.false) `shouldBe` "C2"
      it "encodes true" $ (show E.true) `shouldBe` "C3"
