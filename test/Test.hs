import           Test.Tasty
import           Test.Tasty.Hspec

import           Codec.Bolt.Encode as E

main = do
    encodingTests <- createEncodingTests
    defaultMain (testGroup "All tests" [encodingTests])

createEncodingTests =
  testSpec "Codec.Bolt.Encode" $
    describe "encoding" $ do
      it "encodes null" $ show E.null `shouldBe` "C0"
      it "encodes double" $ show (E.float64 1.1) `shouldBe` "C1 3F F1 99 99 99 99 99 9A"
      it "encodes false" $ show E.false `shouldBe` "C2"
      it "encodes true" $ show E.true `shouldBe` "C3"
      it "encodes -16 as tiny int" $ show (E.tinyInt $ fromIntegral $ - 16) `shouldBe` "F0"
      it "encodes -1 as tiny int" $ show (E.tinyInt $ fromIntegral $ - 1) `shouldBe` "FF"
      it "encodes 0 as tiny int" $ show (E.tinyInt $ fromIntegral 0) `shouldBe` "00"
      it "encodes 127 as tiny int" $ show (E.tinyInt $ fromIntegral 127) `shouldBe` "7F"
      it "encodes -128 as an int8 if given as tiny int" $ show (E.tinyInt $ fromIntegral $ - 128) `shouldBe` "C8 80"
      it "encodes -17 as an int8 if given as tiny int" $ show (E.tinyInt $ fromIntegral $ - 17) `shouldBe` "C8 EF"
      it "encodes -128 as an int8" $ show (E.int8 $ fromIntegral $ - 128) `shouldBe` "C8 80"
      it "encodes -17 as an int8" $ show (E.int8 $ fromIntegral $ - 17) `shouldBe` "C8 EF"
      it "encodes 32000 as an int16" $ show (E.int16 $ fromIntegral 32000) `shouldBe` "C9 7D 00"
      it "encodes 96000 as an int32" $ show (E.int32 $ fromIntegral 96000) `shouldBe` "CA 00 01 77 00"
      it "encodes 999999999 as an int64" $ show (E.int64 $ fromIntegral 96000) `shouldBe` "CB 00 00 00 00 00 01 77 00"
