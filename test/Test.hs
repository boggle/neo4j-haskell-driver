import           Test.Tasty
import           Test.Tasty.Hspec

import           Codec.Packstream.Encode    as E
import           Codec.Packstream.Encode    ((@@))
import           Codec.Packstream.Signature
import           Data.Text                  as T

main = do
    encodingTests <- createEncodingTests
    defaultMain (testGroup "All tests" [encodingTests])

createEncodingTests =
  testSpec "Codec.Bolt.Encode" $ do
    describe "encoding of fixed size value" $ do
      it "encodes null" $ show E.null `shouldBe` "C0"
      it "encodes double" $ show (E.float64 1.1) `shouldBe` "C1 3F F1 99 99 99 99 99 9A"
      it "encodes false" $ show E.false `shouldBe` "C2"
      it "encodes true" $ show E.true `shouldBe` "C3"
      it "encodes -16 as tiny int" $ show (E.tinyInt $ fromIntegral $ - 16) `shouldBe` "F0"
      it "encodes -1 as tiny int" $ show (E.tinyInt $ fromIntegral $ - 1) `shouldBe` "FF"
      it "encodes 0 as tiny int" $ show (E.tinyInt 0) `shouldBe` "00"
      it "encodes 127 as tiny int" $ show (E.tinyInt 127) `shouldBe` "7F"
      it "encodes -128 as an int8 if given as tiny int" $ show (E.tinyInt $ fromIntegral $ - 128) `shouldBe` "C8 80"
      it "encodes -17 as an int8 if given as tiny int" $ show (E.tinyInt $ fromIntegral $ - 17) `shouldBe` "C8 EF"
      it "encodes -128 as an int8" $ show (E.int8 $ fromIntegral $ - 128) `shouldBe` "C8 80"
      it "encodes -17 as an int8" $ show (E.int8 $ fromIntegral $ - 17) `shouldBe` "C8 EF"
      it "encodes 32000 as an int16" $ show (E.int16 32000) `shouldBe` "C9 7D 00"
      it "encodes 96000 as an int32" $ show (E.int32 96000) `shouldBe` "CA 00 01 77 00"
      it "encodes 999999999 as an int64" $ show (E.int64 96000) `shouldBe` "CB 00 00 00 00 00 01 77 00"
    describe "encoding of text" $ do
      it "encodes ''" $ show (E.string "") `shouldBe` "80"
      it "encodes 'hallo'" $ show (E.string "hallo") `shouldBe` "85 68 61 6C 6C 6F"
      it "encodes 'abcdefghijklmonpqrstuvwxyz'" $ show (E.string "abcdefghijklmonpqrstuvwxyz") `shouldBe` "D0 1A 61 62 63 64 65 66 67 68 69 6A 6B 6C 6D 6F 6E 70 71 72 73 74 75 76 77 78 79 7A"
    describe "encoding of lists" $ do
      it "encodes []" $ show (E.list []) `shouldBe` "90"
      it "encodes [1, 2, 3]" $ show (E.list [E.tinyInt 1, E.tinyInt 2, E.tinyInt 3]) `shouldBe` "93 01 02 03"
    describe "encoding of structs" $ do
      it "encodes Struct (signature=0x01) { 1, 2, 3 }" $ show (E.structure (signature 1) [E.tinyInt 1, E.tinyInt 2, E.tinyInt 3]) `shouldBe` "B3 01 01 02 03"
      it "encodes Struct (signature=0x01) { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6 }" $ show longStruct `shouldBe` "DC 10 01 01 02 03 04 05 06 07 08 09 00 01 02 03 04 05 06"
    describe "encoding of maps" $ do
      it "encodes {}" $ show  (E.map []) `shouldBe` "A0"
      it "encodes {a: 1}" $ show  (E.map [E.string "a" @@ E.tinyInt 1]) `shouldBe` "A1 81 61 01"
      it "encodes {a: 1, b: 1, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9, j: 0, k: 1, l: 2, m: 3, n: 4, o: 5, p: 6}" $ show longMap `shouldBe` "D8 10 81 61 01 81 62 01 81 63 03 81 64 04 81 65 05 81 66 06 81 67 07 81 68 08 81 69 09 81 6A 00 81 6B 01 81 6C 02 81 6D 03 81 6E 04 81 6F 05 81 70 06"
      where
        longMap = E.map [
            E.string "a" @@ E.tinyInt 1,
            E.string "b" @@ E.tinyInt 1,
            E.string "c" @@ E.tinyInt 3,
            E.string "d" @@ E.tinyInt 4,
            E.string "e" @@ E.tinyInt 5,
            E.string "f" @@ E.tinyInt 6,
            E.string "g" @@ E.tinyInt 7,
            E.string "h" @@ E.tinyInt 8,
            E.string "i" @@ E.tinyInt 9,
            E.string "j" @@ E.tinyInt 0,
            E.string "k" @@ E.tinyInt 1,
            E.string "l" @@ E.tinyInt 2,
            E.string "m" @@ E.tinyInt 3,
            E.string "n" @@ E.tinyInt 4,
            E.string "o" @@ E.tinyInt 5,
            E.string "p" @@ E.tinyInt 6
          ]
        longStruct = E.structure (signature 1) [
            E.tinyInt 1,
            E.tinyInt 2,
            E.tinyInt 3,
            E.tinyInt 4,
            E.tinyInt 5,
            E.tinyInt 6,
            E.tinyInt 7,
            E.tinyInt 8,
            E.tinyInt 9,
            E.tinyInt 0,
            E.tinyInt 1,
            E.tinyInt 2,
            E.tinyInt 3,
            E.tinyInt 4,
            E.tinyInt 5,
            E.tinyInt 6
          ]
