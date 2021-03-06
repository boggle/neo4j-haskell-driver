module Codec.Packstream.AtomTest (
  unitTests
) where

import           Test.Tasty
import           Test.Tasty.Hspec

import           Data.Binary
import           Data.Int
import           Data.Text                  (pack)
import qualified Data.Vector                as V

import           Codec.Packstream.Atom
import           Codec.Packstream.ShowBytes

unitTests :: IO TestTree
unitTests =
  testSpec "Codec.Packstream.Atom" $ do
      describe "encoding of fixed size values" $ do
        encoding "null" ANull (`shouldBe` "C0")
        encoding "false" (ABool False) (`shouldBe` "C2")
        encoding "true" (ABool True) (`shouldBe` "C3")
      describe "encoding of numbers" $ do
        encoding "double" (ADouble 1.1) (`shouldBe` "C1 3F F1 99 99 99 99 99 9A")
        encoding "double" (ADouble -1.1 ) (`shouldBe` "C1 BF F1 99 99 99 99 99 9A")
        encoding "-16" (AInt8 -16) (`shouldBe` "F0")
        encoding "-1" (AInt8 -1) (`shouldBe` "FF")
        encoding "0" (AInt8 0) (`shouldBe` "00")
        encoding "1" (AInt8 1) (`shouldBe` "01")
        encoding "2" (AInt8 2) (`shouldBe` "02")
        encoding "3" (AInt8 3) (`shouldBe` "03")
        encoding "127" (AInt8 127) (`shouldBe` "7F")
        encoding "-128" (AInt8 -128) (`shouldBe` "C8 80")
        encoding "-17" (AInt8 -17) (`shouldBe` "C8 EF")
        encoding "-128" (AInt8 -128) (`shouldBe` "C8 80")
        encoding "32000" (AInt16 32000) (`shouldBe` "C9 7D 00")
        encoding "96000" (AInt32 96000) (`shouldBe` "CA 00 01 77 00")
        encoding "-9223372036854775808" (AInt64 -9223372036854775808) (`shouldBe` "CB 80 00 00 00 00 00 00 00")
        encoding "9223372036854775807" (AInt64 9223372036854775807) (`shouldBe` "CB 7F FF FF FF FF FF FF FF")
      describe "encoding of text" $ do
        encoding "''" (AText $ pack "") (`shouldBe` "80")
        encoding "'hallo'" (AText $ pack "hallo") (`shouldBe` "85 68 61 6C 6C 6F")
        encoding "'abcdefghijklmonpqrstuvwxyz'" (AText $ pack "abcdefghijklmonpqrstuvwxyz") (`shouldBe` "D0 1A 61 62 63 64 65 66 67 68 69 6A 6B 6C 6D 6F 6E 70 71 72 73 74 75 76 77 78 79 7A")
      describe "encoding of lists" $ do
        encoding "V.fromList []" (AVector V.empty) (`shouldBe` "90")
        encoding "V.fromList [1, 2, 3]" (AVector $ V.fromList [AInt8 1, AInt8 2, AInt8 3]) (`shouldBe` "93 01 02 03")
        encoding "[1, 2, 3]" (AList [AInt8 1, AInt8 2, AInt8 3]) (`shouldBe` "D7 01 02 03 DF")
  where
    encoding :: Example b => String -> Atom -> (String -> b) -> SpecWith (Arg b)
    encoding what value cont = it ("encodes " ++ what) $ cont $ showBytes value

  --     describe "encoding of text" $ do
  --       it "encodes ''" $ show (E.string "") `shouldBe` "80"
  --       it "encodes 'hallo'" $ show (E.string "hallo") `shouldBe` "85 68 61 6C 6C 6F"
  --       it "encodes 'abcdefghijklmonpqrstuvwxyz'" $ show (E.string "abcdefghijklmonpqrstuvwxyz") `shouldBe` "D0 1A 61 62 63 64 65 66 67 68 69 6A 6B 6C 6D 6F 6E 70 71 72 73 74 75 76 77 78 79 7A"
  --     describe "encoding of structs" $ do
  --       it "encodes Struct (signature=0x01) { 1, 2, 3 }" $ show (E.structure (signature 1) [E.tinyInt 1, E.tinyInt 2, E.tinyInt 3]) `shouldBe` "B3 01 01 02 03"
  --       it "encodes Struct (signature=0x01) { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6 }" $ show longStruct `shouldBe` "DC 10 01 01 02 03 04 05 06 07 08 09 00 01 02 03 04 05 06"
  --     describe "encoding of maA" $ do
  --       it "encodes {}" $ show  (E.map []) `shouldBe` "A0"
  --       it "encodes {a: 1}" $ show  (E.map [E.string "a" @@ E.tinyInt 1]) `shouldBe` "A1 81 61 01"
  --       it "encodes {a: 1, b: 1, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9, j: 0, k: 1, l: 2, m: 3, n: 4, o: 5, p: 6}" $ show longMap `shouldBe` "D8 10 81 61 01 81 62 01 81 63 03 81 64 04 81 65 05 81 66 06 81 67 07 81 68 08 81 69 09 81 6A 00 81 6B 01 81 6C 02 81 6D 03 81 6E 04 81 6F 05 81 70 06"
  --       where
  --         longMap = E.map [
  --             E.string "a" @@ E.tinyInt 1,
  --             E.string "b" @@ E.tinyInt 1,
  --             E.string "c" @@ E.tinyInt 3,
  --             E.string "d" @@ E.tinyInt 4,
  --             E.string "e" @@ E.tinyInt 5,
  --             E.string "f" @@ E.tinyInt 6,
  --             E.string "g" @@ E.tinyInt 7,
  --             E.string "h" @@ E.tinyInt 8,
  --             E.string "i" @@ E.tinyInt 9,
  --             E.string "j" @@ E.tinyInt 0,
  --             E.string "k" @@ E.tinyInt 1,
  --             E.string "l" @@ E.tinyInt 2,
  --             E.string "m" @@ E.tinyInt 3,
  --             E.string "n" @@ E.tinyInt 4,
  --             E.string "o" @@ E.tinyInt 5,
  --             E.string "p" @@ E.tinyInt 6
  --           ]
  --         longStruct = E.structure (signature 1) [
  --             E.tinyInt 1,
  --             E.tinyInt 2,
  --             E.tinyInt 3,
  --             E.tinyInt 4,
  --             E.tinyInt 5,
  --             E.tinyInt 6,
  --             E.tinyInt 7,
  --             E.tinyInt 8,
  --             E.tinyInt 9,
  --             E.tinyInt 0,
  --             E.tinyInt 1,
  --             E.tinyInt 2,
  --             E.tinyInt 3,
  --             E.tinyInt 4,
  --             E.tinyInt 5,
  --             E.tinyInt 6
  --           ]
