module Test.KleeneTest where

import Test.Hspec ( Spec, describe, it, shouldBe, hspec )
import Data.Set.Monad ( Set, fromList )

import Src.Util.Synonym ( State, Symbol )
import Src.Util.Kleene ( kleeneStar )

run :: IO ()
run = hspec test

test :: Spec
test = describe "test" $ do it "standard" $ words `shouldBe` expected
    where
        words :: Set [Symbol] = kleeneStar 3 $ fromList ['a', 'b']
        expected :: Set [Symbol] = fromList [
                "",
                "a", "b",
                "aa", "ab", "ba", "bb",
                "aaa", "aab", "aba", "abb", "baa", "bab", "bba", "bbb"
            ]