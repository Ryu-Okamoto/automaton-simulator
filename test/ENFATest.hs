module Test.ENFATest where

import Test.Hspec ( Spec, describe, it, shouldBe, hspec )
import Data.Set.Monad ( Set, fromList, empty )

import Src.Util.Synonym ( State, Symbol )
import Src.Util.Kleene ( kleeneStar )
import Src.ENFA.ENFA ( ENFA (..), judge ) 
import Src.ENFA.ENFAExample ( exampleENFA )

run :: IO ()
run = hspec $ do
    describe "epsilon nfa test. check that exampleENFA accepts decimals correctly" $ do
        it "accept: 100.0" $ judge exampleENFA "100.0" `shouldBe` True
        it "accept: -.999" $ judge exampleENFA "-.999" `shouldBe` True
        it "accept: 5050." $ judge exampleENFA "5050." `shouldBe` True
        it "accept: +123." $ judge exampleENFA "+123." `shouldBe` True
        it "  deny: +12+4" $ judge exampleENFA "+12+4" `shouldBe` False
        it "  deny: 10000" $ judge exampleENFA "10000" `shouldBe` False
        it "  deny: .000." $ judge exampleENFA ".000." `shouldBe` False
        it "  deny: -1240" $ judge exampleENFA "-1240" `shouldBe` False