module Test.NFATest where

import Test.Hspec ( Spec, describe, it, shouldBe, hspec )
import Data.Set.Monad ( Set, fromList, empty )

import Src.Util.Synonym ( State, Symbol )
import Src.Util.Kleene ( kleeneStar )
import Src.NFA.NFA ( NFA (..), enumerateAcceptedWords ) 

run :: IO ()
run = hspec $ do
    test01

test01 :: Spec
test01 = describe "test01, accepting words ending with \"ab\"" $ do it "standard" $ acceptedWords `shouldBe` expected
    where
        stateSet :: Set State = fromList [0, 1, 2]
        alphabet :: Set Symbol = fromList ['a', 'b']
        transitionFunction :: (State, Symbol) -> Set State
        transitionFunction (0, 'a') = fromList [0, 1]
        transitionFunction (0, 'b') = fromList [0]
        transitionFunction (1, 'b') = fromList [2]
        transitionFunction        _ = empty
        initialState :: State = 0
        acceptingStateSet :: Set State = fromList [2]
        exampleNFA :: NFA = NFA stateSet alphabet transitionFunction initialState acceptingStateSet
        acceptedWords :: Set [Symbol] = enumerateAcceptedWords 4 exampleNFA
        expected :: Set [Symbol] = fromList ["ab", "aab", "bab", "aaab", "abab", "baab", "bbab"]