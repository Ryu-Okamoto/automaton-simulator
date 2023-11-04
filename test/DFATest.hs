module Test.DFATest where

import Test.Hspec ( Spec, describe, it, shouldBe, hspec )
import Data.Set.Monad ( Set, fromList, empty )

import Src.Util.Synonym ( State, Symbol )
import Src.Util.Kleene ( kleeneStar )
import Src.DFA.DFA ( DFA (..), enumerateAcceptedWords ) 

run :: IO ()
run = hspec $ do
    test01
    test02
    test03

test01 :: Spec
test01 = describe "test01, accepting words ending with \"bb\"" $ do it "standard" $ acceptedWords `shouldBe` expected
    where
        stateSet :: Set State = fromList [0, 1, 2]
        alphabet :: Set Symbol = fromList ['a', 'b']
        transitionFunction :: (State, Symbol) -> State
        transitionFunction (0, 'a') = 0
        transitionFunction (0, 'b') = 1
        transitionFunction (1, 'a') = 0
        transitionFunction (1, 'b') = 2
        transitionFunction (2, 'a') = 0
        transitionFunction (2, 'b') = 2
        initialState :: State = 0
        acceptingStateSet :: Set State = fromList [2]
        exampleDFA :: DFA = DFA stateSet alphabet transitionFunction initialState acceptingStateSet
        acceptedWords :: Set [Symbol] = enumerateAcceptedWords 3 exampleDFA
        expected :: Set [Symbol] = fromList ["bb", "abb", "bbb"]

test02 :: Spec
test02 = describe "test02, accepting words including \"ab\" as its partial string" $ do it "standard" $ acceptedWords `shouldBe` expected
    where
        stateSet :: Set State = fromList [0, 1, 2]
        alphabet :: Set Symbol = fromList ['a', 'b']
        transitionFunction :: (State, Symbol) -> State
        transitionFunction (0, 'a') = 1
        transitionFunction (0, 'b') = 0
        transitionFunction (1, 'a') = 1
        transitionFunction (1, 'b') = 2
        transitionFunction (2, 'a') = 2
        transitionFunction (2, 'b') = 2
        initialState :: State = 0
        acceptingStateSet :: Set State = fromList [2]
        exampleDFA :: DFA = DFA stateSet alphabet transitionFunction initialState acceptingStateSet
        acceptedWords :: Set [Symbol] = enumerateAcceptedWords 3 exampleDFA
        expected :: Set [Symbol] = fromList ["ab", "aab", "aba", "bab", "abb"]

test03 :: Spec
test03 = describe "test03, accepting words with even numbers of \"a\" and \"b\"" $ do it "standard" $ acceptedWords `shouldBe` expected
    where
        stateSet :: Set State = fromList [0, 1, 2, 3]
        alphabet :: Set Symbol = fromList ['a', 'b']
        transitionFunction :: (State, Symbol) -> State
        transitionFunction (0, 'a') = 2
        transitionFunction (0, 'b') = 1
        transitionFunction (1, 'a') = 3
        transitionFunction (1, 'b') = 0
        transitionFunction (2, 'a') = 0
        transitionFunction (2, 'b') = 3
        transitionFunction (3, 'a') = 1
        transitionFunction (3, 'b') = 2
        initialState :: State = 0
        acceptingStateSet :: Set State = fromList [0]
        exampleDFA :: DFA = DFA stateSet alphabet transitionFunction initialState acceptingStateSet
        acceptedWords :: Set [Symbol] = enumerateAcceptedWords 4 exampleDFA
        expected :: Set [Symbol] = fromList ["", "aa", "bb", "aaaa", "bbbb", "aabb", "abab", "abba", "baba", "bbaa", "baab"]