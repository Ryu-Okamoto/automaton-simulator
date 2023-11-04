module Src.DFA.DFAExample where

import Data.Set.Monad ( Set, fromList, empty )

import Src.Util.Synonym ( State, Symbol )
import Src.DFA.DFA ( DFA (..), enumerateAcceptedWords ) 

{-
    決定性有限オートマトン ( DFA ) の定義の例
    末尾が "bb" となるような {a, b} からなる語を認識する
-}

stateSet :: Set State
stateSet = fromList [0, 1, 2]

alphabet :: Set Symbol
alphabet = fromList ['a', 'b']

transitionFunction :: (State, Symbol) -> State
transitionFunction (0, 'a') = 0
transitionFunction (0, 'b') = 1
transitionFunction (1, 'a') = 0
transitionFunction (1, 'b') = 2
transitionFunction (2, 'a') = 0
transitionFunction (2, 'b') = 2

initialState :: State
initialState = 0

acceptingStateSet :: Set State
acceptingStateSet = fromList [2]

exampleDFA :: DFA
exampleDFA = DFA stateSet alphabet transitionFunction initialState acceptingStateSet

acceptedWords :: Set [Symbol]
acceptedWords = enumerateAcceptedWords 5 exampleDFA