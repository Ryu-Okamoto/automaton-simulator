module Src.NFA.NFAExample where

import Data.Set.Monad ( Set, fromList, empty )

import Src.Util.Synonym ( State, Symbol )
import Src.NFA.NFA ( NFA (..), enumerateAcceptedWords ) 

{-
    非決定性有限オートマトン ( NFA ) の定義の例
    末尾が "ab" となるような {a, b} からなる語を認識する
-}

stateSet :: Set State
stateSet = fromList [0, 1, 2]

alphabet :: Set Symbol
alphabet = fromList ['a', 'b']

transitionFunction :: (State, Symbol) -> Set State
transitionFunction (0, 'a') = fromList [0, 1]
transitionFunction (0, 'b') = fromList [0]
transitionFunction (1, 'b') = fromList [2]
transitionFunction        _ = empty

initialState :: State
initialState = 0

acceptingStateSet :: Set State
acceptingStateSet = fromList [2]

exampleNFA :: NFA
exampleNFA = NFA stateSet alphabet transitionFunction initialState acceptingStateSet

acceptedWords :: Set [Symbol]
acceptedWords = enumerateAcceptedWords 5 exampleNFA