module Src.ENFA.ENFAExample where

import Data.Char ( isNumber )
import Data.Set.Monad ( Set, fromList, empty )

import Src.Util.Synonym ( State, Symbol )
import Src.ENFA.ENFA ( ENFA (..), enumerateAcceptedWords ) 

{-
    epsilon 動作を含む非決定性有限オートマトン ( ENFA ) の定義の例
    小数点を含む10進数を受理する
     e.g) o: 100.0, -1.414, 0.99, .108, +99.
          x: 100, -100, ., 10+10

    注意：epsilon 遷移には自身の状態を含んではいけない（無限再帰に陥る）
-}

stateSet :: Set State
stateSet = fromList [0, 1, 2, 3, 4, 5]

alphabet :: Set Symbol
alphabet = fromList ['+', '-', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']

transitionFunction :: (State, Symbol) -> Set State
transitionFunction (0, a)
    | a `elem` ['+', '-'] = fromList [1]
transitionFunction (1, a)
    | isNumber a = fromList [1, 4]
    |   a == '.' = fromList [2]
transitionFunction (2, a)
    | isNumber a = fromList [3]
transitionFunction (3, a)
    | isNumber a = fromList [3]
transitionFunction (4, a)
    |   a == '.' = fromList [3]
transitionFunction _ = empty

epsilonTransition :: State -> Set State
epsilonTransition 0 = fromList [1]
epsilonTransition 3 = fromList [5]
epsilonTransition _ = empty

initialState :: State
initialState = 0

acceptingStateSet :: Set State
acceptingStateSet = fromList [5]

exampleENFA :: ENFA
exampleENFA = ENFA stateSet alphabet (transitionFunction, epsilonTransition) initialState acceptingStateSet

acceptedWords :: Set [Symbol]
acceptedWords = enumerateAcceptedWords 3 exampleENFA