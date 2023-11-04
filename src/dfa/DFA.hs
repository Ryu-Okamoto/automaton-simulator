module Src.DFA.DFA where

import Data.Set.Monad as S ( Set, filter, member )

import Src.Util.Synonym ( State, Symbol )
import Src.Util.Kleene ( kleeneStar )

data DFA = DFA {
        getStateSet           :: Set State,
        getAlphabet           :: Set Symbol,
        getTransitionFunction :: (State, Symbol) -> State,
        getInitialState       :: State,
        getAcceptingStateSet  :: Set State
    }

extendTransitionFunction :: ((State, Symbol) -> State) -> ((State, [Symbol]) -> State)
extendTransitionFunction f = extendedTransitionFunction
    where
        extendedTransitionFunction :: (State, [Symbol]) -> State
        extendedTransitionFunction (q,  []) = q
        extendedTransitionFunction (q, a:x) = extendedTransitionFunction (f (q, a), x)

judge :: DFA -> [Symbol] -> Bool
judge dfa word = transitionFuntion (initialState, word) `member` acceptionStateSet
    where
        initialState      = getInitialState dfa
        transitionFuntion = extendTransitionFunction $ getTransitionFunction dfa
        acceptionStateSet = getAcceptingStateSet dfa

enumerateAcceptedWords :: Int -> DFA -> Set [Symbol]
enumerateAcceptedWords maxWordLength dfa = S.filter (judge dfa) candidateWords
    where
        candidateWords = kleeneStar maxWordLength $ getAlphabet dfa