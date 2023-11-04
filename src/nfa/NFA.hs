module Src.NFA.NFA where

import Data.Set.Monad as S ( Set, empty, filter, intersection )

import Src.Util.Synonym ( Symbol, State )
import Src.Util.Kleene ( kleeneStar )

data NFA = NFA {
        getStateSet           :: Set State,
        getAlphabet           :: Set Symbol,
        getTransitionFunction :: (State, Symbol) -> Set State,
        getInitialState       :: State,
        getAcceptingStateSet  :: Set State
    }

extendTransitionFunction :: ((State, Symbol) -> Set State) -> ((State, [Symbol]) -> Set State)
extendTransitionFunction f = extendedTransitionFunction
    where
        extendedTransitionFunction :: (State, [Symbol]) -> Set State
        extendedTransitionFunction (q,  []) = return q
        extendedTransitionFunction (q, a:x) = do
                                                p <- f (q, a)
                                                extendedTransitionFunction (p, x)

judge :: NFA -> [Symbol] -> Bool
judge nfa word = transitionFuntion (initialState, word) `intersection` acceptionStateSet /= empty
    where
        initialState      = getInitialState nfa
        transitionFuntion = extendTransitionFunction $ getTransitionFunction nfa
        acceptionStateSet = getAcceptingStateSet nfa

enumerateAcceptedWords :: Int -> NFA -> Set [Symbol]
enumerateAcceptedWords maxWordLength nfa = S.filter (judge nfa) candidateWords
    where
        candidateWords = kleeneStar maxWordLength $ getAlphabet nfa