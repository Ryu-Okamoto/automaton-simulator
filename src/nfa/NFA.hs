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

judge :: NFA -> [Symbol] -> Bool
judge nfa word = extendedTransitionFunction (initialState, word) `intersection` acceptingStateSet /= empty
    where
        initialState      = getInitialState nfa
        acceptingStateSet = getAcceptingStateSet nfa
        transitionFuntion = getTransitionFunction nfa
        extendedTransitionFunction :: (State, [Symbol]) -> Set State
        extendedTransitionFunction (q,  []) = return q
        extendedTransitionFunction (q, a:x) = do
                                                p <- transitionFuntion (q, a)
                                                extendedTransitionFunction (p, x)

enumerateAcceptedWords :: Int -> NFA -> Set [Symbol]
enumerateAcceptedWords maxWordLength nfa = S.filter (judge nfa) candidateWords
    where
        candidateWords = kleeneStar maxWordLength $ getAlphabet nfa