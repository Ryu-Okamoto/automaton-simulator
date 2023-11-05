module Src.ENFA.ENFA where

import Data.Set.Monad as S ( Set, fromList, toList, empty, filter, insert, intersection )

import Src.Util.Synonym ( Symbol, State )
import Src.Util.Kleene ( kleeneStar )
import qualified Src.NFA.NFA as NFA ( NFA (..), judge )

data ENFA = ENFA {
        getStateSet           :: Set State,
        getAlphabet           :: Set Symbol,
        getTransitionFunction :: (
                                    (State, Symbol) -> Set State, 
                                              State -> Set State  -- イプシロン遷移
                                 ),
        getInitialState       :: State,
        getAcceptingStateSet  :: Set State
    }

epsilonClosure :: ENFA -> State -> Set State
epsilonClosure enfa q = insert q $ epsilonClosure' q
    where
        (_, epsilonTransition) = getTransitionFunction enfa
        epsilonClosure' :: State -> Set State
        epsilonClosure' q = do
            p <- epsilonTransition q
            insert p $ epsilonClosure' p

convertToNFA :: ENFA -> NFA.NFA
convertToNFA enfa = NFA.NFA stateSet alphabet transitionFunction initialState acceptingStateSet
    where
        stateSet = getStateSet enfa
        alphabet = getAlphabet enfa
        (e_transition, _) = getTransitionFunction enfa
        transitionFunction :: (State, Symbol) -> Set State
        transitionFunction (q, a) = do
                                        p <- epsilonClosure enfa q
                                        e_transition (p, a)
        initialState = getInitialState enfa
        e_acceptingStateSet = getAcceptingStateSet enfa
        acceptingStateSet = fromList [q | q <- toList stateSet, epsilonClosure enfa q `intersection` e_acceptingStateSet /= empty]

judge :: ENFA -> [Symbol] -> Bool
judge enfa = NFA.judge $ convertToNFA enfa

enumerateAcceptedWords :: Int -> ENFA -> Set [Symbol]
enumerateAcceptedWords maxWordLength enfa = S.filter (judge enfa) candidateWords
    where
        candidateWords = kleeneStar maxWordLength $ getAlphabet enfa