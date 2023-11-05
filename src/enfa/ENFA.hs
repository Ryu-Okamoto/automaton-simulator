module Src.ENFA.ENFA where

import Data.Set.Monad as S ( Set, empty, filter, intersection )

import Src.Util.Synonym ( Symbol, State )
import Src.Util.Kleene ( kleeneStar )

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

