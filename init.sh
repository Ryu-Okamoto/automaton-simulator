#!/bin/sh
cabal install --lib hspec --package-env .
cabal install --lib set-monad --package-env .