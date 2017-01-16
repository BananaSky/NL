#!/bin/bash

cabal sandbox init
cabal install -j
sudo cp .cabal-sandbox/bin/Riemann .
./Riemann
