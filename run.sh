#!/bin/bash

cabal sandbox init
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
cabal install -j
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
sudo cp .cabal-sandbox/bin/Riemann .
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
./Riemann
