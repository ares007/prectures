#!/usr/bin/env bash

ghc --make shake.hs -threaded -rtsopts "-with-rtsopts=-I0 -qg -qb"
rm *.o
rm *.hi
